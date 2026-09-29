"""
scraper.py — Scraping automatitzat de la FCF (NOVA WEB, Next.js)
===================================================================

IMPORTANT — llegeix això abans d'executar:

La FCF ha canviat completament la web (fcf.cat) a una aplicació Next.js.
Investigant-la a fons (capturant les peticions de xarxa reals que fa el
navegador) hem trobat que TOT el que necessitem és accessible amb simples
peticions HTTP (`requests`) — NO cal cap navegador (Playwright) enlloc:

1. El calendari de partits d'un grup (jugats o no, amb resultat i ID
   d'acta) s'obté cridant l'API interna:
       GET https://www.fcf.cat/api/competition/partidos?grupId={grupId}
   Aquesta és la mateixa crida que fa el navegador en carregar la pàgina
   de "fitxa de competició" — i només se'n fa UNA per grup: la pestanya
   "JORNADA" no torna a demanar res al servidor, només filtra en local
   les dades que ja té totes carregades. Per això la vam trobar mirant el
   registre de peticions de xarxa d'una sessió real amb Playwright, tot
   i que ara ja no calgui el navegador per fer-la servir.

2. La pàgina d'ACTA d'un partit concret
   (https://www.fcf.cat/ca/competicio/acta/{id}) ve generada pel servidor
   (Next.js streaming SSR): es descarrega amb una petició HTTP normal i
   se'n reconstrueix el contingut real (equips, resultat, gols amb minut
   i tipus, alineacions amb targetes/substitucions) — validat contra
   dades reals.

Per tant aquest scraper és 100% `requests` + `BeautifulSoup`, sense cap
navegador: més ràpid, més fiable i sense les dependències pesades de
Playwright/Chromium.

CONFIGURACIÓ QUE HAS D'OMPLIR TU (un cop per temporada, ~15 min):
  El diccionari GRUP_IDS més avall necessita, per a cada categoria i grup,
  el "competicioId" i el "grupId" que apareixen a la URL quan navegues
  fins aquell grup a fcf.cat (Competició → selecciona Temporada/Disciplina/
  Competició/Grup). Exemple (el que tu ja em vas passar):

    https://www.fcf.cat/ca/competicio?temporadaId=22&disciplinaId=19308233
        &competicioId=58161869&grupId=58161876
                                   ^^^^^^^^            ^^^^^^^^
    → TERCERA, grup 3: competicioId=58161869, grupId=58161876

  Aquests identificadors NO es poden deduir per fórmula (no són
  correlatius de manera fiable) — cal agafar-los navegant el lloc web una
  vegada per grup. Fins que no ompliràs tots els grups del diccionari,
  el scraper saltarà (amb avís) els grups que no tinguin ID configurat.

AVÍS SOBRE FIABILITAT:
  No he pogut cridar l'API `partidos` des d'aquí per veure l'estructura
  exacta del JSON de resposta (no tinc accés directe a fcf.cat), així que
  `scrape_calendar_api()`:
    - Sempre desa la resposta crua a `debug_api_partidos_*.json` — si els
      noms de camp no coincideixen amb els que he suposat (català/castellà
      barrejat, típic d'aquesta API), es podrà ajustar de seguida mirant
      aquest fitxer, sense haver de tornar a capturar res amb Playwright.
    - Prova diverses variants de nom de camp i cau a un DataFrame buit
      (sense petar) si no reconeix l'estructura.
  La part de l'ACTA (mòdul 3) SÍ que s'ha provat contra una acta real i
  contra la captura de pantalla que em vas passar (gols, targetes grogues/
  vermelles i substitucions coincideixen exactament).

Ús:
    python scraper.py --categoria TERCERA --grup 3          # un sol grup
    python scraper.py --categoria TERCERA --grup 3 --debug  # + bolcats de depuració
    python scraper.py --categoria TERCERA                   # tota la categoria
    python scraper.py                                       # tot
"""

import re
import os
import sys
import time
import argparse
import warnings
from pathlib import Path

import requests
from bs4 import BeautifulSoup
import pandas as pd
import numpy as np
from supabase import create_client, Client
from generar_prediccions import generar_totes as generar_prediccions

warnings.filterwarnings("ignore")

# ============================================================================
# CONFIGURACIÓ CENTRAL
# ============================================================================

TEMPORADA      = "26_27"
TEMPORADA_ID   = "22"           # 21 = 2025/26 ; 22 = 2026/27 (actual)
DISCIPLINA_ID  = "19308233"     # futbol 11 (fixe, trobat empíricament)
HEADERS        = {
    "User-Agent": "Mozilla/5.0 (Windows NT 10.0; Win64; x64) "
                   "AppleWebKit/537.36 (KHTML, like Gecko) Chrome/124.0 Safari/537.36",
}
SLEEP_BETWEEN_REQUESTS = 1.5    # segons entre peticions d'actes (respecta el servidor)
MAX_RETRIES            = 4
RETRY_BACKOFF          = 20

BASE_URL = "https://www.fcf.cat"

# Nombre de grups per categoria (sense canvis respecte l'any passat, verificat
# que Tercera Grup 2 continua tenint 18 grups/30 jornades a la nova web)
CATEGORIES = {
    "TERCERA": 18,
    "SEGONA":   6,
    "PRIMERA":  3,
}

MAX_JORNADES = {
    "TERCERA": 30,
    "SEGONA":  30,
    "PRIMERA": 30,
}

# ----------------------------------------------------------------------------
# IDs de competicioId/grupId per a cada categoria i grup, temporada 2026/27.
# S'HAN D'OMPLIR MANUALMENT navegant fcf.cat (veure instruccions dalt).
# Format: CATEGORIA -> {num_grup: {"competicioId": "...", "grupId": "..."}}
# ----------------------------------------------------------------------------
GRUP_IDS = {
    "TERCERA": {
        1:  {"competicioId": "58161869", "grupId": "58161874"},
        2:  {"competicioId": "58161869", "grupId": "58161875"},
        3:  {"competicioId": "58161869", "grupId": "58161876"},
        4:  {"competicioId": "58161869", "grupId": "58161872"},
        5:  {"competicioId": "58161869", "grupId": "58161873"},
        6:  {"competicioId": "58161869", "grupId": "58161877"},
        7:  {"competicioId": "58161869", "grupId": "58161878"},
        8:  {"competicioId": "58161869", "grupId": "58161879"},
        9:  {"competicioId": "58161869", "grupId": "58161880"},
        10: {"competicioId": "58161869", "grupId": "58161881"},
        11: {"competicioId": "58161869", "grupId": "58161882"},
        12: {"competicioId": "58161869", "grupId": "58161883"},
        13: {"competicioId": "58161869", "grupId": "58161884"},
        14: {"competicioId": "58161869", "grupId": "58161870"},
        15: {"competicioId": "58161869", "grupId": "58161871"},
        16: {"competicioId": "58161869", "grupId": "58161886"},
        17: {"competicioId": "58161869", "grupId": "58161887"},
        18: {"competicioId": "58161869", "grupId": "58161885"},
    },
    "SEGONA": {
        1: {"competicioId": "58161862", "grupId": "58161863"},
        2: {"competicioId": "58161862", "grupId": "58161864"},
        3: {"competicioId": "58161862", "grupId": "58161865"},
        4: {"competicioId": "58161862", "grupId": "58161866"},
        5: {"competicioId": "58161862", "grupId": "58161867"},
        6: {"competicioId": "58161862", "grupId": "58161868"},
    },
    "PRIMERA": {
        1: {"competicioId": "58161856", "grupId": "58161857"},
        2: {"competicioId": "58161856", "grupId": "58161858"},
        3: {"competicioId": "58161856", "grupId": "58161859"},
    },
}


def get_grup_ids(categoria: str, grup: int):
    """Retorna {"competicioId", "grupId"} per aquest grup, o None si falta configurar."""
    return GRUP_IDS.get(categoria, {}).get(grup)


def competicio_url(competicio_id: str, grup_id: str) -> str:
    return (
        f"{BASE_URL}/ca/competicio?temporadaId={TEMPORADA_ID}"
        f"&disciplinaId={DISCIPLINA_ID}&competicioId={competicio_id}&grupId={grup_id}"
    )


# ============================================================================
# MÒDUL 1 — CALENDARI DE PARTITS D'UN GRUP (API interna, NO cal navegador)
# ============================================================================
#
# DESCOBERT capturant les peticions de xarxa amb Playwright (veure historial):
# quan es carrega la pàgina de competició, el navegador fa una única crida a
#     GET https://www.fcf.cat/api/competition/partidos?grupId={grupId}
# i mai més se'n torna a fer cap altra en canviar de jornada — la pestanya
# "JORNADA" només filtra en local les dades que ja té totes carregades.
# Això vol dir que aquesta única crida ja porta tots els partits de la
# temporada sencera, i podem descarregar-la directament amb `requests`,
# sense necessitat de cap navegador ni de simular cap clic.
#
# ⚠️ No he pogut verificar l'estructura exacta del JSON de resposta (no tinc
# accés a fcf.cat des d'aquí per inspeccionar-la en viu), així que aquesta
# funció:
#   1. Sempre desa la resposta crua a un fitxer de depuració
#      (`debug_api_partidos_{categoria}_grup{grup}.json`) — si el format no
#      coincideix amb el que espero, es podrà ajustar de seguida mirant
#      aquest fitxer, sense haver de tornar a capturar res.
#   2. Prova diverses variants raonables de noms de camp (en català/castellà,
#      que és el que fa servir la resta de l'API: "jornada", "equipLocal",
#      "equipVisitant", "golsLocal", "actaId"...) i cau de nou a un DataFrame
#      buit si no reconeix l'estructura, en lloc de petar.

def scrape_calendar_api(categoria: str, grup: int, debug: bool = False) -> pd.DataFrame:
    """Retorna un DataFrame amb els partits del grup (jugats o no) i, quan hi
    hagi acta disponible, el seu ID — descarregat directament de l'API
    interna de fcf.cat, sense navegador.

    Columnes: jornada, local_team, away_team, date, time, venue, goals_home,
    goals_away, acta_id (None si encara no s'ha jugat).
    """
    empty_cols = ["jornada", "local_team", "away_team", "date", "time",
                  "venue", "goals_home", "goals_away", "acta_id"]
    ids = get_grup_ids(categoria, grup)
    if ids is None:
        print(f"     ⚠️  {categoria} Grup {grup}: falta 'competicioId'/'grupId' a "
              f"GRUP_IDS — omple'l navegant fcf.cat. Saltant.")
        return pd.DataFrame(columns=empty_cols)

    api_url = f"{BASE_URL}/api/competition/partidos?grupId={ids['grupId']}"
    print(f"     🌐 Cridant API: {api_url}")

    try:
        resp = requests.get(api_url, headers=HEADERS, timeout=20)
        resp.raise_for_status()
        data = resp.json()
    except Exception as e:
        print(f"     ❌ Error cridant l'API de partits: {e}")
        return pd.DataFrame(columns=empty_cols)

    # Desem sempre la resposta crua per poder-la inspeccionar si cal ajustar
    # el mapeig de camps de sota.
    try:
        import json
        debug_path = Path(f"debug_api_partidos_{categoria}_grup{grup}.json")
        debug_path.write_text(json.dumps(data, indent=2, ensure_ascii=False), encoding="utf-8")
        print(f"     🐛 Resposta crua de l'API desada a {debug_path}")
    except Exception:
        pass

    # ESTRUCTURA REAL confirmada (gràcies al bolcat de depuració d'una
    # execució real): la resposta és un diccionari on cada clau és el
    # número de jornada ("1".."30") i el valor la llista de partits
    # d'aquella jornada, amb camps en castellà/majúscules:
    #   JORNADA, CODACTA, NOMBRE_CASA, NOMBRE_FUERA, CAMPO,
    #   GOLES_CASA, GOLES_FUERA, COMIENZO1 ("YYYY-MM-DD HH:MM:SS"),
    #   CERRADA ("1" si l'acta ja està tancada i es pot consultar, "0" si
    #   no — CODACTA hi és SEMPRE, fins i tot per a partits encara no
    #   jugats, així que NOMÉS considerem l'acta disponible quan
    #   CERRADA == "1".
    if not isinstance(data, dict):
        print("     ⚠️  No he reconegut l'estructura de la resposta de l'API "
              "(mira el fitxer debug_api_partidos_*.json) — cap partit carregat.")
        return pd.DataFrame(columns=empty_cols)

    partits_raw = []
    for jornada_key, partits_jornada in data.items():
        if isinstance(partits_jornada, list):
            partits_raw.extend(partits_jornada)

    if not partits_raw:
        print("     ⚠️  L'API no ha retornat cap partit (mira el fitxer "
              "debug_api_partidos_*.json) — cap partit carregat.")
        return pd.DataFrame(columns=empty_cols)

    def _to_int(v):
        try:
            return int(v)
        except (TypeError, ValueError):
            return None

    rows = []
    acta_ids = set()
    for p in partits_raw:
        if not isinstance(p, dict):
            continue

        jornada = _to_int(p.get("JORNADA"))
        home = p.get("NOMBRE_CASA")
        away = p.get("NOMBRE_FUERA")
        venue = p.get("CAMPO")
        goals_home = _to_int(p.get("GOLES_CASA"))
        goals_away = _to_int(p.get("GOLES_FUERA"))

        # COMIENZO1 ve com "YYYY-MM-DD HH:MM:SS" — el separem en data/hora.
        date = time_ = None
        comienzo = p.get("COMIENZO1")
        if comienzo:
            parts = str(comienzo).split(" ")
            date = parts[0] if len(parts) > 0 else None
            time_ = parts[1][:5] if len(parts) > 1 else None

        # CODACTA hi és sempre (fins i tot per a partits encara no jugats);
        # només el considerem "acta disponible" quan CERRADA == "1".
        acta_id = None
        if str(p.get("CERRADA")) == "1":
            acta_id = _to_int(p.get("CODACTA"))
            if acta_id is not None:
                acta_ids.add(acta_id)

        rows.append({
            "jornada": jornada,
            "local_team": home,
            "away_team": away,
            "date": date,
            "time": time_,
            "venue": venue,
            "goals_home": goals_home,
            "goals_away": goals_away,
            "acta_id": acta_id,
        })

    df = pd.DataFrame(rows) if rows else pd.DataFrame(columns=empty_cols)
    print(f"     ✅ {len(df)} partits llegits de l'API ({len(acta_ids)} amb acta)")
    return df


# ============================================================================
# MÒDUL 2 — CLASSIFICACIÓ ACUMULADA (standings_by_round.csv)
# ============================================================================

STANDINGS_COLUMNS = [
    "team", "played", "wins", "draws", "losses", "goals_for",
    "goals_against", "points", "goal_diff", "position", "jornada",
]


def compute_standings_by_round(matches: pd.DataFrame) -> pd.DataFrame:
    """Calcula la classificació acumulada jornada a jornada."""
    if matches is None or matches.empty or "goals_home" not in matches.columns:
        return pd.DataFrame(columns=STANDINGS_COLUMNS)

    played = matches.dropna(subset=["goals_home", "goals_away"]).copy()
    if played.empty:
        return pd.DataFrame(columns=STANDINGS_COLUMNS)
    played["goals_home"] = played["goals_home"].astype(int)
    played["goals_away"] = played["goals_away"].astype(int)

    jornades = sorted(played["jornada"].unique())
    standings_all = []

    for j in jornades:
        df = played[played["jornada"] <= j]
        teams = pd.unique(df[["local_team", "away_team"]].values.ravel())
        table = pd.DataFrame({"team": teams}).set_index("team")
        for col in ["played", "wins", "draws", "losses", "goals_for", "goals_against", "points"]:
            table[col] = 0

        for _, row in df.iterrows():
            h, a = row["local_team"], row["away_team"]
            gh, ga = row["goals_home"], row["goals_away"]
            table.loc[h, "played"] += 1
            table.loc[a, "played"] += 1
            table.loc[h, "goals_for"]     += gh
            table.loc[h, "goals_against"] += ga
            table.loc[a, "goals_for"]     += ga
            table.loc[a, "goals_against"] += gh
            if gh > ga:
                table.loc[h, ["wins",   "points"]] += [1, 3]
                table.loc[a, "losses"] += 1
            elif gh < ga:
                table.loc[a, ["wins",   "points"]] += [1, 3]
                table.loc[h, "losses"] += 1
            else:
                table.loc[h, ["draws", "points"]] += [1, 1]
                table.loc[a, ["draws", "points"]] += [1, 1]

        table["goal_diff"] = table["goals_for"] - table["goals_against"]
        table = table.sort_values(["points", "goal_diff", "goals_for"], ascending=False)
        table["position"] = range(1, len(table) + 1)
        table.reset_index(inplace=True)
        table["jornada"] = j
        standings_all.append(table)

    return pd.concat(standings_all, ignore_index=True) if standings_all else pd.DataFrame(columns=STANDINGS_COLUMNS)


# ============================================================================
# MÒDUL 3 — ACTA D'UN PARTIT (requests, NO cal navegador)
# ============================================================================
#
# ⚠️ Aquest mòdul ha estat REESCRIT i VALIDAT contra l'HTML real d'una acta
# (gràcies al "view-source" que em vas passar). Descobriment clau: la pàgina
# fa servir el "streaming SSR" de React/Next — el HTML inicial conté
# <template id="P:X"></template> com a marcadors buits, i el contingut real
# que hi ha d'anar apareix més avall al document dins de
# <div hidden id="S:Y">...contingut...</div> seguit d'un
# <script>$RS("S:Y","P:X")</script> que li diu al NAVEGADOR que mogui aquell
# contingut cap al marcador. Com que nosaltres no executem JavaScript,
# `resolve_next_streaming()` fa aquesta reconstrucció manualment abans de
# parsejar res. Un cop resolt, la pàgina té tot el DOM real amb classes CSS
# consistents, i d'aquí traiem:
#   - Equips, resultat, jornada/categoria/grup, data/hora (capçalera)
#   - Gols (secció "Gols": jugador, minut, tipus, equip)
#   - Alineacions (seccions "Alineacions"/"Suplents"): per a cada jugador,
#     identifiquem les insígnies de targeta groga/vermella (per color de
#     fons: #FFEB3B / #F30000) i de substitució ("entra"/"surt", per la
#     direcció i color de la fletxa SVG), i calculem minuts jugats, gols i
#     targetes directament — validat contra l'acta que em vas enviar,
#     incloent-hi la doble targeta groga i les substitucions múltiples.
#
# Nota: la pàgina renderitza el mateix contingut dues vegades (una versió
# per mòbil i una per escriptori, amistoses amb Tailwind responsive), així
# que sempre desduplicom agafant només les dues primeres seccions de cada
# tipus (equip local i visitant) i descartant files de jugador repetides.

JERSEY_ICON_HINT = "M631.2 96.5"  # tros identificatiu del path SVG de la samarreta (marca fila de jugador, no d'staff tècnic)


def resolve_next_streaming(html: str) -> str:
    """Substitueix els marcadors <template id="P:X"></template> pel contingut
    real que el navegador hi mouria en execució (streaming SSR de Next.js)."""
    soup = BeautifulSoup(html, "lxml")
    s_divs = {}
    for div in soup.find_all("div", attrs={"hidden": True}):
        did = div.get("id", "")
        if did.startswith("S:"):
            s_divs[did.split(":", 1)[1]] = div.decode_contents()

    rs_calls = re.findall(r'\$RS\("S:([^"]+)","P:([^"]+)"\)', html)
    p_to_s = {p: s for s, p in rs_calls}

    working = html
    for _ in range(15):
        changed = False
        for p_id, s_id in p_to_s.items():
            token = f'<template id="P:{p_id}"></template>'
            if token in working and s_id in s_divs:
                working = working.replace(token, s_divs[s_id])
                changed = True
        if not changed:
            break
    return working


def _clean_dom(html: str):
    soup = BeautifulSoup(html, "lxml")
    for tag in soup.find_all(["script", "template"]):
        tag.decompose()
    return soup


def _is_player_row(row_div) -> bool:
    """Distingeix una fila de jugador (icona de samarreta) d'una fila
    d'staff tècnic (insígnia rodona amb inicial)."""
    return JERSEY_ICON_HINT in str(row_div)


def _classify_badge(container) -> str | None:
    """Classifica la insígnia (minut + icona) d'una fila de jugador."""
    html_str = str(container)
    if "#00FF73" in html_str:
        return "gol"
    if "#FFEB3B" in html_str:
        return "groga"
    if "bg-[#F30000]" in html_str and "<path" not in html_str:
        return "vermella"
    if 'd="M20 12H4' in html_str:
        return "entra"
    if 'd="M4 12H20' in html_str:
        return "surt"
    return None


def _parse_lineup_section(header_h3, team_name: str, position_label: str):
    """header_h3: tag <h3> amb text 'Alineacions' o 'Suplents'."""
    container = header_h3.find_parent("div").find_next_sibling("div")
    if container is None:
        return []
    out = []
    for row in container.find_all("div", recursive=False):
        if not _is_player_row(row):
            continue
        name_span = row.find("span", class_="truncate")
        if not name_span:
            continue
        name = name_span.get_text(strip=True)
        num_span = row.select_one("span.absolute.inset-0")
        shirt = num_span.get_text(strip=True) if num_span else None

        badges_roots = row.find_all("div", class_="flex items-center gap-3 shrink-0 ml-2")
        raw_events = []
        if badges_roots:
            for pair in badges_roots[-1].find_all("div", recursive=False):
                minute_span = pair.find("span")
                minute_txt = re.sub(r"[^\d]", "", minute_span.get_text(strip=True)) if minute_span else None
                kind = _classify_badge(pair)
                if kind:
                    raw_events.append((int(minute_txt) if minute_txt else None, kind))
        out.append({
            "team": team_name, "player": name, "shirt_number": shirt,
            "position": position_label, "raw_events": raw_events,
        })
    return out


def _extract_score(soup) -> tuple:
    spans = soup.select("span.text-3xl.sm\\:text-4xl.md\\:text-6xl.font-bold")
    nums = [s.get_text(strip=True) for s in spans if s.get_text(strip=True).isdigit()]
    if len(nums) >= 2:
        return int(nums[0]), int(nums[1])
    return None, None


def _extract_gols(soup, home_team, away_team, jornada, date):
    h3 = soup.find("h3", string="Gols")
    if not h3:
        return []
    outer = h3.find_parent("div")
    if outer is None:
        return []
    outer2 = outer.find_parent("div")
    container = outer2.find("div", class_="flex flex-col") if outer2 else None
    if not container:
        return []
    events = []
    for row in container.find_all("div", recursive=False):
        name_span = row.select_one("span.line-clamp-2")
        detail_span = row.select_one("span.text-gray-400")
        if not name_span or not detail_span:
            continue
        full_name_txt = name_span.get_text(" ", strip=True)
        m = re.match(r"(.+?)\s*\((\d+)['’]\)", full_name_txt)
        if not m:
            continue
        player, minut = m.group(1).strip(), int(m.group(2))
        detail_txt = detail_span.get_text(" ", strip=True)
        m2 = re.match(r"GOL\s+(NORMAL|PENAL|EN PR[OÒ]PIA)\s*\(\s*(.+?)\s*\)", detail_txt, re.IGNORECASE)
        tipus, equip = (m2.group(1), m2.group(2)) if m2 else (None, None)
        tipus_norm = {"NORMAL": "Normal", "PENAL": "Penal"}.get((tipus or "").upper(), "Pròpia")
        events.append({
            "match_date": date, "jornada": jornada, "home_team": home_team, "away_team": away_team,
            "event_type": "Gol", "minute": minut, "team": equip, "player": player, "detail": tipus_norm,
        })
    return events


BREADCRUMB_RE = re.compile(r"Competici[oó]\s*/\s*([^/]+)\s*/\s*GRUP\s*(\d+)\s*/\s*Jornada\s*(\d+)", re.IGNORECASE)
DATA_RE       = re.compile(r"Data:\s*([\d.]+)")
HORA_RE       = re.compile(r"Hora:\s*([\d.]+)H", re.IGNORECASE)
ESTADI_RE     = re.compile(r"Estadi:\s*(.+)")


def get_with_retry(url: str) -> str | None:
    """Petició HTTP amb reintents i backoff exponencial."""
    for attempt in range(1, MAX_RETRIES + 1):
        try:
            resp = requests.get(url, headers=HEADERS, timeout=20)
            if resp.status_code == 503:
                wait = RETRY_BACKOFF * (2 ** (attempt - 1))
                print(f"    ⏳ 503 rebut (intent {attempt}/{MAX_RETRIES}), esperant {wait}s...")
                time.sleep(wait)
                continue
            resp.raise_for_status()
            return resp.text
        except requests.exceptions.HTTPError as e:
            if attempt < MAX_RETRIES:
                wait = RETRY_BACKOFF * (2 ** (attempt - 1))
                print(f"    ⏳ Error HTTP (intent {attempt}/{MAX_RETRIES}), esperant {wait}s...")
                time.sleep(wait)
            else:
                print(f"    ❌ Error GET {url}: {e}")
                return None
        except Exception as e:
            print(f"    ❌ Error GET {url}: {e}")
            return None
    return None


def scrape_match_acta(acta_id: int, categoria: str, grup: int):
    """Descarrega i interpreta l'acta d'un partit (per requests, sense navegador).

    Retorna (match_info: dict | None, events: list[dict], lineups: list[dict], ok: bool).
    `lineups` ja porta minuts jugats / gols / targetes calculats directament
    (no cal creuar-ho amb events com a l'antic scraper — la nova web dona el
    minut de cada esdeveniment directament a la fila del jugador).
    """
    url = f"{BASE_URL}/ca/competicio/acta/{acta_id}"
    html = get_with_retry(url)
    if html is None:
        return None, [], [], False

    try:
        resolved = resolve_next_streaming(html)
        soup = _clean_dom(resolved)
        text = soup.get_text("\n", strip=True)
    except Exception as e:
        print(f"    ❌ Error parsejant acta {acta_id}: {e}")
        return None, [], [], False

    # Equips: primers dos enllaços únics a /ca/clubs/{id}/categories/{id}
    seen = {}
    for a in soup.find_all("a", href=re.compile(r"/ca/clubs/\d+/categories/\d+")):
        t = a.get_text(strip=True)
        if t and a["href"] not in seen:
            seen[a["href"]] = t
    clubs = list(seen.values())
    if len(clubs) < 2:
        return None, [], [], False
    home_team, away_team = clubs[0], clubs[1]

    match_info = {"season": "2026-2027", "home_team": home_team, "away_team": away_team, "jornada": None}

    m_bc = BREADCRUMB_RE.search(text)
    if m_bc:
        match_info["competition"] = m_bc.group(1).strip()
        match_info["jornada"] = int(m_bc.group(3))

    m_data = DATA_RE.search(text)
    if m_data:
        match_info["date"] = m_data.group(1)
    m_hora = HORA_RE.search(text)
    if m_hora:
        match_info["time"] = m_hora.group(1)
    m_estadi = ESTADI_RE.search(text)
    if m_estadi:
        match_info["venue"] = m_estadi.group(1).split("\n")[0].strip()

    gh, ga = _extract_score(soup)
    if gh is not None:
        match_info["goals_home"], match_info["goals_away"] = gh, ga

    # --- GOLS (estructural, per evitar duplicats mòbil/escriptori) ---
    events = _extract_gols(soup, home_team, away_team, match_info.get("jornada"), match_info.get("date"))

    # --- ALINEACIONS ---
    alineacions_h = soup.find_all("h3", string="Alineacions")
    suplents_h = soup.find_all("h3", string="Suplents")
    raw_players = []
    for i, h in enumerate(alineacions_h[:2]):
        team = home_team if i == 0 else away_team
        raw_players.extend(_parse_lineup_section(h, team, "Titular"))
    for i, h in enumerate(suplents_h[:2]):
        team = home_team if i == 0 else away_team
        raw_players.extend(_parse_lineup_section(h, team, "Suplent"))

    lineups = []
    seen_players = set()
    for p in raw_players:
        key = (p["team"], p["player"], p["position"])
        if key in seen_players:
            continue
        seen_players.add(key)

        goals      = sum(1 for _, k in p["raw_events"] if k == "gol")
        n_groga    = sum(1 for _, k in p["raw_events"] if k == "groga")
        n_vermella = sum(1 for _, k in p["raw_events"] if k == "vermella")
        surt_mins  = [m for m, k in p["raw_events"] if k == "surt"]
        entra_mins = [m for m, k in p["raw_events"] if k == "entra"]

        if p["position"] == "Titular":
            minutes_played = surt_mins[0] if surt_mins else 90
        else:
            minutes_played = (90 - entra_mins[0]) if entra_mins else 0

        yellow_cards = 1 if n_groga >= 1 else 0
        red_cards = 1 if (n_vermella >= 1 or n_groga >= 2) else 0

        lineups.append({
            "match_date": match_info.get("date"),
            "jornada":    match_info.get("jornada"),
            "home_team":  home_team,
            "away_team":  away_team,
            "team":       p["team"],
            "player":     p["player"],
            "shirt_number": p["shirt_number"],
            "position":   p["position"],
            "minutes_played": minutes_played,
            "goals":      goals,
            "yellow_cards": yellow_cards,
            "red_cards":  red_cards,
        })

    ok = bool(home_team and away_team)
    return (match_info if ok else None), events, lineups, ok


# ============================================================================
# MÒDUL 4 — ESTADÍSTIQUES DE JUGADORS I EQUIPS  (sense canvis respecte l'anterior)
# ============================================================================

PLAYER_MATCH_STATS_COLUMNS = [
    "match_id", "jornada", "match_date", "player", "team",
    "starter", "minutes_played", "goals", "yellow_cards", "red_cards",
]

EVENTS_EMPTY_COLUMNS = [
    "match_date", "jornada", "home_team", "away_team",
    "event_type", "minute", "team", "player", "detail", "match_id",
]


def build_player_match_stats(lineups: pd.DataFrame, events: pd.DataFrame) -> pd.DataFrame:
    """Construeix player_match_stats a partir de lineups.

    A diferència de l'scraper antic, la nova acta dona el minut de cada
    esdeveniment (targeta/substitució/gol) directament a la fila del propi
    jugador, així que `lineups` ja arriba amb `minutes_played`, `goals`,
    `yellow_cards` i `red_cards` calculats al mòdul 3 — aquí només cal
    donar-los format i afegir `match_id`. `events` es manté com a paràmetre
    per compatibilitat però ja no cal creuar-hi dades.

    Guarda: si `lineups` és buit (inici de temporada sense partits jugats),
    retornem un DataFrame buit amb l'esquema correcte.
    """
    required_cols = {"jornada", "home_team", "away_team", "player", "team", "position"}
    if lineups is None or lineups.empty or not required_cols.issubset(lineups.columns):
        return pd.DataFrame(columns=PLAYER_MATCH_STATS_COLUMNS)

    lineups = lineups.copy()
    lineups["match_id"] = (
        lineups["jornada"].astype(str) + "_"
        + lineups["home_team"].str[:3] + "_"
        + lineups["away_team"].str[:3]
    )
    lineups["starter"] = (lineups["position"] == "Titular").astype(int)

    for col in ["minutes_played", "goals", "yellow_cards", "red_cards"]:
        if col not in lineups.columns:
            lineups[col] = 0

    df = lineups.rename(columns={})[
        ["match_id", "jornada", "match_date", "player", "team",
         "starter", "minutes_played", "goals", "yellow_cards", "red_cards"]
    ].copy()

    if not df.empty:
        df.sort_values(["jornada", "team", "starter"], ascending=[True, True, False], inplace=True)
    else:
        df = pd.DataFrame(columns=PLAYER_MATCH_STATS_COLUMNS)
    return df


PLAYER_STATS_COLUMNS = [
    "player", "team", "matches_played", "starts", "total_minutes",
    "goals", "goals_per_90", "cards_per_90",
]


def build_player_stats(player_match_stats: pd.DataFrame) -> pd.DataFrame:
    """Estadístiques agregades per jugador."""
    if (player_match_stats is None or player_match_stats.empty
            or "player" not in player_match_stats.columns):
        return pd.DataFrame(columns=PLAYER_STATS_COLUMNS)

    agg = player_match_stats.groupby(["player", "team"]).agg(
        matches_played=("match_id", "count"),
        starts=("starter", "sum"),
        total_minutes=("minutes_played", "sum"),
        goals=("goals", "sum"),
        total_yellow_cards=("yellow_cards", "sum"),
        total_red_cards=("red_cards", "sum"),
    ).reset_index()

    agg["goals_per_90"] = (
        agg["goals"] / agg["total_minutes"].replace(0, np.nan) * 90
    ).fillna(0).round(2)

    agg["cards_per_90"] = (
        (agg["total_yellow_cards"] + agg["total_red_cards"])
        / agg["total_minutes"].replace(0, np.nan) * 90
    ).fillna(0).round(2)

    return agg[PLAYER_STATS_COLUMNS].sort_values("goals", ascending=False)


TEAM_MATCH_STATS_COLUMNS = [
    "team", "match_id", "jornada", "match_date", "opponent", "home_away",
    "goals_for", "goals_against", "yellow_cards", "red_cards",
]


def build_team_match_stats(matches_info: pd.DataFrame,
                           player_match_stats: pd.DataFrame) -> pd.DataFrame:
    """Estadístiques per equip i partit."""
    if (matches_info is None or matches_info.empty
            or "jornada" not in matches_info.columns):
        return pd.DataFrame(columns=TEAM_MATCH_STATS_COLUMNS)

    matches_info = matches_info.copy()
    matches_info["match_id"] = (
        matches_info["jornada"].astype(str) + "_"
        + matches_info["home_team"].str[:3] + "_"
        + matches_info["away_team"].str[:3]
    )

    if (player_match_stats is None or player_match_stats.empty
            or "match_id" not in player_match_stats.columns):
        player_match_stats = pd.DataFrame(
            columns=["match_id", "team", "yellow_cards", "red_cards"]
        )

    records = []
    for _, m in matches_info.iterrows():
        mid = m["match_id"]
        for side, team, opp, gf_col, ga_col in [
            ("Home", m["home_team"], m["away_team"], "goals_home", "goals_away"),
            ("Away", m["away_team"], m["home_team"], "goals_away", "goals_home"),
        ]:
            pms = player_match_stats[
                (player_match_stats["match_id"] == mid) &
                (player_match_stats["team"] == team)
            ]
            records.append({
                "team":         team,
                "match_id":     mid,
                "jornada":      m["jornada"],
                "match_date":   m.get("date"),
                "opponent":     opp,
                "home_away":    side,
                "goals_for":    int(m[gf_col]) if pd.notna(m.get(gf_col)) else 0,
                "goals_against":int(m[ga_col]) if pd.notna(m.get(ga_col)) else 0,
                "yellow_cards": int(pms["yellow_cards"].sum()),
                "red_cards":    int(pms["red_cards"].sum()),
            })

    df = pd.DataFrame(records)
    if not df.empty:
        df.sort_values(["jornada", "team"], inplace=True)
    else:
        df = pd.DataFrame(columns=TEAM_MATCH_STATS_COLUMNS)
    return df


# ============================================================================
# MÒDUL 5 — UPLOAD A SUPABASE  (sense canvis respecte l'anterior)
# ============================================================================

CSV_TABLE_MAP = {
    "matches.csv":            "matches",
    "all_matches_info.csv":   "matches_info",
    "all_matches_events.csv": "matches_events",
    "all_matches_lineups.csv":"matches_lineups",
    "player_match_stats.csv": "player_match_stats",
    "player_stats.csv":       "player_stats",
    "standings_by_round.csv": "standings_by_round",
    "team_match_stats.csv":   "team_match_stats",
}

def get_supabase_client() -> Client | None:
    url = os.environ.get("SUPABASE_URL")
    key = os.environ.get("SUPABASE_KEY")
    if not url or not key:
        print("  ⚠️  SUPABASE_URL o SUPABASE_KEY no definides — saltant upload")
        return None
    return create_client(url, key)


def to_python_native(val, force_int: bool = False):
    if val is None:
        return None
    if isinstance(val, (float, np.floating)) and np.isnan(val):
        return None
    if isinstance(val, (np.integer,)):
        return int(val)
    if isinstance(val, (np.floating,)):
        return int(val) if force_int else float(val)
    if isinstance(val, float):
        return int(val) if force_int else val
    if isinstance(val, (np.bool_,)):
        return bool(val)
    return val


def upload_grup_to_supabase(client: Client, categoria: str, grup: int, output_dir: Path):
    """Puja els CSVs d'un grup a Supabase: esborra els registres anteriors i insereix els nous."""
    print(f"  ☁️  Pujant {categoria} Grup {grup} a Supabase...")

    COLS_SCHEMA = {
        "matches":           ["categoria","grup","season","competition","jornada","local_team","away_team","goals_home","goals_away","venue"],
        "matches_info":      ["categoria","grup","season","competition","jornada","date","time","home_team","away_team","goals_home","goals_away","referee"],
        "matches_events":    ["categoria","grup","match_date","jornada","home_team","away_team","event_type","minute","team","player","detail"],
        "matches_lineups":   ["categoria","grup","match_date","jornada","home_team","away_team","team","player","shirt_number","position","stats"],
        "player_match_stats":["categoria","grup","match_id","jornada","match_date","player","team","starter","minutes_played","goals","yellow_cards","red_cards"],
        "player_stats":      ["categoria","grup","player","team","matches_played","starts","total_minutes","goals","goals_per_90","cards_per_90"],
        "standings_by_round":["categoria","grup","team","jornada","position","played","wins","draws","losses","goals_for","goals_against","goal_diff","points"],
        "team_match_stats":  ["categoria","grup","team","match_id","jornada","match_date","opponent","home_away","goals_for","goals_against","yellow_cards","red_cards"],
    }

    INT_NONNULL = {"jornada","grup","goals_home","goals_away","goals_for","goals_against",
                   "goal_diff","points","played","wins","draws","losses","position",
                   "starter","minutes_played","goals","yellow_cards","red_cards",
                   "starts","total_minutes","matches_played","shirt_number"}
    INT_NULLABLE = {"minute"}

    for csv_name, table_name in CSV_TABLE_MAP.items():
        csv_path = output_dir / csv_name
        if not csv_path.exists():
            print(f"    ⚠️  {csv_name} no trobat, saltant")
            continue

        try:
            df = pd.read_csv(csv_path)
        except pd.errors.EmptyDataError:
            print(f"    ⚠️  {csv_name} sense capçalera/dades (fitxer buit), saltant")
            continue
        if df.empty:
            print(f"    ⚠️  {csv_name} buit, saltant")
            continue

        if "categoria" not in df.columns:
            df["categoria"] = categoria
        if "grup" not in df.columns:
            df["grup"] = grup

        valid_cols = COLS_SCHEMA.get(table_name, list(df.columns))
        df = df[[c for c in valid_cols if c in df.columns]].copy()

        for col in INT_NONNULL:
            if col in df.columns:
                df[col] = pd.to_numeric(df[col], errors="coerce").fillna(0)
        for col in INT_NULLABLE:
            if col in df.columns:
                df[col] = pd.to_numeric(df[col], errors="coerce")

        records = []
        for row in df.to_dict(orient="records"):
            clean = {}
            for k, v in row.items():
                if k in INT_NONNULL:
                    clean[k] = int(to_python_native(v, force_int=True) or 0)
                elif k in INT_NULLABLE:
                    native = to_python_native(v, force_int=True)
                    clean[k] = native
                else:
                    clean[k] = to_python_native(v)
            records.append(clean)

        try:
            client.table(table_name).delete().eq("categoria", categoria).eq("grup", grup).execute()
            chunk_size = 500
            for i in range(0, len(records), chunk_size):
                client.table(table_name).insert(records[i:i + chunk_size]).execute()
            print(f"    ✅ {table_name}: {len(records)} registres pujats")
        except Exception as e:
            print(f"    ❌ Error pujant {table_name}: {e}")


# ============================================================================
# PIPELINE PRINCIPAL PER UN GRUP
# ============================================================================

MATCH_INFO_COLUMNS = [
    "season", "competition", "date", "time", "jornada",
    "home_team", "away_team", "goals_home", "goals_away", "venue",
]
MATCH_EVENTS_COLUMNS = [
    "match_date", "jornada", "home_team", "away_team",
    "event_type", "minute", "team", "player", "detail",
]
MATCH_LINEUPS_COLUMNS = [
    "match_date", "jornada", "home_team", "away_team",
    "team", "player", "shirt_number", "position",
    "minutes_played", "goals", "yellow_cards", "red_cards",
]


def process_grup(categoria: str, grup: int, output_dir: Path, debug: bool = False):
    """Executa el pipeline sencer per a un grup i guarda els CSVs."""
    output_dir.mkdir(parents=True, exist_ok=True)
    print(f"\n{'='*70}")
    print(f"  {categoria} — Grup {grup}")
    print(f"{'='*70}")

    if get_grup_ids(categoria, grup) is None:
        print(f"  ⚠️  competicioId/grupId no configurats per {categoria} Grup {grup} "
              f"al diccionari GRUP_IDS — saltant tot el grup.")
        return False

    # 1. Calendari (API interna, sense navegador) → tots els partits + IDs d'acta
    print("  1/6 Descarregant calendari (API)...")
    df_calendar = scrape_calendar_api(categoria, grup, debug=debug)
    acta_ids = [int(x) for x in df_calendar["acta_id"].dropna().tolist()]
    print(f"     ✅ {len(acta_ids)} actes a processar")

    # 2. Actes (requests) → match_info, events, lineups
    print("  2/6 Scraping actes de partits...")
    all_match_info, all_events, all_lineups = [], [], []
    ok = err = 0
    for i, acta_id in enumerate(acta_ids, 1):
        print(f"     [{i:3}/{len(acta_ids)}] acta {acta_id}", end="")
        mi, ev, lu, success = scrape_match_acta(acta_id, categoria, grup)
        if success:
            all_match_info.append(mi)
            all_events.extend(ev)
            all_lineups.extend(lu)
            ok += 1
            print(f" ✅ {mi.get('home_team')} {mi.get('goals_home')}-{mi.get('goals_away')} {mi.get('away_team')}")
        else:
            err += 1
            print(" ❌")
        time.sleep(SLEEP_BETWEEN_REQUESTS)
    print(f"     ✅ {ok} actes OK / ❌ {err} errors")

    df_match_info = pd.DataFrame(all_match_info) if all_match_info else pd.DataFrame(columns=MATCH_INFO_COLUMNS)
    df_events     = pd.DataFrame(all_events)     if all_events     else pd.DataFrame(columns=MATCH_EVENTS_COLUMNS)
    df_lineups    = pd.DataFrame(all_lineups)    if all_lineups    else pd.DataFrame(columns=MATCH_LINEUPS_COLUMNS)

    # 3. matches.csv: partim del calendari SENCER (tots els partits, jugats
    #    o no) i hi encreuem el resultat real de les actes ja processades.
    #    L'encreuament es fa per (jornada, equips normalitzats) perquè el
    #    calendari mostra els noms amb una lletra de subgrup al final
    #    ("PENYA ESPORTIVA MONTAGUT A") que l'acta no porta.
    print("  3/6 Consolidant matches.csv...")

    def _normalize_team(name):
        if not isinstance(name, str):
            return name
        return re.sub(r"\s+[A-B]$", "", name.strip()).upper()

    calendar_rows = df_calendar.dropna(subset=["local_team", "away_team"]).copy() if not df_calendar.empty else pd.DataFrame()

    if not calendar_rows.empty:
        cols_disponibles = [c for c in ["jornada", "local_team", "away_team", "date", "venue",
                                          "goals_home", "goals_away"] if c in calendar_rows.columns]
        df_matches = calendar_rows[cols_disponibles].copy()
        if "goals_home" not in df_matches.columns:
            df_matches["goals_home"] = None
        if "goals_away" not in df_matches.columns:
            df_matches["goals_away"] = None
        df_matches["season"] = "2026-2027"
        df_matches["competition"] = f"{categoria.capitalize()} Catalana"

        if not df_match_info.empty:
            results_lookup = {}
            for _, r in df_match_info.iterrows():
                key = (r.get("jornada"), _normalize_team(r.get("home_team")), _normalize_team(r.get("away_team")))
                results_lookup[key] = (r.get("goals_home"), r.get("goals_away"))

            def _lookup_score(row):
                # Si l'API del calendari ja portava el resultat, el respectem;
                # només busquem a l'acta el que encara falti.
                if pd.notna(row.get("goals_home")) and pd.notna(row.get("goals_away")):
                    return (row["goals_home"], row["goals_away"])
                key = (row["jornada"], _normalize_team(row["local_team"]), _normalize_team(row["away_team"]))
                return results_lookup.get(key, (row.get("goals_home"), row.get("goals_away")))

            scores = df_matches.apply(_lookup_score, axis=1)
            df_matches["goals_home"] = [s[0] for s in scores]
            df_matches["goals_away"] = [s[1] for s in scores]
    elif not df_match_info.empty:
        # Sense calendari (p. ex. l'API de partits no ha retornat res)
        # però amb actes: com a mínim guardem els partits jugats trobats.
        df_matches = df_match_info.rename(columns={"home_team": "local_team"}).copy()
        if "venue" not in df_matches.columns:
            df_matches["venue"] = None
        df_matches = df_matches[["season", "competition", "jornada", "local_team",
                                   "away_team", "goals_home", "goals_away", "venue"]].copy()
    else:
        df_matches = pd.DataFrame(columns=[
            "season", "competition", "jornada", "local_team",
            "away_team", "goals_home", "goals_away", "venue",
        ])

    if not df_matches.empty:
        df_matches.insert(0, "categoria", categoria)
        df_matches.insert(1, "grup", grup)
        df_matches.sort_values(["jornada", "local_team"], inplace=True)
    df_matches.to_csv(output_dir / "matches.csv", index=False)
    n_jugats = int(df_matches["goals_home"].notna().sum()) if not df_matches.empty else 0
    print(f"     ✅ {len(df_matches)} partits ({n_jugats} jugats)")

    # 4. Classificació per jornada
    print("  4/6 Classificació per jornada...")
    df_standings = compute_standings_by_round(df_matches)
    df_standings.to_csv(output_dir / "standings_by_round.csv", index=False)
    print(f"     ✅ {len(df_standings)} files")

    # Desar all_matches_info / events / lineups amb categoria+grup
    for df_ref, path in [
        (df_match_info, output_dir / "all_matches_info.csv"),
        (df_events,     output_dir / "all_matches_events.csv"),
        (df_lineups,    output_dir / "all_matches_lineups.csv"),
    ]:
        if not df_ref.empty:
            if "categoria" not in df_ref.columns:
                df_ref.insert(0, "categoria", categoria)
            if "grup" not in df_ref.columns:
                df_ref.insert(1, "grup", grup)
        df_ref.to_csv(path, index=False)

    # 5. Estadístiques de jugadors (dependent de lineups — de moment buit,
    #    veure avís al mòdul 3 sobre alineacions pendents de validar)
    print("  5/6 Estadístiques jugadors...")
    df_pms = build_player_match_stats(df_lineups, df_events)
    df_pms.to_csv(output_dir / "player_match_stats.csv", index=False)
    df_ps = build_player_stats(df_pms)
    df_ps.to_csv(output_dir / "player_stats.csv", index=False)

    # 6. Estadístiques per equip
    print("  6/6 Estadístiques per equip...")
    df_tms = build_team_match_stats(df_match_info, df_pms)
    df_tms.to_csv(output_dir / "team_match_stats.csv", index=False)

    print(f"  💾 Fitxers guardats a: {output_dir}")

    supabase = get_supabase_client()
    if supabase:
        upload_grup_to_supabase(supabase, categoria, grup, output_dir)

    return True


# ============================================================================
# CONSOLIDACIÓ FINAL
# ============================================================================

FITXERS_A_CONSOLIDAR = [
    "all_matches_events.csv",
    "all_matches_info.csv",
    "all_matches_lineups.csv",
    "matches.csv",
    "player_match_stats.csv",
    "player_stats.csv",
    "standings_by_round.csv",
    "team_match_stats.csv",
]


def consolidar_tot(base_dir: Path, output_dir: Path):
    output_dir.mkdir(parents=True, exist_ok=True)
    print(f"\n{'='*70}")
    print("  CONSOLIDACIÓ FINAL")
    print(f"{'='*70}")

    for nom_fitxer in FITXERS_A_CONSOLIDAR:
        dfs = []
        for categoria, num_grups in CATEGORIES.items():
            for grup in range(1, num_grups + 1):
                path = base_dir / categoria / f"GRUP{grup}" / nom_fitxer
                if path.exists():
                    try:
                        df = pd.read_csv(path)
                    except pd.errors.EmptyDataError:
                        continue
                    if "categoria" not in df.columns:
                        df.insert(0, "categoria", categoria)
                    if "grup" not in df.columns:
                        df.insert(1, "grup", grup)
                    dfs.append(df)

        if dfs:
            consolidat = pd.concat(dfs, ignore_index=True)
            nom_sortida = f"consolidat_{TEMPORADA}_{nom_fitxer}"
            consolidat.to_csv(output_dir / nom_sortida, index=False)
            print(f"  ✅ {nom_sortida}: {len(consolidat):,} files")
        else:
            print(f"  ⚠️  {nom_fitxer}: cap fitxer trobat")


# ============================================================================
# ENTRY POINT
# ============================================================================

def main():
    parser = argparse.ArgumentParser(description="Scraper FCF — Futbol Català (nova web)")
    parser.add_argument("--categoria", choices=list(CATEGORIES.keys()),
                        help="Processa només aquesta categoria")
    parser.add_argument("--grup", type=int,
                        help="Processa només aquest grup (requereix --categoria)")
    parser.add_argument("--output", default="dades",
                        help="Directori de sortida base (default: dades/)")
    parser.add_argument("--only-consolidar", action="store_true",
                        help="Només fa la consolidació final sense scraping")
    parser.add_argument("--debug", action="store_true",
                        help="Desa bolcats de depuració (text del calendari renderitzat)")
    args = parser.parse_args()

    base_output = Path(args.output)

    if args.only_consolidar:
        consolidar_tot(base_output, base_output)
        print("\n🎉 Consolidació completada!")
        return

    if args.categoria and args.grup:
        out = base_output / args.categoria / f"GRUP{args.grup}"
        process_grup(args.categoria, args.grup, out, debug=args.debug)
    elif args.categoria:
        for grup in range(1, CATEGORIES[args.categoria] + 1):
            out = base_output / args.categoria / f"GRUP{grup}"
            process_grup(args.categoria, grup, out, debug=args.debug)
    else:
        for categoria, num_grups in CATEGORIES.items():
            for grup in range(1, num_grups + 1):
                out = base_output / categoria / f"GRUP{grup}"
                process_grup(categoria, grup, out, debug=args.debug)

    consolidar_tot(base_output, base_output)

    if not args.only_consolidar:
        print("\n🔮 Generant prediccions de classificació...")
        try:
            generar_prediccions(base_output)
        except Exception as e:
            print(f"  ⚠️  Error generant prediccions: {e}")

    print("\n🎉 Scraping completat!")


if __name__ == "__main__":
    main()
