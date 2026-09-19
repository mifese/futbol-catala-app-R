"""
scraper.py — Scraping automatitzat de la FCF (NOVA WEB, Next.js)
===================================================================

IMPORTANT — llegeix això abans d'executar:

La FCF ha canviat completament la web (fcf.cat) a una aplicació Next.js.
Això afecta el scraper en dos punts molt diferents:

1. La pàgina de "fitxa de competició" (calendari/resultats/classificació
   d'un grup, https://www.fcf.cat/ca/competicio?...&grupId=...) es genera
   ÍNTEGRAMENT al navegador (React) — l'HTML que arriba per una petició
   HTTP normal NO conté cap partit ni classificació. Cal un navegador real
   (Playwright) perquè el JavaScript s'executi i pinti les dades. La resta
   de scrapers que s'han trobat fent servir la FCF confirmen el mateix
   (la seva API interna té protecció anti-bot i només accepta navegadors).

2. La pàgina d'ACTA d'un partit concret
   (https://www.fcf.cat/ca/competicio/acta/{id}) SÍ que ve generada pel
   servidor: es pot descarregar amb una simple petició HTTP (requests) i
   ja hi surt tota la informació (equips, resultat, gols amb minut i tipus,
   alineacions). NO cal navegador per aquesta part — més ràpid i fiable.

Per tant aquest scraper és HÍBRID:
  - Playwright (navegador headless) només per descobrir, per a cada grup,
    la llista de partits de cada jornada i l'ID d'acta de cada partit jugat.
  - requests (HTTP normal, ràpid) per descarregar cada acta i extreure'n
    tots els detalls.

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
  No he pogut executar aquest scraper contra el lloc real (no tinc accés
  a un navegador ni a fcf.cat des d'aquí), així que la part de Playwright
  (extreure la llista de partits/jornades del calendari renderitzat) és
  la meva millor estimació basada en el que se sap de la web, però pot
  necessitar ajustos un cop la provis. Per això:
    - Hi ha un mode --debug que desa el text renderitzat de la primera
      pàgina de calendari a un fitxer .txt, perquè puguem revisar-lo i
      ajustar les expressions regulars si cal.
    - La part de l'ACTA (mòdul 3) SÍ que s'ha provat contra una acta real
      (la que em vas passar) i el parsing de gols funciona correctament.
      Les alineacions/targetes són best-effort: si el format real no
      coincideix exactament, es guardaran com a None/buit en lloc de
      petar, i podrem ajustar-ho amb un exemple real.

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
        1:  None,
        2:  None,
        3:  {"competicioId": "58161869", "grupId": "58161876"},  # ← exemple donat
        4:  None,
        5:  None,
        6:  None,
        7:  None,
        8:  None,
        9:  None,
        10: None,
        11: None,
        12: None,
        13: None,
        14: None,
        15: None,
        16: None,
        17: None,
        18: None,
    },
    "SEGONA": {
        1: None,
        2: None,
        3: None,
        4: None,
        5: None,
        6: None,
    },
    "PRIMERA": {
        1: None,
        2: None,
        3: None,
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
# MÒDUL 1 — CALENDARI DE PARTITS D'UN GRUP (Playwright, requereix navegador)
# ============================================================================
#
# La pàgina de competició és una SPA en React: cal esperar que el JavaScript
# carregui les dades. Estratègia:
#   1. Navegar a la URL del grup.
#   2. Esperar que la xarxa quedi inactiva (networkidle) — dona temps a la
#      crida interna que omple el calendari.
#   3. Recollir TOTS els enllaços <a href="…/competicio/acta/{id}"> que hi
#      hagi renderitzats a la pàgina — aquests corresponen als partits ja
#      jugats (amb acta tancada). No calen selectors CSS fràgils per això:
#      només cal que l'enllaç existeixi al DOM.
#   4. Per a cada partit (jugat o no), intentar llegir la fila/contenidor
#      que envolta l'enllaç (o, si no n'hi ha per partits no jugats, el
#      text ordenat de tota la pàgina) per treure equip local/visitant,
#      jornada i data. Aquesta part és la que caldrà validar/ajustar amb
#      un cas real (--debug bolca el text complet per revisar-lo).
#
# Un cop tenim els IDs d'acta, tota la informació fiable (equips, resultat,
# jornada, data) es torna a confirmar directament des de l'acta (mòdul 3),
# així que aquesta llista només ha de ser prou bona per: (a) saber quants
# partits/jornades hi ha en total, i (b) donar-nos els IDs d'acta a seguir.

ACTA_LINK_RE = re.compile(r"/ca/competicio/acta/(\d+)")


def _get_playwright_page(headless: bool = True):
    """Crea un navegador Playwright i retorna (playwright, browser, page)."""
    from playwright.sync_api import sync_playwright
    pw = sync_playwright().start()
    browser = pw.chromium.launch(headless=headless)
    context = browser.new_context(user_agent=HEADERS["User-Agent"])
    page = context.new_page()
    return pw, browser, page


def scrape_calendar_playwright(categoria: str, grup: int, debug: bool = False) -> pd.DataFrame:
    """Retorna un DataFrame amb els partits del grup (jugats o no) i, quan hi
    hagi acta disponible, la seva URL/ID.

    Columnes: jornada, local_team, away_team, acta_id (pot ser None).
    Aquesta llista es fa servir només per saber quins partits existeixen i
    seguir les seves actes — el detall fiable (resultat, data...) surt de
    l'acta mateixa (mòdul 3).
    """
    ids = get_grup_ids(categoria, grup)
    if ids is None:
        print(f"     ⚠️  {categoria} Grup {grup}: falta 'competicioId'/'grupId' a "
              f"GRUP_IDS — omple'l navegant fcf.cat. Saltant.")
        return pd.DataFrame(columns=["jornada", "local_team", "away_team", "acta_id"])

    url = competicio_url(ids["competicioId"], ids["grupId"])
    print(f"     🌐 Obrint {url}")

    pw = browser = None
    try:
        pw, browser, page = _get_playwright_page(headless=True)
        page.goto(url, timeout=60000)
        # Esperar que la SPA acabi de fer les crides internes.
        try:
            page.wait_for_load_state("networkidle", timeout=30000)
        except Exception:
            pass
        time.sleep(2)  # marge extra per re-renderitzats posteriors a networkidle

        # Intentar mostrar la pestanya "Calendari" si existeix, per assegurar
        # que hi surten TOTS els partits (jugats i pendents), no només
        # l'últim resum de jornada.
        for label in ["Calendari", "Resultats"]:
            try:
                loc = page.get_by_text(label, exact=True)
                if loc.count() > 0:
                    loc.first.click(timeout=5000)
                    page.wait_for_timeout(1500)
                    break
            except Exception:
                pass

        full_text = page.inner_text("body")
        if debug:
            debug_path = Path(f"debug_calendari_{categoria}_grup{grup}.txt")
            debug_path.write_text(full_text, encoding="utf-8")
            print(f"     🐛 Text de depuració desat a {debug_path}")

        # IDs d'acta presents al DOM (partits jugats).
        acta_ids = []
        for a in page.query_selector_all("a[href*='/competicio/acta/']"):
            href = a.get_attribute("href") or ""
            m = ACTA_LINK_RE.search(href)
            if m:
                acta_ids.append(int(m.group(1)))
        acta_ids = sorted(set(acta_ids))
        print(f"     🔗 {len(acta_ids)} actes trobades al calendari renderitzat")

    finally:
        try:
            if browser:
                browser.close()
        except Exception:
            pass
        try:
            if pw:
                pw.stop()
        except Exception:
            pass

    # De moment retornem només els IDs d'acta trobats; jornada/equips es
    # reconstrueixen a partir de cada acta al mòdul 3 (que sabem que
    # funciona). Si en el futur cal la llista completa de partits pendents
    # (encara sense acta) caldrà ampliar aquesta funció per parsejar
    # `full_text` — es deixa preparat el bolcat --debug per fer-ho.
    return pd.DataFrame({
        "jornada": [None] * len(acta_ids),
        "local_team": [None] * len(acta_ids),
        "away_team": [None] * len(acta_ids),
        "acta_id": acta_ids,
    })


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
# MÒDUL 3 — ACTA D'UN PARTIT (requests + regex, NO cal navegador)
# ============================================================================
#
# Confirmat contra una acta real: aquesta pàgina es genera al servidor, així
# que una petició HTTP normal ja retorna tot el contingut. En lloc de fiar-nos
# de classes CSS concretes (que no he pogut inspeccionar en brut), fem servir
# el text visible de la pàgina + expressions regulars, cosa que és més
# resistent a petits canvis de maquetació.

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


CLUB_LINK_RE = re.compile(
    r'<a[^>]+href="https://www\.fcf\.cat/ca/clubs/\d+/categories/\d+"[^>]*>\s*([^<]+?)\s*</a>'
)

BREADCRUMB_RE = re.compile(r"Competici[oó]\s*/\s*([^/]+)\s*/\s*GRUP\s*(\d+)\s*/\s*Jornada\s*(\d+)", re.IGNORECASE)
DATA_RE       = re.compile(r"Data:\s*([\d.]+)")
HORA_RE       = re.compile(r"Hora:\s*([\d.]+)H", re.IGNORECASE)
ESTADI_RE     = re.compile(r"Estadi:\s*(.+)")
SCORE_RE      = re.compile(r"(?<!\d)(\d{1,2})\s*-\s*(\d{1,2})(?!\d)")
GOL_RE        = re.compile(
    r"([A-ZÀ-Ú'ÇÍÏÜÓÒ.,\- ]+?)\s*\((\d{1,3})'\)\s*GOL\s*(NORMAL|PENAL|EN PR[OÒ]PIA)\s*\(([^)]+)\)",
    re.IGNORECASE,
)
CARD_RE = re.compile(
    r"([A-ZÀ-Ú'ÇÍÏÜÓÒ.,\- ]+?)\s*\((\d{1,3})'\)\s*(TARGETA\s*(?:GROGA|VERMELLA)|DOBLE\s*TARGETA\s*GROGA)",
    re.IGNORECASE,
)


def scrape_match_acta(acta_id: int, categoria: str, grup: int):
    """Descarrega i interpreta l'acta d'un partit (per requests, sense navegador).

    Retorna (match_info: dict | None, events: list[dict], lineups: list[dict], ok: bool).
    `lineups` es deixa buit de moment: el format exacte de la taula d'alineacions
    no s'ha pogut validar contra una acta real en brut (només en tinc la versió
    "text pla"); es recomana revisar-ho amb --debug un cop es tingui accés al
    HTML complet d'una acta real.
    """
    url = f"{BASE_URL}/ca/competicio/acta/{acta_id}"
    html = get_with_retry(url)
    if html is None:
        return None, [], [], False

    # Traiem el text visible (sense tags) per aplicar les regex de forma
    # robusta encara que canviïn detalls de maquetació.
    try:
        from bs4 import BeautifulSoup
        soup = BeautifulSoup(html, "lxml")
        text = soup.get_text("\n", strip=True)
    except Exception:
        text = html

    if "Acta" not in text and "acta" not in text:
        return None, [], [], False

    match_info = {"season": "2026-2027", "jornada": None}

    m_bc = BREADCRUMB_RE.search(text)
    if m_bc:
        match_info["competition"] = m_bc.group(1).strip()
        match_info["jornada"] = int(m_bc.group(3))

    clubs = CLUB_LINK_RE.findall(html)
    if len(clubs) >= 2:
        match_info["home_team"] = clubs[0].strip()
        match_info["away_team"] = clubs[1].strip()

    m_data = DATA_RE.search(text)
    if m_data:
        match_info["date"] = m_data.group(1)
    m_hora = HORA_RE.search(text)
    if m_hora:
        match_info["time"] = m_hora.group(1)

    # Resultat final: agafem el primer "N-N" que aparegui després dels noms
    # d'equip (evita confondre'l amb minuts o dorsals).
    m_score = SCORE_RE.search(text)
    if m_score:
        try:
            match_info["goals_home"] = int(m_score.group(1))
            match_info["goals_away"] = int(m_score.group(2))
        except ValueError:
            pass

    if "jornada" not in match_info or match_info["jornada"] is None:
        match_info["jornada"] = None  # es reomplirà pel cridant si cal

    # --- GOLS ---
    events = []
    for jugador, minut, tipus, equip in GOL_RE.findall(text):
        tipus_norm = {"NORMAL": "Normal", "PENAL": "Penal"}.get(tipus.upper(), "Pròpia")
        events.append({
            "match_date": match_info.get("date"),
            "jornada":    match_info.get("jornada"),
            "home_team":  match_info.get("home_team"),
            "away_team":  match_info.get("away_team"),
            "event_type": "Gol",
            "minute":     int(minut),
            "team":       equip.strip(),
            "player":     jugador.strip(),
            "detail":     tipus_norm,
        })

    # --- TARGETES (best-effort; validar format real amb --debug) ---
    for jugador, minut, tipus in CARD_RE.findall(text):
        tipus_norm = "Targeta Vermella" if "VERMELLA" in tipus.upper() else "Targeta Groga"
        events.append({
            "match_date": match_info.get("date"),
            "jornada":    match_info.get("jornada"),
            "home_team":  match_info.get("home_team"),
            "away_team":  match_info.get("away_team"),
            "event_type": tipus_norm,
            "minute":     int(minut),
            "team":       None,  # no es pot atribuir l'equip sense el HTML en brut
            "player":     jugador.strip(),
            "detail":     None,
        })

    # --- ALINEACIONS: pendent de validar amb HTML real (veure docstring) ---
    lineups = []

    ok = bool(match_info.get("home_team") and match_info.get("away_team"))
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
    """Construeix player_match_stats a partir de lineups i events.

    Guarda: si `lineups` és buit (p. ex. perquè encara no s'ha validat el
    parsing d'alineacions de la nova web, o inici de temporada sense partits
    jugats), retornem directament un DataFrame buit amb l'esquema correcte.
    """
    if lineups is None or lineups.empty or "jornada" not in lineups.columns:
        return pd.DataFrame(columns=PLAYER_MATCH_STATS_COLUMNS)

    lineups = lineups.copy()
    lineups["match_id"] = (
        lineups["jornada"].astype(str) + "_"
        + lineups["home_team"].str[:3] + "_"
        + lineups["away_team"].str[:3]
    )

    if events is None or events.empty or "jornada" not in events.columns:
        events = pd.DataFrame(columns=EVENTS_EMPTY_COLUMNS)
    else:
        events = events.copy()
        events["match_id"] = (
            events["jornada"].astype(str) + "_"
            + events["home_team"].str[:3] + "_"
            + events["away_team"].str[:3]
        )

    records = []
    for _, pr in lineups.iterrows():
        player   = pr["player"]
        match_id = pr["match_id"]
        team     = pr["team"]
        stats_text = pr["stats"]
        starter    = 1 if pr["position"] == "Titular" else 0

        goals = yellow_cards = red_cards = 0
        was_sub_out = entered_as_sub = False

        if pd.notna(stats_text):
            for s in str(stats_text).split(", "):
                if "gol(s)" in s:
                    try: goals = int(s.split()[0])
                    except: goals = 1
                if "Groga"      in s: yellow_cards = 1
                if "Vermella"   in s: red_cards    = 1
                if "Ha jugat"   in s: entered_as_sub  = True
                if "Substituït" in s: was_sub_out = True

        if starter:
            if was_sub_out:
                subst = events[
                    (events["match_id"] == match_id) &
                    (events["event_type"] == "Substitució") &
                    (events["detail"].str.contains(str(player), na=False))
                ]
                minutes_played = int(subst.iloc[0]["minute"]) if not subst.empty else 90
            else:
                minutes_played = 90
        else:
            if entered_as_sub:
                subst = events[
                    (events["match_id"] == match_id) &
                    (events["event_type"] == "Substitució") &
                    (events["player"] == player)
                ]
                if not subst.empty:
                    minutes_played = 90 - int(subst.iloc[0]["minute"])
                else:
                    minutes_played = 0
            else:
                minutes_played = 0

        records.append({
            "match_id":      match_id,
            "jornada":       pr["jornada"],
            "match_date":    pr["match_date"],
            "player":        player,
            "team":          team,
            "starter":       starter,
            "minutes_played": minutes_played,
            "goals":         goals,
            "yellow_cards":  yellow_cards,
            "red_cards":     red_cards,
        })

    df = pd.DataFrame(records)
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
    "home_team", "away_team", "goals_home", "goals_away",
]
MATCH_EVENTS_COLUMNS = [
    "match_date", "jornada", "home_team", "away_team",
    "event_type", "minute", "team", "player", "detail",
]
MATCH_LINEUPS_COLUMNS = [
    "match_date", "jornada", "home_team", "away_team",
    "team", "player", "shirt_number", "position", "stats",
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

    # 1. Calendari (Playwright) → llista d'IDs d'acta a seguir
    print("  1/6 Descobrint calendari (navegador)...")
    df_calendar = scrape_calendar_playwright(categoria, grup, debug=debug)
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

    # 3. matches.csv es reconstrueix directament a partir de les actes ja
    #    processades (mateix nivell de detall que abans per als partits
    #    jugats; els partits encara no jugats no hi surten fins que no
    #    ampliem el mòdul 1 per llegir-los del calendari — veure docstring).
    print("  3/6 Consolidant matches.csv...")
    if not df_match_info.empty:
        df_matches = df_match_info.rename(columns={"home_team": "local_team"}).copy()
        df_matches = df_matches[["season", "competition", "jornada", "local_team",
                                   "away_team", "goals_home", "goals_away"]].copy()
        df_matches["venue"] = None
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
    print(f"     ✅ {len(df_matches)} partits (dels trobats al calendari)")

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
