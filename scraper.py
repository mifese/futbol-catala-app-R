"""
scraper.py — Scraping automatitzat de la FCF
Substitueix tots els notebooks .ipynb per un sol script executable.

Ús:
    python scraper.py                   # Scraping complet (totes les categories i grups)
    python scraper.py --categoria TERCERA
    python scraper.py --categoria TERCERA --grup 1
"""

import requests
from bs4 import BeautifulSoup
import pandas as pd
import numpy as np
import re
import time
import os
import argparse
import warnings
from pathlib import Path
from unidecode import unidecode

warnings.filterwarnings("ignore")

# ============================================================================
# CONFIGURACIÓ CENTRAL — modifica aquí si canvia la temporada o els grups
# ============================================================================

TEMPORADA     = "25_26"
TEMPORADA_FCF = "2526"
HEADERS       = {"User-Agent": "Mozilla/5.0"}
SLEEP_BETWEEN_REQUESTS = 2.5  # segons entre peticions (respecta el servidor)
MAX_RETRIES            = 4    # intents en cas de 503
RETRY_BACKOFF          = 30   # segons d'espera base entre reintents (es dobla cada vegada)

# Nombre de jornades per categoria
MAX_JORNADES = {
    "TERCERA": 30,
    "SEGONA":  30,
    "PRIMERA": 30,
}

# Nombre de grups per categoria
CATEGORIES = {
    "TERCERA": 18,
    "SEGONA":   6,
    "PRIMERA":  3,
}

# Noms FCF per a les URLs
FCF_NOMS = {
    "TERCERA": "tercera-catalana",
    "SEGONA":  "segona-catalana",
    "PRIMERA": "primera-catalana",
}

# Codis FCF per a les actes
FCF_CODIS = {
    "TERCERA": "3cat",
    "SEGONA":  "2cat",
    "PRIMERA": "1cat",
}

# ============================================================================
# UTILITATS
# ============================================================================

def normalize_team_name(team_name: str) -> str:
    """Converteix el nom d'un equip al format URL de la FCF."""
    name = unidecode(team_name)
    name = name.lower()
    name = name.replace("'", "")
    name = name.replace(",", " ")
    name = name.replace(".", "")
    name = re.sub(r"[^a-z0-9\s-]", "", name)
    name = name.replace(" ", "-")
    name = re.sub(r"-+", "-", name)
    name = name.strip("-")
    return name


def get(url: str) -> BeautifulSoup | None:
    """Petició HTTP amb retry automàtic i backoff exponencial en cas de 503."""
    for attempt in range(1, MAX_RETRIES + 1):
        try:
            resp = requests.get(url, headers=HEADERS, timeout=20)
            if resp.status_code == 503:
                wait = RETRY_BACKOFF * (2 ** (attempt - 1))
                print(f"    ⏳ 503 rebut (intent {attempt}/{MAX_RETRIES}), esperant {wait}s...")
                time.sleep(wait)
                continue
            resp.raise_for_status()
            return BeautifulSoup(resp.text, "lxml")
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
    print(f"    ❌ Exhaurits {MAX_RETRIES} intents per: {url}")
    return None


# ============================================================================
# MÒDUL 1 — PARTITS (matches.csv)
# ============================================================================

def scrape_matches(categoria: str, grup: int) -> pd.DataFrame:
    """Extreu tots els partits (resultats) de totes les jornades d'un grup."""
    base_url = (
        f"https://www.fcf.cat/resultats/{TEMPORADA_FCF}/futbol-11/"
        f"{FCF_NOMS[categoria]}/grup-{grup}/jornada-"
    )
    matches = []

    for jornada_num in range(1, MAX_JORNADES[categoria] + 1):
        soup = get(f"{base_url}{jornada_num}")
        if soup is None:
            continue

        for row in soup.select("tr.linia"):
            try:
                local_td = row.select_one("td.resultats-w-equip.tr")
                away_td  = row.select_one("td.resultats-w-equip.tl")
                if not local_td or not away_td:
                    continue
                local_a = local_td.find("a")
                away_a  = away_td.find("a")
                local = local_a.get_text(strip=True) if local_a else None
                away  = away_a.get_text(strip=True)  if away_a  else None
                if not local or not away:
                    continue

                gols_home = gols_away = None
                resultat_td = row.select_one("td.resultats-w-resultat")
                if resultat_td:
                    for div in resultat_td.find_all("div", class_="bg-darkgrey"):
                        if "fs-17" in div.get("class", []):
                            txt = div.get_text(strip=True).replace(" ", "")
                            if "-" in txt:
                                parts = txt.split("-")
                                if len(parts) == 2:
                                    try:
                                        gols_home = int(parts[0])
                                        gols_away = int(parts[1])
                                        break
                                    except ValueError:
                                        pass

                camp_a = row.select_one("td.resultats-w-text2 a")
                camp = camp_a.get_text(strip=True).replace('"', "") if camp_a else None

                matches.append({
                    "season":      "2025-2026",
                    "competition": f"{categoria.capitalize()} Catalana",
                    "group":       grup,
                    "jornada":     jornada_num,
                    "local_team":  local,
                    "away_team":   away,
                    "goals_home":  gols_home,
                    "goals_away":  gols_away,
                    "venue":       camp,
                })
            except Exception as e:
                print(f"    ⚠️  Jornada {jornada_num}, error en fila: {e}")

        time.sleep(SLEEP_BETWEEN_REQUESTS)

    df = pd.DataFrame(matches)
    if not df.empty:
        df.sort_values(["jornada", "local_team"], inplace=True)
    return df


# ============================================================================
# MÒDUL 2 — CLASSIFICACIÓ ACUMULADA (standings_by_round.csv)
# ============================================================================

def compute_standings_by_round(matches: pd.DataFrame) -> pd.DataFrame:
    """Calcula la classificació acumulada jornada a jornada."""
    played = matches.dropna(subset=["goals_home", "goals_away"]).copy()
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

    return pd.concat(standings_all, ignore_index=True) if standings_all else pd.DataFrame()


# ============================================================================
# MÒDUL 3 — ACTA DE CADA PARTIT (events, lineups, match_info)
# ============================================================================

def scrape_match_acta(home_team: str, away_team: str, jornada_num: int,
                      categoria: str, grup: int):
    """Extreu l'acta completa d'un partit (info, events, alineacions)."""
    home_url = normalize_team_name(home_team)
    away_url = normalize_team_name(away_team)
    codi     = FCF_CODIS[categoria]

    url = (
        f"https://www.fcf.cat/acta/{TEMPORADA_FCF}/futbol-11/"
        f"{FCF_NOMS[categoria]}/grup-{grup}/{codi}/"
        f"{home_url}/{codi}/{away_url}"
    )

    soup = get(url)
    if soup is None:
        return None, [], [], False

    match_info = {}
    events     = []
    lineups    = []
    player_team_map = {}

    # --- INFO BÀSICA ---
    for cls, key in [
        ("print-acta-temp", "season"),
        ("print-acta-comp", "competition"),
    ]:
        tag = soup.find("div", class_=cls)
        match_info[key] = tag.get_text(strip=True) if tag else None

    data_div = soup.find("div", class_="print-acta-data")
    if data_div:
        m = re.search(r"(\d{2}-\d{2}-\d{4}),\s+(\d{2}:\d{2})", data_div.get_text(strip=True))
        if m:
            match_info["date"] = m.group(1)
            match_info["time"] = m.group(2)

    match_info["jornada"] = jornada_num

    acta_head = soup.find("div", class_="acta-head")
    if acta_head:
        equips = acta_head.find_all("div", class_="acta-equip")
        if len(equips) >= 2:
            match_info["home_team"] = equips[0].get_text(strip=True)
            match_info["away_team"] = equips[1].get_text(strip=True)
        marcador = acta_head.find("div", class_="acta-marcador")
        if marcador:
            gols = marcador.get_text(strip=True).split("-")
            if len(gols) == 2:
                try:
                    match_info["goals_home"] = int(gols[0].strip())
                    match_info["goals_away"] = int(gols[1].strip())
                except ValueError:
                    pass

    # --- ALINEACIONS ---
    def parse_lineup_table(header_text: str, position_label: str):
        tables = soup.find_all("th", string=header_text)
        for idx, tbl in enumerate(tables):
            equip = match_info.get("home_team") if idx == 0 else match_info.get("away_team")
            tbody = tbl.find_parent("table").find("tbody")
            if not tbody:
                continue
            for row in tbody.find_all("tr"):
                cells = row.find_all("td")
                if len(cells) < 2:
                    continue
                num_span = cells[0].find("span", class_="num-samarreta-acta2")
                numero   = num_span.get_text(strip=True) if num_span else None
                link     = cells[1].find("a")
                jugador  = link.get_text(strip=True) if link else None
                if jugador:
                    player_team_map[jugador] = equip
                stats_list = []
                if len(cells) >= 3:
                    sd = cells[2].find("div", class_="acta-stats")
                    if sd:
                        gols = len(sd.find_all("div", class_="gol"))
                        if gols:
                            cpt = sd.find("div", class_="comptador")
                            stats_list.append(f"{int(cpt.get_text(strip=True)) if cpt else gols} gol(s)")
                        if sd.find("div", class_="groga-s"):   stats_list.append("Groga")
                        if sd.find("div", class_="vermella-s"):stats_list.append("Vermella")
                        if sd.find("div", class_="surt-s"):    stats_list.append("Substituït")
                        if sd.find("div", class_="entra-s"):   stats_list.append("Ha jugat")
                lineups.append({
                    "match_date": match_info.get("date"),
                    "jornada":    jornada_num,
                    "home_team":  match_info.get("home_team"),
                    "away_team":  match_info.get("away_team"),
                    "team":       equip,
                    "player":     jugador,
                    "shirt_number": numero,
                    "position":   position_label,
                    "stats":      ", ".join(stats_list) if stats_list else None,
                })

    parse_lineup_table("Titulars", "Titular")
    parse_lineup_table("Suplents", "Suplent")

    # --- GOLS ---
    gols_th = soup.find("th", string="Gols")
    if gols_th:
        tbody = gols_th.find_parent("table").find("tbody")
        if tbody:
            imgs_head = acta_head.find_all("img") if acta_head else []
            home_escut = imgs_head[0]["src"] if len(imgs_head) > 0 else None
            away_escut = imgs_head[1]["src"] if len(imgs_head) > 1 else None
            for row in tbody.find_all("tr"):
                cells = row.find_all("td")
                if len(cells) < 4:
                    continue
                minut_txt = cells[3].get_text(strip=True).replace("'", "")
                link      = cells[2].find("a")
                jugador   = link.get_text(strip=True) if link else None
                equip     = None
                escut_img = cells[1].find("img")
                if escut_img and "src" in escut_img.attrs:
                    src = escut_img["src"]
                    if home_escut and home_escut in src:
                        equip = match_info.get("home_team")
                    elif away_escut and away_escut in src:
                        equip = match_info.get("away_team")
                gol_div = cells[0].find("div", class_="gol")
                tipus = "Normal"
                if gol_div:
                    s = str(gol_div)
                    if "gol-penal"  in s: tipus = "Penal"
                    elif "gol-propia" in s: tipus = "Pròpia"
                events.append({
                    "match_date": match_info.get("date"),
                    "jornada":    jornada_num,
                    "home_team":  match_info.get("home_team"),
                    "away_team":  match_info.get("away_team"),
                    "event_type": "Gol",
                    "minute":     int(minut_txt) if minut_txt.isdigit() else None,
                    "team":       equip,
                    "player":     jugador,
                    "detail":     tipus,
                })

    # --- SUBSTITUCIONS ---
    for subst_th in soup.find_all("th", string="Substitucions"):
        tbody = subst_th.find_parent("table").find("tbody")
        if not tbody:
            continue
        rows = tbody.find_all("tr")
        i = 0
        while i < len(rows):
            row = rows[i]
            minut_cell = row.find("td", class_="fs-30")
            if minut_cell:
                minut_txt = minut_cell.get_text(strip=True).replace("'", "")
                minut     = int(minut_txt) if minut_txt.isdigit() else None
                cells     = row.find_all("td")
                jugador_surt = None
                if len(cells) >= 3:
                    l = cells[2].find("a")
                    jugador_surt = l.get_text(strip=True) if l else None
                equip = player_team_map.get(jugador_surt)
                if i + 1 < len(rows):
                    cells_entra = rows[i + 1].find_all("td")
                    jugador_entra = None
                    if len(cells_entra) >= 2:
                        l = cells_entra[1].find("a")
                        jugador_entra = l.get_text(strip=True) if l else None
                    events.append({
                        "match_date": match_info.get("date"),
                        "jornada":    jornada_num,
                        "home_team":  match_info.get("home_team"),
                        "away_team":  match_info.get("away_team"),
                        "event_type": "Substitució",
                        "minute":     minut,
                        "team":       equip,
                        "player":     jugador_entra,
                        "detail":     f"Entra per {jugador_surt}",
                    })
                i += 2
            else:
                i += 1

    # --- TARGETES ---
    for targ_th in soup.find_all("th", string="Targetes"):
        tbody = targ_th.find_parent("table").find("tbody")
        if not tbody:
            continue
        for row in tbody.find_all("tr"):
            cells = row.find_all("td")
            if len(cells) < 3:
                continue
            link    = cells[1].find("a")
            jugador = link.get_text(strip=True) if link else None
            minut_div = cells[2].find("div", class_="acta-minut-targeta")
            minut_txt = minut_div.get_text(strip=True).replace("'", "") if minut_div else None
            minut     = int(minut_txt) if minut_txt and minut_txt.isdigit() else None
            sd = cells[2].find("div", class_="acta-stats")
            tipus = None
            if sd:
                if sd.find("div", class_="vermella-s"): tipus = "Targeta Vermella"
                elif sd.find("div", class_="groga-s"):  tipus = "Targeta Groga"
            equip = player_team_map.get(jugador)
            if jugador and tipus:
                events.append({
                    "match_date": match_info.get("date"),
                    "jornada":    jornada_num,
                    "home_team":  match_info.get("home_team"),
                    "away_team":  match_info.get("away_team"),
                    "event_type": tipus,
                    "minute":     minut,
                    "team":       equip,
                    "player":     jugador,
                    "detail":     None,
                })

    return match_info, events, lineups, True


# ============================================================================
# MÒDUL 4 — ESTADÍSTIQUES DE JUGADORS I EQUIPS
# ============================================================================

def build_player_match_stats(lineups: pd.DataFrame, events: pd.DataFrame) -> pd.DataFrame:
    """Construeix player_match_stats a partir de lineups i events."""
    for df in [lineups, events]:
        df["match_id"] = (
            df["jornada"].astype(str) + "_"
            + df["home_team"].str[:3] + "_"
            + df["away_team"].str[:3]
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
    return df


def build_player_stats(player_match_stats: pd.DataFrame) -> pd.DataFrame:
    """Estadístiques agregades per jugador."""
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

    return agg[["player", "team", "matches_played", "starts", "total_minutes",
                "goals", "goals_per_90", "cards_per_90"]].sort_values("goals", ascending=False)


def build_team_match_stats(matches_info: pd.DataFrame,
                           player_match_stats: pd.DataFrame) -> pd.DataFrame:
    """Estadístiques per equip i partit."""
    matches_info = matches_info.copy()
    matches_info["match_id"] = (
        matches_info["jornada"].astype(str) + "_"
        + matches_info["home_team"].str[:3] + "_"
        + matches_info["away_team"].str[:3]
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
    return df


# ============================================================================
# PIPELINE PRINCIPAL PER UN GRUP
# ============================================================================

def process_grup(categoria: str, grup: int, output_dir: Path):
    """Executa el pipeline sencer per a un grup i guarda els CSVs."""
    output_dir.mkdir(parents=True, exist_ok=True)
    print(f"\n{'='*70}")
    print(f"  {categoria} — Grup {grup}")
    print(f"{'='*70}")

    # 1. Partits
    print("  1/6 Scraping partits...")
    df_matches = scrape_matches(categoria, grup)
    df_matches.to_csv(output_dir / "matches.csv", index=False)
    if df_matches.empty or "goals_home" not in df_matches.columns:
        print(f"     ⚠️  Cap partit obtingut per {categoria} Grup {grup} — saltant")
        return False
    print(f"     ✅ {len(df_matches)} partits ({df_matches['goals_home'].notna().sum()} jugats)")

    # 2. Classificació per jornada
    print("  2/6 Classificació per jornada...")
    df_standings = compute_standings_by_round(df_matches)
    df_standings.to_csv(output_dir / "standings_by_round.csv", index=False)
    print(f"     ✅ {len(df_standings)} files")

    # 3. Actes de partits
    print("  3/6 Scraping actes de partits...")
    matches_played = df_matches.dropna(subset=["goals_home", "goals_away"])
    all_match_info = []
    all_events     = []
    all_lineups    = []
    ok = err = 0
    total = len(matches_played)

    for i, (_, row) in enumerate(matches_played.iterrows(), 1):
        print(f"     [{i:3}/{total}] J{row['jornada']:2}: {row['local_team']} vs {row['away_team']}", end="")
        mi, ev, lu, success = scrape_match_acta(
            row["local_team"], row["away_team"], row["jornada"], categoria, grup
        )
        if success:
            all_match_info.append(mi)
            all_events.extend(ev)
            all_lineups.extend(lu)
            ok += 1
            print(" ✅")
        else:
            err += 1
            print(" ❌")
        time.sleep(SLEEP_BETWEEN_REQUESTS)

    df_match_info = pd.DataFrame(all_match_info)
    df_events     = pd.DataFrame(all_events)
    df_lineups    = pd.DataFrame(all_lineups)

    if not df_events.empty:
        df_events.sort_values(["jornada", "minute"], na_position="last", inplace=True)

    df_match_info.to_csv(output_dir / "all_matches_info.csv",    index=False)
    df_events.to_csv(    output_dir / "all_matches_events.csv",  index=False)
    df_lineups.to_csv(   output_dir / "all_matches_lineups.csv", index=False)
    print(f"     ✅ {ok} actes OK / ❌ {err} errors")

    # 4. Estadístiques de jugadors per partit
    print("  4/6 Estadístiques jugadors per partit...")
    df_pms = build_player_match_stats(df_lineups, df_events)
    df_pms.to_csv(output_dir / "player_match_stats.csv", index=False)

    # 5. Estadístiques agregades jugadors
    print("  5/6 Estadístiques agregades jugadors...")
    df_ps = build_player_stats(df_pms)
    df_ps.to_csv(output_dir / "player_stats.csv", index=False)

    # 6. Estadístiques per equip
    print("  6/6 Estadístiques per equip...")
    df_tms = build_team_match_stats(df_match_info, df_pms)
    df_tms.to_csv(output_dir / "team_match_stats.csv", index=False)

    print(f"  💾 Fitxers guardats a: {output_dir}")
    return True


# ============================================================================
# CONSOLIDACIÓ FINAL (equivalent a ajuntarTOT.ipynb)
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
    """Consolida tots els grups i categories en fitxers únics."""
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
                    df = pd.read_csv(path)
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
    parser = argparse.ArgumentParser(description="Scraper FCF — Futbol Català")
    parser.add_argument("--categoria", choices=list(CATEGORIES.keys()),
                        help="Processa només aquesta categoria")
    parser.add_argument("--grup", type=int,
                        help="Processa només aquest grup (requereix --categoria)")
    parser.add_argument("--output", default="dades",
                        help="Directori de sortida base (default: dades/)")
    args = parser.parse_args()

    base_output = Path(args.output)

    if args.categoria and args.grup:
        # Un sol grup
        out = base_output / args.categoria / f"GRUP{args.grup}"
        process_grup(args.categoria, args.grup, out)
    elif args.categoria:
        # Tots els grups d'una categoria
        for grup in range(1, CATEGORIES[args.categoria] + 1):
            out = base_output / args.categoria / f"GRUP{grup}"
            process_grup(args.categoria, grup, out)
    else:
        # Tot (totes les categories i grups)
        for categoria, num_grups in CATEGORIES.items():
            for grup in range(1, num_grups + 1):
                out = base_output / categoria / f"GRUP{grup}"
                process_grup(categoria, grup, out)

    # Consolidació final
    consolidar_tot(base_output, base_output)
    print("\n🎉 Scraping completat!")


if __name__ == "__main__":
    main()
