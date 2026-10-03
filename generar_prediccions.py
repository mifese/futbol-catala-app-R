"""
generar_prediccions.py
Equivalent Python del generar_prediccions.R
Simula 10.000 lligues per grup (vectoritzat amb numpy) i puja els resultats a Supabase.
S'executa automàticament des del scraper.py al final de cada scraping complet.
"""

import pandas as pd
import numpy as np
from supabase import create_client, Client
from pathlib import Path
import os, re, math, warnings
warnings.filterwarnings("ignore")

N_SIMS    = 10000
N_DESCENS = 4
N_TOP     = 3
CHUNK     = 500


def get_supabase() -> Client | None:
    url = os.environ.get("SUPABASE_URL")
    key = os.environ.get("SUPABASE_KEY")
    if not url or not key:
        return None
    return create_client(url, key)


def estimate_latents(tms: pd.DataFrame) -> pd.DataFrame:
    """Estima latents d'atac/defensa per equip (model Poisson jeràrquic)."""
    global_avg = tms[["goals_for", "goals_against"]].stack().mean()
    if not np.isfinite(global_avg) or global_avg <= 0:
        global_avg = 1.2

    grp = tms.groupby("team")
    agg = grp.agg(
        n        =("goals_for", "count"),
        avg_gf   =("goals_for", "mean"),
        avg_ga   =("goals_against", "mean"),
    ).reset_index()

    home  = tms[tms["home_away"] == "Home"].groupby("team").agg(avg_gf_h=("goals_for","mean"), avg_ga_h=("goals_against","mean")).reset_index()
    away  = tms[tms["home_away"] == "Away"].groupby("team").agg(avg_gf_a=("goals_for","mean"), avg_ga_a=("goals_against","mean")).reset_index()
    agg   = agg.merge(home, on="team", how="left").merge(away, on="team", how="left")

    agg["shrink"]     = agg["n"] / (agg["n"] + 6)
    agg["attack"]     = (np.log(agg["avg_gf"].clip(lower=0.01)) - math.log(global_avg)) * agg["shrink"]
    agg["defense"]    = -(np.log(agg["avg_ga"].clip(lower=0.01)) - math.log(global_avg)) * agg["shrink"]
    agg["avg_gf_h"]   = agg["avg_gf_h"].fillna(agg["avg_gf"])
    agg["avg_gf_a"]   = agg["avg_gf_a"].fillna(agg["avg_gf"])
    agg["avg_ga_h"]   = agg["avg_ga_h"].fillna(agg["avg_ga"])
    agg["avg_ga_a"]   = agg["avg_ga_a"].fillna(agg["avg_ga"])
    agg["home_boost"] = ((agg["avg_gf_h"] - agg["avg_gf_a"]) / 2 * agg["shrink"]).clip(lower=0)
    agg["away_pen"]   = ((agg["avg_ga_a"] - agg["avg_ga_h"]) / 2 * agg["shrink"]).clip(lower=0)
    return agg.set_index("team")


def simular_grup(mts: pd.DataFrame, cat: str, grup: int) -> pd.DataFrame | None:
    """Simula la lliga a partir NOMÉS de matches.csv (calendari complet amb
    resultats). Abans es creuaven tres fitxers amb noms d'equip diferents
    (standings/matches amb "... A", team_match_stats sense), així que cap
    equip coincidia: tot sortia amb 0 punts i cap partit pendent es simulava.
    """
    mts = mts.dropna(subset=["local_team", "away_team"]).copy()
    teams = sorted(set(mts["local_team"]) | set(mts["away_team"]))
    n_teams = len(teams)
    if n_teams < 4:
        return None
    idx = {t: i for i, t in enumerate(teams)}

    done = mts["goals_home"].notna() & mts["goals_away"].notna()
    played, pending = mts[done], mts[~done]

    # Classificació actual calculada dels partits jugats
    pts = np.zeros(n_teams); gf = np.zeros(n_teams); ga = np.zeros(n_teams)
    rows = []
    for r in played.itertuples():
        h, a = idx[r.local_team], idx[r.away_team]
        gh, gaw = float(r.goals_home), float(r.goals_away)
        gf[h] += gh; ga[h] += gaw; gf[a] += gaw; ga[a] += gh
        if gh > gaw: pts[h] += 3
        elif gh < gaw: pts[a] += 3
        else: pts[h] += 1; pts[a] += 1
        rows.append((r.local_team, "Home", gh, gaw))
        rows.append((r.away_team, "Away", gaw, gh))
    games = pd.DataFrame(rows, columns=["team", "home_away", "goals_for", "goals_against"])

    def _rank(score_pts, score_gd, score_gf, rng):
        # ordre: punts, diferència, gols a favor (+ soroll <1 per desempatar)
        sc = score_pts * 1e8 + (score_gd + 1000) * 1e4 + score_gf * 10
        if rng is not None:
            sc = sc + rng.random(sc.shape)
        order = np.argsort(-sc, axis=0)
        ranks = np.empty_like(order)
        np.put_along_axis(ranks, order, np.arange(1, n_teams + 1)[:, None] * np.ones((1, sc.shape[1]), dtype=int), axis=0)
        return ranks

    # Lliga acabada
    if len(pending) == 0:
        ranks = _rank(pts[:, None], (gf - ga)[:, None], gf[:, None], None)[:, 0]
        return pd.DataFrame({
            "categoria": cat, "grup": grup, "team": teams,
            "punts_actuals": pts.astype(int),
            "punts_esperats": pts.round(2),
            "pos_esperada": ranks.astype(float),
            "prob_campió": (ranks == 1).astype(float),
            "prob_top3": (ranks <= N_TOP).astype(float),
            "prob_descens": (ranks > n_teams - N_DESCENS).astype(float),
        }).sort_values("pos_esperada").reset_index(drop=True)

    lat = estimate_latents(games) if not games.empty else None
    global_avg = games[["goals_for", "goals_against"]].stack().mean() if not games.empty else 1.2
    if not np.isfinite(global_avg) or global_avg <= 0:
        global_avg = 1.2

    def get_lat(team, col):
        if lat is None or team not in lat.index:
            return 0.0
        v = lat.loc[team, col]
        return float(v) if np.isfinite(v) else 0.0

    rng = np.random.default_rng(42 + grup + len(cat))
    S = N_SIMS
    sim_pts = np.tile(pts[:, None], (1, S))
    sim_gd = np.tile((gf - ga)[:, None], (1, S))
    sim_gf = np.tile(gf[:, None], (1, S))

    # Simulació vectoritzada: cada partit pendent = un vector de N_SIMS gols
    for r in pending.itertuples():
        h, a = r.local_team, r.away_team
        lh = max(0.05, math.exp(math.log(global_avg) + get_lat(h, "attack") - get_lat(a, "defense") + get_lat(h, "home_boost")))
        la = max(0.05, math.exp(math.log(global_avg) + get_lat(a, "attack") - get_lat(h, "defense") - get_lat(a, "away_pen")))
        gh = rng.poisson(lh, S); gaw = rng.poisson(la, S)
        hi, ai = idx[h], idx[a]
        sim_pts[hi] += 3 * (gh > gaw) + (gh == gaw)
        sim_pts[ai] += 3 * (gh < gaw) + (gh == gaw)
        sim_gd[hi] += gh - gaw; sim_gd[ai] += gaw - gh
        sim_gf[hi] += gh;       sim_gf[ai] += gaw

    ranks = _rank(sim_pts, sim_gd, sim_gf, rng)

    return pd.DataFrame({
        "categoria": cat, "grup": grup, "team": teams,
        "punts_actuals": pts.astype(int),
        "punts_esperats": sim_pts.mean(axis=1).round(2),
        "pos_esperada": ranks.mean(axis=1).round(2),
        "prob_campió": (ranks == 1).mean(axis=1).round(4),
        "prob_top3": (ranks <= N_TOP).mean(axis=1).round(4),
        "prob_descens": (ranks > n_teams - N_DESCENS).mean(axis=1).round(4),
    }).sort_values("pos_esperada").reset_index(drop=True)


def pujar_prediccions(client: Client, df: pd.DataFrame, cat: str, grup: int):
    try:
        client.table("prediccions").delete().eq("categoria", cat).eq("grup", grup).execute()
        records = df.to_dict(orient="records")
        for i in range(0, len(records), CHUNK):
            client.table("prediccions").insert(records[i:i+CHUNK]).execute()
        print(f"    ✅ prediccions {cat} G{grup}: {len(records)} equips pujats")
    except Exception as e:
        print(f"    ❌ Error pujant prediccions {cat} G{grup}: {e}")


def generar_totes(base_dir: Path = Path("dades")):
    """Genera prediccions per tots els grups i les puja a Supabase."""
    client = get_supabase()

    CATEGORIES = {"TERCERA": 18, "SEGONA": 6, "PRIMERA": 3}

    for cat, n_grups in CATEGORIES.items():
        for grup in range(1, n_grups + 1):
            grup_dir = base_dir / cat / f"GRUP{grup}"
            f_matches = grup_dir / "matches.csv"
            if not f_matches.exists():
                continue
            mts = pd.read_csv(f_matches)
            if mts.empty:
                continue
            if "home_team" in mts.columns and "local_team" not in mts.columns:
                mts = mts.rename(columns={"home_team": "local_team"})

            print(f"  🔮 Simulant {cat} Grup {grup}...", end=" ")
            df = simular_grup(mts, cat, grup)
            if df is None:
                print("skip")
                continue
            print(f"{len(df)} equips")

            if client:
                pujar_prediccions(client, df, cat, grup)


if __name__ == "__main__":
    print("🔮 Generant prediccions...")
    generar_totes()
    print("✅ Fet!")
