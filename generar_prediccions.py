"""
generar_prediccions.py
Equivalent Python del generar_prediccions.R
Simula 10.000 lligues per grup i puja els resultats a Supabase.
S'executa automàticament des del scraper.py al final de cada scraping complet.
"""

import pandas as pd
import numpy as np
from supabase import create_client, Client
from pathlib import Path
import os, math, warnings
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


def simular_grup(tms: pd.DataFrame, mts: pd.DataFrame, std: pd.DataFrame,
                 cat: str, grup: int) -> pd.DataFrame | None:
    teams   = sorted(tms["team"].unique())
    n_teams = len(teams)
    if n_teams < 4:
        return None

    # Classificació actual (darrera jornada)
    jornada_max = std["jornada"].max()
    cur = std[std["jornada"] == jornada_max][["team","points","goals_for","goals_against"]].copy()
    # Afegir equips que falten
    missing = set(teams) - set(cur["team"])
    if missing:
        extra = pd.DataFrame({"team": list(missing), "points": 0, "goals_for": 0, "goals_against": 0})
        cur = pd.concat([cur, extra], ignore_index=True)
    cur = cur.set_index("team")

    # Partits pendents
    pending = mts[mts["goals_home"].isna()][["local_team","away_team"]].values.tolist()
    n_pend  = len(pending)

    # Si ja acabada la lliga
    if n_pend == 0:
        cur_sorted = cur.sort_values(["points","goals_for"], ascending=False)
        cur_sorted["pos"] = range(1, len(cur_sorted)+1)
        return pd.DataFrame({
            "categoria":      cat,
            "grup":           grup,
            "team":           cur_sorted.index,
            "punts_actuals":  cur_sorted["points"].astype(int),
            "punts_esperats": cur_sorted["points"].round(2),
            "pos_esperada":   cur_sorted["pos"].astype(float),
            "prob_campió":    (cur_sorted["pos"] == 1).astype(float),
            "prob_top3":      (cur_sorted["pos"] <= N_TOP).astype(float),
            "prob_descens":   (cur_sorted["pos"] > n_teams - N_DESCENS).astype(float),
        })

    # Latents
    lat = estimate_latents(tms)
    global_avg = tms["goals_for"].mean()
    if not np.isfinite(global_avg) or global_avg <= 0:
        global_avg = 1.2

    def get_lat(team, col):
        try:
            v = lat.loc[team, col]
            return float(v) if np.isfinite(v) else 0.0
        except Exception:
            return 0.0

    # Acumuladors
    rng         = np.random.default_rng(42 + grup + len(cat))
    pos_counts  = {t: np.zeros(n_teams, dtype=int) for t in teams}
    pts_sums    = {t: 0.0 for t in teams}
    top3_counts = {t: 0   for t in teams}
    desc_counts = {t: 0   for t in teams}
    camp_counts = {t: 0   for t in teams}

    # Pre-calcular lambdes per partits pendents
    lambdes = []
    for h, a in pending:
        if h not in teams or a not in teams:
            lambdes.append(None); continue
        A_h = get_lat(h,"attack");  D_h = get_lat(h,"defense")
        A_a = get_lat(a,"attack");  D_a = get_lat(a,"defense")
        hb  = get_lat(h,"home_boost"); ap = get_lat(a,"away_pen")
        lh  = max(0.05, math.exp(math.log(global_avg) + A_h - D_a + hb))
        la  = max(0.05, math.exp(math.log(global_avg) + A_a - D_h - ap))
        lambdes.append((h, a, lh, la))

    # Monte Carlo
    for _ in range(N_SIMS):
        sim_pts = {t: int(cur.loc[t,"points"])       if t in cur.index else 0 for t in teams}
        sim_gf  = {t: int(cur.loc[t,"goals_for"])    if t in cur.index else 0 for t in teams}
        sim_gc  = {t: int(cur.loc[t,"goals_against"]) if t in cur.index else 0 for t in teams}

        for entry in lambdes:
            if entry is None: continue
            h, a, lh, la = entry
            gh = rng.poisson(lh)
            ga = rng.poisson(la)
            if gh > ga:
                sim_pts[h] += 3
            elif gh == ga:
                sim_pts[h] += 1; sim_pts[a] += 1
            else:
                sim_pts[a] += 3
            sim_gf[h] += gh; sim_gc[h] += ga
            sim_gf[a] += ga; sim_gc[a] += gh

        # Classificació simulada
        sim_df = sorted(teams, key=lambda t: (-sim_pts[t], -(sim_gf[t]-sim_gc[t]), -sim_gf[t]))
        for pos, t in enumerate(sim_df, 1):
            pos_counts[t][pos-1] += 1
            pts_sums[t]          += sim_pts[t]
            if pos <= N_TOP:                  top3_counts[t] += 1
            if pos > n_teams - N_DESCENS:     desc_counts[t] += 1
            if pos == 1:                      camp_counts[t] += 1

    # Resum
    posicions    = np.arange(1, n_teams+1)
    pos_esp      = {t: float(np.average(posicions, weights=pos_counts[t])) for t in teams}
    pts_esp      = {t: round(pts_sums[t]/N_SIMS, 2) for t in teams}
    prob_camp    = {t: round(camp_counts[t]/N_SIMS, 4) for t in teams}
    prob_top3    = {t: round(top3_counts[t]/N_SIMS, 4) for t in teams}
    prob_desc    = {t: round(desc_counts[t]/N_SIMS, 4) for t in teams}
    pts_act      = {t: int(cur.loc[t,"points"]) if t in cur.index else 0 for t in teams}

    df = pd.DataFrame({
        "categoria":      cat,
        "grup":           grup,
        "team":           teams,
        "punts_actuals":  [pts_act[t]   for t in teams],
        "punts_esperats": [pts_esp[t]   for t in teams],
        "pos_esperada":   [round(pos_esp[t],2) for t in teams],
        "prob_campió":    [prob_camp[t] for t in teams],
        "prob_top3":      [prob_top3[t] for t in teams],
        "prob_descens":   [prob_desc[t] for t in teams],
    }).sort_values("pos_esperada").reset_index(drop=True)

    return df


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
            f_tms     = grup_dir / "team_match_stats.csv"
            f_std     = grup_dir / "standings_by_round.csv"

            if not (f_matches.exists() and f_tms.exists() and f_std.exists()):
                continue

            mts = pd.read_csv(f_matches)
            tms = pd.read_csv(f_tms)
            std = pd.read_csv(f_std)

            if mts.empty or tms.empty or std.empty:
                continue

            # Normalitzar nom columna
            if "home_team" in mts.columns and "local_team" not in mts.columns:
                mts = mts.rename(columns={"home_team": "local_team"})

            print(f"  🔮 Simulant {cat} Grup {grup}...", end=" ")
            df = simular_grup(tms, mts, std, cat, grup)
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
