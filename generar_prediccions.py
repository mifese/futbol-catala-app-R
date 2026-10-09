"""
generar_prediccions.py
======================
Simula 10.000 lligues per grup i puja les probabilitats a Supabase.

Canvis respecte a la versió anterior (detall i validació a METODE_RATINGS_PREDICCIONS.md):
  1. La força dels equips s'estima de manera CONJUNTA (Poisson jeràrquic, forca_equips.py):
     atac i defensa de cada equip es corregeixen pel rival que han tingut.
  2. L'encongiment cap a la mitjana ve d'un prior estimat amb les dades de la categoria
     (evidència marginal), no de la constant n/(n+6).
  3. Un únic avantatge de camp per categoria (abans: un per equip, truncat a >= 0, que
     només podia ser positiu i es calculava amb 1-3 partits).
  4. S'hi propaga la INCERTESA de la força: cada simulació extreu els paràmetres de la
     posterior. Abans s'usaven com si fossin exactes: probabilitats massa extremes.
  5. Sortida ampliada (rang 10-90 % de posició i punts, rating d'equip) amb pujada
     compatible: si la taula no té les columnes noves, es puja el format antic.
"""
import math
import os
import warnings
from pathlib import Path

import numpy as np
import pandas as pd
from supabase import Client, create_client

import forca_equips as fe

warnings.filterwarnings("ignore")

N_SIMS = 10000
N_DESCENS = 4          # revisa-ho amb el reglament de cada categoria
N_TOP = 3
CHUNK = 500
SIGMA_PER_DEFECTE = 0.28
MIN_PARTITS_SIGMA = 40   # partits jugats (de la categoria) a partir dels quals s'estima sigma

COLUMNES_BASE = ["categoria", "grup", "team", "punts_actuals", "punts_esperats", "pos_esperada",
                 "prob_campió", "prob_top3", "prob_descens"]


def get_supabase() -> Client | None:
    url = os.environ.get("SUPABASE_URL")
    key = os.environ.get("SUPABASE_KEY")
    if not url or not key:
        return None
    return create_client(url, key)


def _partits_jugats(mts: pd.DataFrame):
    d = mts.dropna(subset=["local_team", "away_team", "goals_home", "goals_away"])
    return [(r.local_team, r.away_team, float(r.goals_home), float(r.goals_away)) for r in d.itertuples()]


def hiperparametres(grups_cat: dict) -> tuple:
    """(h, sigma) d'una categoria a partir de TOTS els seus grups (dict grup -> matches df)."""
    dades = []
    for g, mts in grups_cat.items():
        mts = mts.dropna(subset=["local_team", "away_team"])
        eq = sorted(set(mts["local_team"]) | set(mts["away_team"]))
        dades.append((_partits_jugats(mts), eq))
    jugats = [d[0] for d in dades if d[0]]
    if not jugats:
        return fe.H_PER_DEFECTE, SIGMA_PER_DEFECTE
    h = fe.h_categoria(jugats)
    h = float(np.clip(h, 0.0, 0.45))
    if sum(len(j) for j in jugats) < MIN_PARTITS_SIGMA:
        return h, SIGMA_PER_DEFECTE
    try:
        sigma, _ = fe.estimar_sigma(dades, (h, fe.TAU_H))
    except Exception:
        sigma = SIGMA_PER_DEFECTE
    # L'evidència amb poques jornades és plana: no deixem que s'allunyi massa del valor validat.
    return h, float(np.clip(sigma, 0.18, 0.45))


def simular_grup(mts: pd.DataFrame, cat: str, grup: int, h=None, sigma=None, n_sims=N_SIMS):
    """Simula la lliga a partir NOMÉS de matches.csv (calendari complet amb resultats)."""
    mts = mts.dropna(subset=["local_team", "away_team"]).copy()
    teams = sorted(set(mts["local_team"]) | set(mts["away_team"]))
    n_teams = len(teams)
    if n_teams < 4:
        return None
    idx = {t: i for i, t in enumerate(teams)}
    h = fe.H_PER_DEFECTE if h is None else h
    sigma = SIGMA_PER_DEFECTE if sigma is None else sigma

    done = mts["goals_home"].notna() & mts["goals_away"].notna()
    played, pending = mts[done], mts[~done]

    pts = np.zeros(n_teams); gf = np.zeros(n_teams); ga = np.zeros(n_teams)
    for r in played.itertuples():
        i, j = idx[r.local_team], idx[r.away_team]
        gh, gaw = float(r.goals_home), float(r.goals_away)
        gf[i] += gh; ga[i] += gaw; gf[j] += gaw; ga[j] += gh
        if gh > gaw: pts[i] += 3
        elif gh < gaw: pts[j] += 3
        else: pts[i] += 1; pts[j] += 1

    def _rank(score_pts, score_gd, score_gf, rng):
        # ordre: punts, diferència, gols a favor (+ soroll < 1 per desempatar)
        sc = score_pts * 1e8 + (score_gd + 1000) * 1e4 + score_gf * 10
        if rng is not None:
            sc = sc + rng.random(sc.shape)
        order = np.argsort(-sc, axis=0)
        ranks = np.empty_like(order)
        np.put_along_axis(ranks, order, np.arange(1, n_teams + 1)[:, None] * np.ones((1, sc.shape[1]), dtype=int), axis=0)
        return ranks

    aj = fe.ajustar(_partits_jugats(mts), sigma, sigma, h_prior=(h, fe.TAU_H), equips=teams)
    rating = fe.rating_equips(aj)

    if len(pending) == 0:
        ranks = _rank(pts[:, None], (gf - ga)[:, None], gf[:, None], None)[:, 0]
        return pd.DataFrame({
            "categoria": cat, "grup": grup, "team": teams,
            "punts_actuals": pts.astype(int), "punts_esperats": pts.round(2),
            "pos_esperada": ranks.astype(float),
            "prob_campió": (ranks == 1).astype(float),
            "prob_top3": (ranks <= N_TOP).astype(float),
            "prob_descens": (ranks > n_teams - N_DESCENS).astype(float),
            "pos_p10": ranks.astype(int), "pos_p90": ranks.astype(int),
            "punts_p10": pts.astype(int), "punts_p90": pts.astype(int),
            "rating": [rating[t] for t in teams],
        }).sort_values("pos_esperada").reset_index(drop=True)

    rng = np.random.default_rng(42 + grup + len(cat))
    S = n_sims
    n = aj.n
    TH = aj.mostrejar(S, rng)                          # (S, 2+2n): incertesa de la força
    mu, hh = TH[:, 0], TH[:, 1]
    A, D = TH[:, 2:2 + n], TH[:, 2 + n:]
    sim_pts = np.tile(pts[:, None], (1, S))
    sim_gd = np.tile((gf - ga)[:, None], (1, S))
    sim_gf = np.tile(gf[:, None], (1, S))

    for r in pending.itertuples():
        hi, ai = idx[r.local_team], idx[r.away_team]
        lh = np.exp(np.clip(mu + hh + A[:, hi] - D[:, ai], -4, 2.5))
        la = np.exp(np.clip(mu + A[:, ai] - D[:, hi], -4, 2.5))
        gh = rng.poisson(lh); gaw = rng.poisson(la)
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
        "pos_p10": np.percentile(ranks, 10, axis=1).astype(int),
        "pos_p90": np.percentile(ranks, 90, axis=1).astype(int),
        "punts_p10": np.percentile(sim_pts, 10, axis=1).astype(int),
        "punts_p90": np.percentile(sim_pts, 90, axis=1).astype(int),
        "rating": [rating[t] for t in teams],
    }).sort_values("pos_esperada").reset_index(drop=True)


def _insertar(client, records):
    for i in range(0, len(records), CHUNK):
        client.table("prediccions").insert(records[i:i + CHUNK]).execute()


def pujar_prediccions(client: Client, df: pd.DataFrame, cat: str, grup: int):
    records = df.to_dict(orient="records")
    try:
        client.table("prediccions").delete().eq("categoria", cat).eq("grup", grup).execute()
        try:
            _insertar(client, records)                              # format ampliat
        except Exception:
            # La taula encara no té les columnes noves (supabase_prediccions_v2.sql): format antic.
            client.table("prediccions").delete().eq("categoria", cat).eq("grup", grup).execute()
            _insertar(client, [{k: r[k] for k in COLUMNES_BASE} for r in records])
        print(f"    ✅ prediccions {cat} G{grup}: {len(records)} equips pujats")
    except Exception as e:
        print(f"    ❌ Error pujant prediccions {cat} G{grup}: {e}")


def generar_totes(base_dir: Path = Path("dades")):
    """Genera prediccions per tots els grups i les puja a Supabase."""
    client = get_supabase()
    CATEGORIES = {"TERCERA": 18, "SEGONA": 6, "PRIMERA": 3}

    for cat, n_grups in CATEGORIES.items():
        grups = {}
        for grup in range(1, n_grups + 1):
            f_matches = base_dir / cat / f"GRUP{grup}" / "matches.csv"
            if not f_matches.exists():
                continue
            mts = pd.read_csv(f_matches)
            if mts.empty:
                continue
            if "home_team" in mts.columns and "local_team" not in mts.columns:
                mts = mts.rename(columns={"home_team": "local_team"})
            grups[grup] = mts
        if not grups:
            continue
        h, sigma = hiperparametres(grups)
        print(f"  📐 {cat}: avantatge de camp h={h:.3f} (x{math.exp(h):.2f} gols) · sigma={sigma:.2f}")
        for grup, mts in grups.items():
            print(f"  🔮 Simulant {cat} Grup {grup}...", end=" ")
            df = simular_grup(mts, cat, grup, h=h, sigma=sigma)
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
