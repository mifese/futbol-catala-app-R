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
  5. Normativa 2026/27 (normativa.py): ascens directe, play-off (inclosos millors segons/tercers entre grups)
     i descens per territorial, simulant tots els grups d'una categoria a la vegada.
  6. Sortida ampliada (rang 10-90 % de posició i punts, rating d'equip) amb pujada
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
import normativa as nrm

warnings.filterwarnings("ignore")

N_SIMS = 10000
N_TOP = 3              # només per a la columna prob_top3 (la normativa viu a normativa.py)
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


def _simular_taula(mts: pd.DataFrame, h, sigma, S, rng):
    """Simula la taula final d'un grup (S lligues). Retorna arrays (n_equips, S) i els paràmetres mostrejats."""
    mts = mts.dropna(subset=["local_team", "away_team"]).copy()
    teams = sorted(set(mts["local_team"]) | set(mts["away_team"]))
    n = len(teams)
    idx = {t: i for i, t in enumerate(teams)}
    done = mts["goals_home"].notna() & mts["goals_away"].notna()
    played, pending = mts[done], mts[~done]

    pts = np.zeros(n); gf = np.zeros(n); ga = np.zeros(n)
    for r in played.itertuples():
        i, j = idx[r.local_team], idx[r.away_team]
        gh, gaw = float(r.goals_home), float(r.goals_away)
        gf[i] += gh; ga[i] += gaw; gf[j] += gaw; ga[j] += gh
        if gh > gaw: pts[i] += 3
        elif gh < gaw: pts[j] += 3
        else: pts[i] += 1; pts[j] += 1

    aj = fe.ajustar(_partits_jugats(mts), sigma, sigma, h_prior=(h, fe.TAU_H), equips=teams)
    TH = aj.mostrejar(S, rng)                          # incertesa de la força
    mu, hh = TH[:, 0], TH[:, 1]
    A, D = TH[:, 2:2 + n].T.copy(), TH[:, 2 + n:].T.copy()
    sim_pts = np.tile(pts[:, None], (1, S)); sim_gd = np.tile((gf - ga)[:, None], (1, S))
    sim_gf = np.tile(gf[:, None], (1, S))
    for r in pending.itertuples():
        hi, ai = idx[r.local_team], idx[r.away_team]
        lh = np.exp(np.clip(mu + hh + A[hi] - D[ai], -4, 2.5))
        la = np.exp(np.clip(mu + A[ai] - D[hi], -4, 2.5))
        gh = rng.poisson(lh); gaw = rng.poisson(la)
        sim_pts[hi] += 3 * (gh > gaw) + (gh == gaw)
        sim_pts[ai] += 3 * (gh < gaw) + (gh == gaw)
        sim_gd[hi] += gh - gaw; sim_gd[ai] += gaw - gh
        sim_gf[hi] += gh;       sim_gf[ai] += gaw
    # ordre: punts, diferència, gols a favor (+ soroll < 1 per desempatar)
    sc = sim_pts * 1e8 + (sim_gd + 1000) * 1e4 + sim_gf * 10 + rng.random(sim_pts.shape)
    order = np.argsort(-sc, axis=0)                    # order[r] = equip amb posició r+1
    ranks = np.empty_like(order)
    np.put_along_axis(ranks, order, np.arange(1, n + 1)[:, None] * np.ones((1, S), dtype=int), axis=0)
    return dict(teams=teams, pts0=pts, aj=aj, mu=mu, hh=hh, A=A, D=D, pts=sim_pts, gd=sim_gd,
                ranks=ranks, order=order, rating=fe.rating_equips(aj))


def simular_categoria(grups: dict, cat: str, h=None, sigma=None, n_sims=N_SIMS):
    """
    Simula TOTS els grups d'una categoria alhora (mateixa simulació = mateixa temporada) i aplica la
    normativa d'ascens / play-off / descens (normativa.py), incloent les comparacions entre grups
    (millors segons i tercers, pitjors 12ns de Barcelona). Retorna {grup: DataFrame}.
    """
    h = fe.H_PER_DEFECTE if h is None else h
    sigma = SIGMA_PER_DEFECTE if sigma is None else sigma
    S = n_sims
    rng = np.random.default_rng(42 + len(cat))
    cols = np.arange(S)
    sims = {}
    for g in sorted(grups):
        t = _simular_taula(grups[g], h, sigma, S, rng)
        if len(t["teams"]) >= 4:
            sims[g] = t
    if not sims:
        return {}
    G = sorted(sims)
    sizes = [len(sims[g]["teams"]) for g in G]
    off = dict(zip(G, np.concatenate([[0], np.cumsum(sizes)[:-1]]).astype(int)))
    N = sum(sizes)

    pts_all = np.vstack([sims[g]["pts"] for g in G]); gd_all = np.vstack([sims[g]["gd"] for g in G])
    rank_all = np.vstack([sims[g]["ranks"] for g in G])
    A_all = np.vstack([sims[g]["A"] for g in G]); D_all = np.vstack([sims[g]["D"] for g in G])
    mu_cat = np.mean([sims[g]["mu"] for g in G], axis=0); h_cat = np.mean([sims[g]["hh"] for g in G], axis=0)
    grup_de = np.concatenate([[g] * len(sims[g]["teams"]) for g in G])
    # clau per comparar equips de grups diferents (mateix lloc): punts, diferència de gols, sorteig
    key_all = pts_all * 1e6 + (gd_all + 1000) * 1e2 + rng.random((N, S)) * 50
    seed_all = rank_all * 1e12 - key_all                # més petit = millor classificat

    def at_rank(r):                                     # (G, S) índexs globals de l'equip que és r-èsim
        return np.vstack([off[g] + sims[g]["order"][r - 1][None, :] for g in G])

    def leg(home, away):
        lh = np.exp(np.clip(mu_cat + h_cat + A_all[home, cols] - D_all[away, cols], -4, 2.5))
        la = np.exp(np.clip(mu_cat + A_all[away, cols] - D_all[home, cols], -4, 2.5))
        return rng.poisson(lh), rng.poisson(la)

    def eliminatoria(x, y):
        """Doble partit; la tornada, al camp del millor classificat. Empat global -> penals 50/50."""
        a_better = seed_all[x, cols] <= seed_all[y, cols]
        a = np.where(a_better, x, y); b = np.where(a_better, y, x)
        g1b, g1a = leg(b, a)
        g2a, g2b = leg(a, b)
        ta, tb = g1a + g2a, g1b + g2b
        gana = (ta > tb) | ((ta == tb) & (rng.random(S) < 0.5))
        return np.where(gana, a, b)

    def sorteig(equips):                                # (k, S) -> mateixes files barrejades per simulació
        perm = np.argsort(rng.random(equips.shape), axis=0)
        return np.take_along_axis(equips, perm, axis=0)

    def millors(idx_gs, k):                             # (G,S) candidats -> (k,S) els k millors per clau
        key = key_all[idx_gs, cols[None, :]]
        sel = np.argsort(-key, axis=0)[:k]
        return np.take_along_axis(idx_gs, sel, axis=0)

    entra = np.zeros((N, S), dtype=bool)               # participa al play-off
    puja_po = np.zeros((N, S), dtype=bool)             # puja pel play-off

    def marca(mask, idxs):
        for row in np.atleast_2d(idxs):
            mask[row, cols] = True

    if cat == "PRIMERA":
        seg = at_rank(2); ter = millors(at_rank(3), 1)
        part = np.vstack([seg, ter]); marca(entra, part)
        p = sorteig(part)
        w1, w2 = eliminatoria(p[0], p[1]), eliminatoria(p[2], p[3])
        marca(puja_po, eliminatoria(w1, w2))
    elif cat == "SEGONA":
        for gi, g in enumerate(G):
            r2, r3, r4, r5 = (at_rank(r)[gi] for r in (2, 3, 4, 5))
            marca(entra, np.vstack([r2, r3, r4, r5]))
            marca(puja_po, eliminatoria(eliminatoria(r2, r5), eliminatoria(r3, r4)))
    else:  # TERCERA
        seg = millors(at_rank(2), 6); ter = millors(at_rank(3), 6)
        marca(entra, np.vstack([seg, ter]))
        def barreja(x):
            return np.take_along_axis(x, np.argsort(rng.random(x.shape), axis=0), axis=0)
        ter_p = barreja(ter)
        for _ in range(100):                             # aparellament: cap tercer-tercer ni mateix grup
            confl = (grup_de[seg] == grup_de[ter_p]).any(axis=0)
            if not confl.any():
                break
            ter_p = np.where(confl[None, :], barreja(ter), ter_p)
        guanyadors = np.vstack([eliminatoria(seg[i], ter_p[i]) for i in range(6)])
        for _ in range(nrm.TERCERA_RONDES - 1):          # rondes addicionals (parelles de guanyadors)
            guanyadors = np.vstack([eliminatoria(guanyadors[2 * i], guanyadors[2 * i + 1])
                                    for i in range(len(guanyadors) // 2)])
        marca(puja_po, guanyadors)

    # descens
    desc = np.zeros((N, S), dtype=bool)
    for g in G:
        n_g = len(sims[g]["teams"])
        nd = nrm.n_descens_directes(cat, g)
        desc[off[g]:off[g] + n_g] = rank_all[off[g]:off[g] + n_g] > n_g - nd
    bcn = [gi for gi, g in enumerate(G) if nrm.descens_12_barcelona(cat, g)]
    if bcn:
        r12 = at_rank(12)[bcn]                           # el 12è de cada grup de Barcelona
        key12 = key_all[r12, cols[None, :]]
        pitjors = np.take_along_axis(r12, np.argsort(key12, axis=0)[:nrm.N_PIRORS_12_BARCELONA], axis=0)
        marca(desc, pitjors)

    out = {}
    for g in G:
        t = sims[g]; n_g = len(t["teams"]); sl = slice(off[g], off[g] + n_g)
        rk = t["ranks"]; sp = t["pts"]
        p_camp = (rk == 1).mean(axis=1); p_po = entra[sl].mean(axis=1); p_pup = puja_po[sl].mean(axis=1)
        df = pd.DataFrame({
            "categoria": cat, "grup": g, "team": t["teams"],
            "punts_actuals": t["pts0"].astype(int),
            "punts_esperats": sp.mean(axis=1).round(2),
            "pos_esperada": rk.mean(axis=1).round(2),
            "prob_campió": p_camp.round(4),
            "prob_top3": (rk <= N_TOP).mean(axis=1).round(4),
            "prob_descens": desc[sl].mean(axis=1).round(4),
            "pos_p10": np.percentile(rk, 10, axis=1).astype(int),
            "pos_p90": np.percentile(rk, 90, axis=1).astype(int),
            "punts_p10": np.percentile(sp, 10, axis=1).astype(int),
            "punts_p90": np.percentile(sp, 90, axis=1).astype(int),
            "rating": [t["rating"][e] for e in t["teams"]],
            "prob_playoff": p_po.round(4),
            "prob_ascens": (p_camp + p_pup).round(4),
        }).sort_values("pos_esperada").reset_index(drop=True)
        out[g] = df
    return out


def _insertar(client, records):
    for i in range(0, len(records), CHUNK):
        client.table("prediccions").insert(records[i:i + CHUNK]).execute()


COLUMNES_V2 = COLUMNES_BASE + ["pos_p10", "pos_p90", "punts_p10", "punts_p90", "rating"]
COLUMNES_V3 = COLUMNES_V2 + ["prob_playoff", "prob_ascens"]


def pujar_prediccions(client: Client, df: pd.DataFrame, cat: str, grup: int):
    records = df.to_dict(orient="records")
    try:
        # Prova el format més ampli i va baixant si la taula encara no té les columnes noves
        # (supabase_prediccions_v2.sql / v3.sql).
        for cols in (COLUMNES_V3, COLUMNES_V2, COLUMNES_BASE):
            client.table("prediccions").delete().eq("categoria", cat).eq("grup", grup).execute()
            try:
                _insertar(client, [{k: r[k] for k in cols} for r in records])
                break
            except Exception as e:
                if cols is COLUMNES_BASE:
                    raise
                print(f"    ⚠️ format {len(cols)} columnes no acceptat ({str(e)[:60]}); provant el següent")
        print(f"    ✅ prediccions {cat} G{grup}: {len(records)} equips pujats ({len(cols)} columnes)")
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
        print(f"  🔮 Simulant {cat} ({len(grups)} grups, ascens/play-off/descens segons normativa 26/27)...")
        resultats = simular_categoria(grups, cat, h=h, sigma=sigma)
        for grup, df in resultats.items():
            print(f"    {cat} Grup {grup}: {len(df)} equips")
            if client:
                pujar_prediccions(client, df, cat, grup)


if __name__ == "__main__":
    print("🔮 Generant prediccions...")
    generar_totes()
    print("✅ Fet!")
