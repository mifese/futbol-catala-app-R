"""
calcular_posicions.py
=====================
Estima la POSICIÓ (POR / DEF / MIG / DAV) de cada jugador i la FORMACIÓ de cada
equip a cada partit, a partir de les alineacions de les actes de la FCF.

La FCF no publica la posició dels jugadors; només dorsal, titular/suplent,
minuts, gols i targetes. El mètode (detall a METODE_POSICIONS.md) combina:

  1. PORTER per estructura de l'acta: el primer titular de cada equip a l'acta
     és el porter (a les dades de Tercera: 99,6 % dels "dorsal 1" i 92 % dels
     "dorsal 13" són el primer de la llista). Això és una etiqueta gairebé
     real, sense haver d'endevinar res.
  2. PRIOR PER DORSAL: convenció històrica (1928, esquema 2-3-5 / WM) per als
     dorsals 1-11; la resta de dorsals aprenen la seva distribució de les
     dades (EM), perquè als equips amateurs el 14-25 s'assignen sense criteri.
  3. EVIDÈNCIA ESTADÍSTICA: gols/90 i targetes/90 (Poisson amb exposició =
     minuts). Els gols separen molt bé DAV (0,37) de MIG (≈0,10) i DEF (≈0,05).
  4. FORMACIÓ VÀLIDA PER PARTIT: 10 jugadors de camp s'han de repartir segons
     una formació real (4-4-2, 4-3-3, 5-3-2…). Es tria la combinació de rols +
     formació de màxima versemblança (programació dinàmica), amb un prior de
     formació per equip aprés en una primera passada.
  5. AGREGACIÓ: la posició final = barreja del posterior individual i dels rols que
     ha tingut als partits, amb pes w = n/(n+1) per a n titularitats.

Sortides (CSV + Supabase si hi ha credencials):
  player_positions  → una fila per jugador (posició, probabilitats, fiabilitat)
  match_roles       → una fila per titular i partit (rol al partit + formació)

Ús:
    python calcular_posicions.py                  # llegeix dades/, escriu CSV i puja
    python calcular_posicions.py --no-upload      # només CSV
    python calcular_posicions.py --informe        # imprimeix diagnòstic del model
    python calcular_posicions.py --exportar-mostra 80
    python calcular_posicions.py --validar mostra_validacio.csv
"""

from __future__ import annotations

import argparse
import math
import os
import re
from pathlib import Path

import numpy as np
import pandas as pd

# ----------------------------------------------------------------------------
# Constants
# ----------------------------------------------------------------------------
CLASSES = ["POR", "DEF", "MIG", "DAV"]
OUT = ["DEF", "MIG", "DAV"]
PLACEHOLDER_NAMES = {"jugador/a", "jugador", "jugadora"}   # l'acta en posa quan no hi ha nom

# Convenció de dorsals (DEF, MIG, DAV) per a jugadors de camp. Font: origen de la
# numeració per posició (Arsenal/Chelsea 1928, esquema 2-3-5 → WM) i ús habitual
# a Espanya: 2-3 laterals, 4-5 centrals, 6 pivot, 7/11 extrems, 8 interior,
# 9 davanter centre, 10 mitjapunta/segon davanter. Només és el PUNT DE PARTIDA:
# l'EM l'ajusta amb les dades reals.
DORSAL_CONVENCIO = {
    2: (.88, .10, .02), 3: (.88, .10, .02),
    4: (.85, .13, .02), 5: (.85, .13, .02),
    6: (.40, .55, .05),
    7: (.04, .46, .50),
    8: (.04, .86, .10),
    9: (.01, .06, .93),
    10: (.01, .50, .49),
    11: (.04, .40, .56),
}
GLOBAL_OUT_PRIOR = np.array([.38, .35, .27])        # marginal de camp si no coneixem el dorsal

# Taxes inicials (per 90') de gols i targetes per classe; l'EM les reestima.
LAMBDA0 = {"POR": .003, "DEF": .05, "MIG": .12, "DAV": .35}
KAPPA0  = {"POR": .07,  "DEF": .27, "MIG": .26, "DAV": .21}
PRIOR_EXPOSICIO = 12.0       # "pes" del prior Gamma en unitats de 90' (≈ 12 partits)
PRIOR_DORSAL = 150.0          # pseudocomptes del prior de dorsal en l'M-step
APRENDRE_DORSALS_MIN_MINUTS = 450
P_ROW0_GK, P_ROW0_OUT = 0.985, 0.005   # P(1r a l'acta | porter) i | jugador de camp)

# Formacions (DEF, MIG, DAV) amb log-prior. 4-2-3-1 i 4-1-4-1 compten com 4-5-1.
FORMACIONS = {
    (4, 4, 2): 0.0, (4, 3, 3): -0.25, (4, 5, 1): -0.45,
    (3, 5, 2): -1.1, (5, 3, 2): -1.2, (5, 4, 1): -1.2, (3, 4, 3): -1.4,
    (4, 2, 4): -2.5,
}
# Quan hi ha exactament 10 jugadors de camp NOMÉS s'accepten aquestes formacions
# (mai 1-7-2, 6-3-1, etc.). Si l'acta està incompleta (≠10 de camp) es fa servir
# una restricció més laxa: 3-6 defenses, 2-5 migcampistes, 1-4 davanters.
LOG_FORMACIO_DESCONEGUDA = -3.5
MAX_D, MAX_M = 6, 7


# ----------------------------------------------------------------------------
# Càrrega i preparació
# ----------------------------------------------------------------------------
def carregar_lineups(base_dir: Path) -> pd.DataFrame:
    """Llegeix tots els all_matches_lineups.csv de dades/<CAT>/GRUP<n>/."""
    frames = []
    for path in sorted(Path(base_dir).glob("*/GRUP*/all_matches_lineups.csv")):
        try:
            df = pd.read_csv(path)
        except pd.errors.EmptyDataError:
            continue
        if not df.empty:
            frames.append(df)
    if not frames:
        # Sense carpetes per grup: es prova amb el CSV consolidat (p. ex. el que
        # es baixa de l'artefacte "dades-consolidades-N" de GitHub Actions).
        for path in sorted(Path(base_dir).glob("consolidat_*all_matches_lineups.csv")):
            try:
                df = pd.read_csv(path)
            except pd.errors.EmptyDataError:
                continue
            if not df.empty:
                frames.append(df)
    if not frames:
        return pd.DataFrame()
    return pd.concat(frames, ignore_index=True)


def preparar(lineups: pd.DataFrame) -> pd.DataFrame:
    df = lineups.copy()
    for c in ["minutes_played", "goals", "yellow_cards", "red_cards"]:
        df[c] = pd.to_numeric(df.get(c, 0), errors="coerce").fillna(0)
    df["shirt_number"] = pd.to_numeric(df.get("shirt_number"), errors="coerce")
    keys = ["categoria", "grup", "jornada", "home_team", "away_team", "team", "position"]
    if "ordre" not in df.columns or df["ordre"].isna().all():
        # L'ordre de fila del CSV és l'ordre de l'acta (el scraper el conserva).
        df["ordre"] = df.groupby(keys).cumcount()
    df["ordre"] = pd.to_numeric(df["ordre"], errors="coerce").fillna(0).astype(int)
    df["titular"] = (df["position"] == "Titular")
    df["row0"] = df["titular"] & (df["ordre"] == 0)
    df["cards"] = df["yellow_cards"] + df["red_cards"]
    df["placeholder"] = df["player"].astype(str).str.strip().str.lower().isin(PLACEHOLDER_NAMES)
    return df


# ----------------------------------------------------------------------------
# Resum per jugador
# ----------------------------------------------------------------------------
def resum_jugadors(df: pd.DataFrame) -> pd.DataFrame:
    g = df[~df["placeholder"]].groupby(["categoria", "grup", "team", "player"], sort=False)
    p = g.agg(
        n_partits=("jornada", "size"),
        n_titular=("titular", "sum"),
        n_row0=("row0", "sum"),
        minuts=("minutes_played", "sum"),
        gols=("goals", "sum"),
        cards=("cards", "sum"),
        dorsal=("shirt_number", lambda s: s.mode().iloc[0] if s.notna().any() else np.nan),
    ).reset_index()
    p["exposicio"] = p["minuts"] / 90.0
    return p


# ----------------------------------------------------------------------------
# Model (stage 1): posterior individual per jugador + EM
# ----------------------------------------------------------------------------
def _prior_dorsal_init(df: pd.DataFrame) -> dict:
    """π_d(c) inicial: GK a partir de l'ordre d'acta, camp a partir de la convenció."""
    t = df[df["titular"] & df["shirt_number"].notna()]
    glob_gk = float(t["row0"].mean()) if len(t) else .09
    by_d = t.groupby("shirt_number")["row0"].agg(["sum", "size"])
    prior = {}
    for d in range(0, 100):
        if d in by_d.index:
            s, n = by_d.loc[d]
            gk = (s + 5 * glob_gk) / (n + 5)
        else:
            gk = glob_gk
        out = np.array(DORSAL_CONVENCIO.get(d, GLOBAL_OUT_PRIOR), dtype=float)
        prior[d] = np.concatenate([[gk], (1 - gk) * out])
    prior["_na"] = np.concatenate([[glob_gk], (1 - glob_gk) * GLOBAL_OUT_PRIOR])
    return prior


def _loglik(p: pd.DataFrame, prior: dict, lam: dict, kap: dict, ignorar_dorsal=False) -> np.ndarray:
    """log P(dades jugador | classe) + log prior, matriu (n, 4)."""
    n = len(p)
    out = np.zeros((n, 4))
    e = p["exposicio"].to_numpy() + 1e-3
    G = p["gols"].to_numpy(); C = p["cards"].to_numpy()
    S = p["n_titular"].to_numpy(); R0 = p["n_row0"].to_numpy()
    for j, c in enumerate(CLASSES):
        out[:, j] += G * np.log(lam[c] * e) - lam[c] * e
        out[:, j] += C * np.log(kap[c] * e) - kap[c] * e
    # evidència d'acta: el porter és el 1r de la llista
    a, b = P_ROW0_GK, P_ROW0_OUT
    out[:, 0] += R0 * math.log(a) + (S - R0) * math.log(1 - a)
    for j in (1, 2, 3):
        out[:, j] += R0 * math.log(b) + (S - R0) * math.log(1 - b)
    if not ignorar_dorsal:
        for i, d in enumerate(p["dorsal"].to_numpy()):
            pr = prior["_na"] if (isinstance(d, float) and math.isnan(d)) or int(d) not in prior else prior[int(d)]
            out[i] += np.log(np.clip(pr, 1e-6, None))
    else:
        out += np.log(np.array([.09, .91 * GLOBAL_OUT_PRIOR[0], .91 * GLOBAL_OUT_PRIOR[1], .91 * GLOBAL_OUT_PRIOR[2]]))
    return out


def _softmax(x: np.ndarray) -> np.ndarray:
    x = x - x.max(axis=1, keepdims=True)
    ex = np.exp(x)
    return ex / ex.sum(axis=1, keepdims=True)


def ajustar_em(p: pd.DataFrame, df: pd.DataFrame, iteracions: int = 40, ignorar_dorsal=False):
    """EM: reestima λ_c (gols/90), κ_c (targetes/90) i π_d(c) amb priors informatius."""
    prior = _prior_dorsal_init(df)
    lam, kap = dict(LAMBDA0), dict(KAPPA0)
    prior0 = {k: v.copy() for k, v in prior.items()}
    dors = p["dorsal"].to_numpy()
    # Els dorsals alts només s'aprenen quan hi ha prou minuts per jugador (cap a
    # la jornada 8-10); abans, amb 2-3 partits, les dades són massa fines.
    tit = p[p["n_titular"] > 0]
    aprendre_dorsals = bool(len(tit) and tit["minuts"].median() >= APRENDRE_DORSALS_MIN_MINUTS)
    for _ in range(iteracions):
        resp = _softmax(_loglik(p, prior, lam, kap, ignorar_dorsal))
        e = p["exposicio"].to_numpy() + 1e-3
        for j, c in enumerate(CLASSES):
            if c == "POR":
                continue    # el porter queda fixat pel prior (hi ha pocs gols/targetes)
            r = resp[:, j]
            lam[c] = (LAMBDA0[c] * PRIOR_EXPOSICIO + (r * p["gols"]).sum()) / (PRIOR_EXPOSICIO + (r * e).sum())
            kap[c] = (KAPPA0[c] * PRIOR_EXPOSICIO + (r * p["cards"]).sum()) / (PRIOR_EXPOSICIO + (r * e).sum())
        if aprendre_dorsals and not ignorar_dorsal:
            # M-step de π_d(c) NOMÉS per als dorsals sense convenció (12+), amb un
            # prior fort cap a la marginal global. Els dorsals 1-11 mantenen la
            # convenció: reestimar-los amb poques dades els difumina (el posterior
            # sense dorsal és gairebé pla) i, fent-ho amb el posterior complet,
            # l'EM es reforça a si mateix i dorsals arbitraris acabarien "sent"
            # una posició per atzar.
            for d in range(12, 100):
                m = (dors == d)
                if not m.any():
                    continue
                nou = (PRIOR_DORSAL * prior0[d] + resp[m].sum(axis=0)) / (PRIOR_DORSAL + m.sum())
                prior[d] = nou / nou.sum()
    resp = _softmax(_loglik(p, prior, lam, kap, ignorar_dorsal))
    return resp, prior, lam, kap


# ----------------------------------------------------------------------------
# Stage 2: formació vàlida per equip i partit (DP)
# ----------------------------------------------------------------------------
def _log_formacio(d, m, f, prior_equip=None, pes_equip=0.0):
    base = FORMACIONS.get((d, m, f), LOG_FORMACIO_DESCONEGUDA)
    if prior_equip is not None and pes_equip > 0:
        base += pes_equip * prior_equip.get((d, m, f), math.log(1e-3))
    return base


def assignar_partit(logp_out: np.ndarray, prior_equip=None, pes_equip=0.0):
    """logp_out: (n, 3) log P(DEF/MIG/DAV) de cada jugador de camp.
    Retorna (rols[n] amb 0/1/2, formació (d,m,f), score)."""
    n = len(logp_out)
    NEG = -1e18
    dp = np.full((n + 1, MAX_D + 1, MAX_M + 1), NEG)
    ch = np.zeros((n + 1, MAX_D + 1, MAX_M + 1), dtype=np.int8)
    dp[0, 0, 0] = 0.0
    for i in range(n):
        for d in range(min(i, MAX_D) + 1):
            for m in range(min(i - d, MAX_M) + 1):
                cur = dp[i, d, m]
                if cur <= NEG / 2:
                    continue
                f = i - d - m
                if d < MAX_D and cur + logp_out[i, 0] > dp[i + 1, d + 1, m]:
                    dp[i + 1, d + 1, m] = cur + logp_out[i, 0]; ch[i + 1, d + 1, m] = 0
                if m < MAX_M and cur + logp_out[i, 1] > dp[i + 1, d, m + 1]:
                    dp[i + 1, d, m + 1] = cur + logp_out[i, 1]; ch[i + 1, d, m + 1] = 1
                if f < 5 and cur + logp_out[i, 2] > dp[i + 1, d, m]:
                    dp[i + 1, d, m] = cur + logp_out[i, 2]; ch[i + 1, d, m] = 2
    def _triar(admet):
        millor, best, segon = None, NEG, NEG
        for d in range(MAX_D + 1):
            for m in range(MAX_M + 1):
                f = n - d - m
                if f < 0 or dp[n, d, m] <= NEG / 2 or not admet(d, m, f):
                    continue
                tot = dp[n, d, m] + _log_formacio(d, m, f, prior_equip, pes_equip)
                if tot > best:
                    segon, best, millor = best, tot, (d, m)
                elif tot > segon:
                    segon = tot
        return millor, best, segon

    def _estricta(d, m, f):
        if n == 10:
            return (d, m, f) in FORMACIONS
        return d >= 3 and 2 <= m <= 5 and 1 <= f <= 4

    def _laxa(d, m, f):
        return d >= 2 and m >= 1 and f >= 1

    millor, best, segon = _triar(_estricta)
    if millor is None:
        millor, best, segon = _triar(_laxa)
    if millor is None:
        millor, best, segon = _triar(lambda d, m, f: True)
    if millor is None:
        return np.argmax(logp_out, axis=1), None, 0.0
    d, m = millor
    rols = np.zeros(n, dtype=int)
    for i in range(n, 0, -1):
        k = ch[i, d, m]
        rols[i - 1] = k
        if k == 0: d -= 1
        elif k == 1: m -= 1
    return rols, millor + (n - millor[0] - millor[1],), (best - segon if segon > NEG / 2 else 99.0)


def assignar_tots(df: pd.DataFrame, p: pd.DataFrame, resp: np.ndarray, passades: int = 2) -> pd.DataFrame:
    """Aplica el DP a cada (partit, equip) de titulars. Dues passades: a la
    segona, cada equip té un prior de formació aprés de la primera."""
    idx = {(r.categoria, r.grup, r.team, r.player): i for i, r in enumerate(p.itertuples())}
    post = resp
    tit = df[df["titular"]].copy()
    tit["_i"] = [idx.get((r.categoria, r.grup, r.team, r.player), -1) for r in tit.itertuples()]
    grups = tit.groupby(["categoria", "grup", "jornada", "home_team", "away_team", "team"], sort=False)

    prior_equip = {}
    res = []
    for passada in range(passades):
        res = []
        forms_equip = {}
        for key, g in grups:
            g = g.sort_values("ordre")
            gk_pos = g.index[0]                      # 1r de l'acta = porter
            camp = g.iloc[1:]
            lp = np.zeros((len(camp), 3))
            for r_i, (_, row) in enumerate(camp.iterrows()):
                if row["_i"] >= 0:
                    pr = post[int(row["_i"]), 1:]
                    pr = pr / pr.sum()
                else:
                    pr = GLOBAL_OUT_PRIOR
                lp[r_i] = np.log(np.clip(pr, 1e-6, None))
            team_key = (key[0], key[1], key[5])
            rols, form, marge = assignar_partit(lp, prior_equip.get(team_key), 0.6 if passada else 0.0)
            forms_equip.setdefault(team_key, []).append(form)
            res.append((key, g.iloc[0], camp, rols, form, marge))
        # prior de formació per equip (log-freq suavitzada)
        prior_equip = {}
        for tk, fs in forms_equip.items():
            fs = [f for f in fs if f]
            if not fs:
                continue
            tot = len(fs) + 1.0
            cnt = pd.Series(fs).value_counts()
            prior_equip[tk] = {f: math.log((c + .2) / tot) for f, c in cnt.items()}

    rows = []
    for key, gk, camp, rols, form, marge in res:
        cat, grup, jor, home, away, team = key
        form_txt = "-".join(map(str, form)) if form else None
        rows.append(dict(categoria=cat, grup=grup, jornada=jor, home_team=home, away_team=away,
                         team=team, player=gk["player"], dorsal=gk["shirt_number"], ordre=int(gk["ordre"]),
                         rol="POR", formacio=form_txt, formacio_marge=round(float(min(marge, 99)), 2)))
        for (_, row), k in zip(camp.iterrows(), rols):
            rows.append(dict(categoria=cat, grup=grup, jornada=jor, home_team=home, away_team=away,
                             team=team, player=row["player"], dorsal=row["shirt_number"], ordre=int(row["ordre"]),
                             rol=OUT[int(k)], formacio=form_txt, formacio_marge=round(float(min(marge, 99)), 2)))
    return pd.DataFrame(rows)


# ----------------------------------------------------------------------------
# Resultat final per jugador
# ----------------------------------------------------------------------------
def combinar(p: pd.DataFrame, resp: np.ndarray, roles: pd.DataFrame) -> pd.DataFrame:
    p = p.copy()
    # vots de rols per partit
    votes = {}
    if not roles.empty:
        cnt = roles.groupby(["categoria", "grup", "team", "player", "rol"]).size().unstack(fill_value=0)
        for c in CLASSES:
            if c not in cnt.columns:
                cnt[c] = 0
        cnt = cnt[CLASSES]
        votes = {k: v.to_numpy(float) for k, v in cnt.iterrows()}
    finals = np.zeros_like(resp)
    for i, r in enumerate(p.itertuples()):
        v = votes.get((r.categoria, r.grup, r.team, r.player))
        base = resp[i]
        if v is not None and v.sum() >= 1:
            # El rol assignat a cada partit (formació vàlida) corregeix el biaix del
            # prior; pesa més com més titularitats té el jugador: w = n / (n + 1).
            w = v.sum() / (v.sum() + 1.0)
            finals[i] = (1 - w) * base + w * (v / v.sum())
        else:
            finals[i] = base
    finals = finals / finals.sum(axis=1, keepdims=True)
    p["p_por"], p["p_def"], p["p_mig"], p["p_dav"] = (finals[:, k].round(4) for k in range(4))
    p["posicio"] = [CLASSES[k] for k in finals.argmax(axis=1)]
    p["confianca"] = finals.max(axis=1).round(3)

    def fiab(r):
        if r.minuts < 90 and r.n_titular == 0:
            return "baixa"
        if r.confianca >= .8 and r.minuts >= 270:
            return "alta"
        if r.confianca >= .6 and r.minuts >= 90:
            return "mitjana"
        return "baixa"
    p["fiabilitat"] = p.apply(fiab, axis=1)

    def font(r):
        if r.n_row0 > 0 and r.posicio == "POR":
            return "acta"                      # primer de la llista a l'acta
        if r.minuts < 180:
            return "dorsal"                    # poques dades: gairebé només el dorsal
        return "dorsal+estadístiques+formació"
    p["font"] = p.apply(font, axis=1)
    return p


def calcular(lineups: pd.DataFrame, ignorar_dorsal=False):
    df = preparar(lineups)
    p = resum_jugadors(df)
    resp, prior, lam, kap = ajustar_em(p, df, ignorar_dorsal=ignorar_dorsal)
    roles = assignar_tots(df, p, resp)
    final = combinar(p, resp, roles)
    cols = ["categoria", "grup", "team", "player", "posicio", "confianca", "fiabilitat",
            "p_por", "p_def", "p_mig", "p_dav", "dorsal", "n_partits", "n_titular",
            "minuts", "gols", "font"]
    final = final[cols].copy()
    final["dorsal"] = final["dorsal"].astype("Int64")
    params = dict(lambda_gols90=lam, kappa_targetes90=kap)
    return final, roles, params


# ----------------------------------------------------------------------------
# Diagnòstic, mostra de validació
# ----------------------------------------------------------------------------
def informe(final: pd.DataFrame, roles: pd.DataFrame, params: dict, lineups: pd.DataFrame):
    print("\n=== INFORME DEL MODEL DE POSICIONS ===")
    print(f"Jugadors: {len(final)}  ·  titulars-partit: {len(roles)}")
    print("Taxes estimades per 90':")
    for c in CLASSES:
        print(f"  {c}: gols {params['lambda_gols90'][c]:.3f}  targetes {params['kappa_targetes90'][c]:.3f}")
    print("\nDistribució de posicions:\n", final["posicio"].value_counts().to_string())
    print("\nFiabilitat:\n", final["fiabilitat"].value_counts().to_string())
    print("\nFormacions més usades:\n",
          roles.drop_duplicates(["categoria", "grup", "jornada", "home_team", "away_team", "team"])
          ["formacio"].value_counts().head(8).to_string())
    conv = {2: "DEF", 3: "DEF", 4: "DEF", 5: "DEF", 8: "MIG", 9: "DAV", 1: "POR"}
    f = final[final["dorsal"].isin(list(conv))].copy()
    f["esperat"] = f["dorsal"].map(conv)
    print(f"\nAcord amb la convenció per dorsal (1-5, 8, 9): {(f.posicio == f.esperat).mean():.1%}")
    ok = roles.groupby(["categoria", "grup", "jornada", "home_team", "away_team", "team"]).rol.apply(
        lambda s: (s == "POR").sum() == 1).mean()
    print(f"Partits-equip amb exactament 1 porter: {ok:.1%}")


def exportar_mostra(final: pd.DataFrame, n: int, path: Path):
    """Mostra estratificada per validar a mà: la meitat dels casos incerts."""
    rng = np.random.default_rng(7)
    inc = final[final["fiabilitat"] != "alta"]
    alt = final[final["fiabilitat"] == "alta"]
    m = pd.concat([inc.sample(min(len(inc), n // 2), random_state=7),
                   alt.sample(min(len(alt), n - n // 2), random_state=7)])
    m = m[["categoria", "grup", "team", "player", "dorsal", "minuts", "gols", "posicio", "confianca"]].copy()
    m["posicio_real"] = ""
    m.sample(frac=1, random_state=3).to_csv(path, index=False)
    print(f"Mostra de {len(m)} jugadors escrita a {path}. Omple 'posicio_real' (POR/DEF/MIG/DAV).")


def validar(final: pd.DataFrame, path: Path):
    v = pd.read_csv(path)
    v = v[v["posicio_real"].isin(CLASSES)]
    if v.empty:
        print("Cap fila amb 'posicio_real' vàlida.")
        return
    m = v.merge(final[["categoria", "grup", "team", "player", "posicio", "fiabilitat"]],
                on=["categoria", "grup", "team", "player"], suffixes=("_antic", ""))
    print(f"Precisió global: {(m.posicio == m.posicio_real).mean():.1%} sobre {len(m)} jugadors")
    print(m.assign(ok=m.posicio == m.posicio_real).groupby("fiabilitat").ok.agg(["mean", "size"]).round(3))
    print(pd.crosstab(m.posicio_real, m.posicio))


# ----------------------------------------------------------------------------
# Supabase
# ----------------------------------------------------------------------------
# Columnes enteres a Supabase: un 13.0 (float) es rebutja amb
# 'invalid input syntax for type integer: "13.0"', i el dorsal és float perquè pot ser NaN.
INT_COLS = {"grup", "dorsal", "n_partits", "n_titular", "minuts", "gols", "jornada", "ordre"}


def _record(row: dict) -> dict:
    out = {}
    for k, v in row.items():
        v = _native(v)
        if k in INT_COLS and v is not None:
            v = int(round(float(v)))
        out[k] = v
    return out


def _native(v):
    if v is None or (isinstance(v, float) and math.isnan(v)):
        return None
    if pd.isna(v) if not isinstance(v, (list, dict, str)) else False:
        return None
    if isinstance(v, (np.integer,)):
        return int(v)
    if isinstance(v, (np.floating,)):
        return float(v)
    return v


def _insert(client, table, records, chunk=500):
    dropped = set()
    for i in range(0, len(records), chunk):
        part = records[i:i + chunk]
        while True:
            payload = [{k: v for k, v in r.items() if k not in dropped} for r in part]
            try:
                client.table(table).insert(payload).execute()
                break
            except Exception as e:                           # columna inexistent → s'omet
                m = re.search(r"Could not find the '([^']+)' column", str(e))
                if m and m.group(1) in part[0] and m.group(1) not in dropped:
                    dropped.add(m.group(1))
                    print(f"    ⚠️  {table}: columna '{m.group(1)}' inexistent, s'omet")
                    continue
                raise


def pujar(final: pd.DataFrame, roles: pd.DataFrame):
    url, key = os.environ.get("SUPABASE_URL"), os.environ.get("SUPABASE_KEY")
    if not url or not key:
        raise RuntimeError("SUPABASE_URL/SUPABASE_KEY no definides: no es pot pujar")
    from supabase import create_client
    client = create_client(url, key)
    print(f"  ☁️  Pujant a {url.split('//')[-1].split('.')[0]}… ({len(final)} jugadors, {len(roles)} rols)")
    for nom, df in (("player_positions", final), ("match_roles", roles)):
        if df.empty:
            print(f"    ⚠️  {nom}: res a pujar")
            continue
        try:
            for (cat, grup), part in df.groupby(["categoria", "grup"]):
                client.table(nom).delete().eq("categoria", cat).eq("grup", int(grup)).execute()
                recs = [_record(r) for r in part.to_dict(orient="records")]
                _insert(client, nom, recs)
        except Exception as e:
            print(f"    ❌ Error pujant {nom}: {e}")
            if "row-level security" in str(e) or "42501" in str(e):
                print("       → Sembla RLS: desactiva-la a la taula o fes servir la clau 'service_role'.")
            raise
        # Verificació: tornem a comptar el que hi ha realment a la taula.
        try:
            n = client.table(nom).select("id", count="exact").limit(1).execute().count
        except Exception as e:
            n = None
            print(f"    ⚠️  {nom}: no s'ha pogut verificar el recompte ({e})")
        if n is not None:
            print(f"    ✅ {nom}: {len(df)} files enviades, {n} files a la taula")
            if n == 0:
                raise RuntimeError(
                    f"{nom} segueix buida després de pujar: probablement RLS (insert acceptat però "
                    "no visible) o una clau sense permisos. Revisa Supabase → Authentication → Policies.")
        else:
            print(f"    ✅ {nom}: {len(df)} files enviades")


# ----------------------------------------------------------------------------
# Entrada
# ----------------------------------------------------------------------------
def aplicar_manuals(final: pd.DataFrame, path: Path) -> pd.DataFrame:
    """Sobreescriu la posició amb les que dóna una persona (posicions_manuals.csv).

    Columnes: categoria, grup, team, player, posicio   (posicio = POR/DEF/MIG/DAV).
    `categoria` i `grup` són opcionals; `team` pot ser un tros del nom (p. ex. "CELLERA").
    La comparació ignora accents, majúscules i espais repetits.
    """
    if not path.exists():
        return final
    import unicodedata
    norm = lambda x: " ".join("".join(c for c in unicodedata.normalize("NFD", str(x).upper())
                                      if unicodedata.category(c) != "Mn").split())
    man = pd.read_csv(path, dtype=str).fillna("")
    final = final.copy()
    fp, ft = final["player"].map(norm), final["team"].map(norm)
    n_ok = 0
    for r in man.itertuples():
        pos = str(r.posicio).strip().upper()
        if pos not in CLASSES:
            continue
        mask = (fp == norm(r.player)) & ft.str.contains(norm(r.team), regex=False)
        if getattr(r, "categoria", ""):
            mask &= final["categoria"].str.upper() == str(r.categoria).upper()
        if getattr(r, "grup", ""):
            mask &= final["grup"].astype(str) == str(r.grup)
        if mask.any():
            n_ok += int(mask.sum())
            final.loc[mask, "posicio"] = pos
            final.loc[mask, "confianca"] = 1.0
            final.loc[mask, "fiabilitat"] = "manual"
            final.loc[mask, "font"] = "manual"
            for c in CLASSES:
                final.loc[mask, "p_" + c.lower()] = 1.0 if c == pos else 0.0
    print(f"  ✍️  Posicions manuals aplicades: {n_ok} jugadors")
    return final


def generar_totes(base_dir: Path = Path("dades"), upload: bool = True, mostrar_informe=False):
    lineups = carregar_lineups(base_dir)
    if lineups.empty:
        print("  ⚠️  Cap alineació trobada — no es calculen posicions")
        return None
    final, roles, params = calcular(lineups)
    base_dir = Path(base_dir)
    # fitxer opcional amb posicions reals (a la carpeta del script o a la de dades)
    for cand in (Path(__file__).parent / "posicions_manuals.csv", base_dir / "posicions_manuals.csv"):
        final = aplicar_manuals(final, cand)
    final.to_csv(base_dir / "player_positions.csv", index=False)
    roles.to_csv(base_dir / "match_roles.csv", index=False)
    print(f"  🧭 Posicions: {len(final)} jugadors, {roles[['jornada','home_team','team']].drop_duplicates().shape[0]} alineacions")
    if mostrar_informe:
        informe(final, roles, params, lineups)
    if upload:
        pujar(final, roles)
    return final, roles, params


if __name__ == "__main__":
    ap = argparse.ArgumentParser()
    ap.add_argument("--base", default="dades")
    ap.add_argument("--no-upload", action="store_true")
    ap.add_argument("--informe", action="store_true")
    ap.add_argument("--exportar-mostra", type=int, metavar="N")
    ap.add_argument("--validar", metavar="CSV")
    a = ap.parse_args()
    out = generar_totes(Path(a.base), upload=not a.no_upload, mostrar_informe=a.informe)
    if out and a.exportar_mostra:
        exportar_mostra(out[0], a.exportar_mostra, Path("mostra_validacio.csv"))
    if out and a.validar:
        validar(out[0], Path(a.validar))
