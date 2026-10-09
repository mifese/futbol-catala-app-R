"""
backtest_prediccions.py
=======================
Compara el model de prediccions ANTIC (mitjanes amb encongiment n/(n+6)) amb el NOU
(Poisson jeràrquic conjunt, forca_equips.py) amb validació creuada per partits.

Ús:
    python backtest_prediccions.py --base dades              # llegeix dades/<CAT>/GRUP<n>/matches.csv
    python backtest_prediccions.py --csv consolidat_26_27_matches.csv

Mètriques (més petit = millor, excepte log-versemblança):
  * logloss gols : -log P(gols local) P(gols visitant)  (Poisson independent)
  * logloss 1X2  : -log P(resultat real)
  * RPS          : Ranked Probability Score del resultat (1X2 ordenat)
S'executa a mesura que avança la temporada: amb més jornades els resultats són més fiables.
"""
import argparse
import math
from pathlib import Path

import numpy as np
import pandas as pd

import forca_equips as fe


# --------------------------- model ANTIC (còpia fidel) -----------------------------
def latents_antic(games: pd.DataFrame):
    global_avg = games[["goals_for", "goals_against"]].stack().mean()
    if not np.isfinite(global_avg) or global_avg <= 0:
        global_avg = 1.2
    agg = games.groupby("team").agg(n=("goals_for", "count"), avg_gf=("goals_for", "mean"),
                                    avg_ga=("goals_against", "mean")).reset_index()
    home = games[games.home_away == "Home"].groupby("team").agg(avg_gf_h=("goals_for", "mean"),
                                                                  avg_ga_h=("goals_against", "mean")).reset_index()
    away = games[games.home_away == "Away"].groupby("team").agg(avg_gf_a=("goals_for", "mean"),
                                                                  avg_ga_a=("goals_against", "mean")).reset_index()
    agg = agg.merge(home, on="team", how="left").merge(away, on="team", how="left")
    agg["shrink"] = agg["n"] / (agg["n"] + 6)
    agg["attack"] = (np.log(agg["avg_gf"].clip(lower=0.01)) - math.log(global_avg)) * agg["shrink"]
    agg["defense"] = -(np.log(agg["avg_ga"].clip(lower=0.01)) - math.log(global_avg)) * agg["shrink"]
    for c, b in [("avg_gf_h", "avg_gf"), ("avg_gf_a", "avg_gf"), ("avg_ga_h", "avg_ga"), ("avg_ga_a", "avg_ga")]:
        agg[c] = agg[c].fillna(agg[b])
    agg["home_boost"] = ((agg["avg_gf_h"] - agg["avg_gf_a"]) / 2 * agg["shrink"]).clip(lower=0)
    agg["away_pen"] = ((agg["avg_ga_a"] - agg["avg_ga_h"]) / 2 * agg["shrink"]).clip(lower=0)
    return agg.set_index("team"), global_avg


def lambdas_antic(lat, g, h, a):
    def L(t, c):
        if t not in lat.index:
            return 0.0
        v = lat.loc[t, c]
        return float(v) if np.isfinite(v) else 0.0
    lh = max(0.05, math.exp(math.log(g) + L(h, "attack") - L(a, "defense") + L(h, "home_boost")))
    la = max(0.05, math.exp(math.log(g) + L(a, "attack") - L(h, "defense") - L(a, "away_pen")))
    return lh, la


# --------------------------------- mètriques --------------------------------------
def pois_logpmf(k, lam):
    return -lam + k * math.log(lam) - math.lgamma(k + 1)


def rps(p, res):
    obs = np.zeros(3)
    obs[res] = 1                                   # 0 = local, 1 = empat, 2 = visitant
    return float(((np.cumsum(p) - np.cumsum(obs))[:2] ** 2).sum() / 2)


def avaluar(lh, la, gh, ga, rho=0.0):
    ll = pois_logpmf(gh, lh) + pois_logpmf(ga, la)
    p = np.array(fe.prob_1x2(lh, la, rho))
    res = 0 if gh > ga else (1 if gh == ga else 2)
    return -ll, -math.log(max(p[res], 1e-9)), rps(p, res)


def carregar(args):
    if args.csv:
        return pd.read_csv(args.csv)
    frames = []
    for f in Path(args.base).glob("*/GRUP*/matches.csv"):
        frames.append(pd.read_csv(f))
    return pd.concat(frames, ignore_index=True)


def cv(m: pd.DataFrame, models, k=8, seed=7):
    """Validació creuada k-fold per partits dins cada grup."""
    rng = np.random.default_rng(seed)
    out = {name: [] for name in models}
    out["lliga"] = []
    cats = {}
    for (cat, grup), g in m.groupby(["categoria", "grup"]):
        g = g.dropna(subset=["goals_home", "goals_away"]).reset_index(drop=True)
        if len(g) < 8:
            continue
        cats.setdefault(cat, []).append((cat, grup, g))
    for cat, grups in cats.items():
        for _, grup, g in grups:
            equips = sorted(set(g.local_team) | set(g.away_team))
            folds = rng.permutation(len(g)) % k
            for f in range(k):
                tr, te = g[folds != f], g[folds == f]
                if te.empty:
                    continue
                # h de la categoria a partir dels ALTRES grups (sense mirar el grup avaluat)
                altres = [[(r.local_team, r.away_team, r.goals_home, r.goals_away) for r in gg.itertuples()]
                          for (_, gr, gg) in grups if gr != grup]
                h0 = fe.h_categoria(altres) if altres else fe.H_PER_DEFECTE
                part = [(r.local_team, r.away_team, r.goals_home, r.goals_away) for r in tr.itertuples()]
                # línia base: només mitjana de la lliga + avantatge de camp
                mh, ma = tr.goals_home.mean(), tr.goals_away.mean()
                games = pd.DataFrame(
                    [(r.local_team, "Home", r.goals_home, r.goals_away) for r in tr.itertuples()] +
                    [(r.away_team, "Away", r.goals_away, r.goals_home) for r in tr.itertuples()],
                    columns=["team", "home_away", "goals_for", "goals_against"])
                lat, gavg = latents_antic(games)
                fits = {}
                for name, kw in models.items():
                    if kw is None or name == "antic":
                        continue
                    fits[name] = fe.ajustar(part, equips=equips, sigma_a=kw["sa"], sigma_d=kw["sd"],
                                            h_prior=(h0, kw.get("tau", fe.TAU_H)))
                for r in te.itertuples():
                    gh, ga = int(r.goals_home), int(r.goals_away)
                    out["lliga"].append((cat, *avaluar(max(mh, .05), max(ma, .05), gh, ga)))
                    if "antic" in models:
                        lh, la = lambdas_antic(lat, gavg, r.local_team, r.away_team)
                        out["antic"].append((cat, *avaluar(lh, la, gh, ga)))
                    for name, kw in models.items():
                        if kw is None or name == "antic":
                            continue
                        lh, la = fits[name].lambdas(r.local_team, r.away_team)
                        out[name].append((cat, *avaluar(lh, la, gh, ga, kw.get("rho", 0.0))))
    return out


def resum(out):
    files = []
    for name, vals in out.items():
        if not vals:
            continue
        a = np.array([v[1:] for v in vals])
        files.append((name, len(a), *a.mean(axis=0), *(a.std(axis=0, ddof=1) / math.sqrt(len(a)))))
    df = pd.DataFrame(files, columns=["model", "n", "logloss_gols", "logloss_1x2", "rps",
                                      "se_gols", "se_1x2", "se_rps"])
    return df.sort_values("logloss_gols").reset_index(drop=True)


if __name__ == "__main__":
    ap = argparse.ArgumentParser()
    ap.add_argument("--base", default="dades")
    ap.add_argument("--csv")
    ap.add_argument("--k", type=int, default=8)
    a = ap.parse_args()
    m = carregar(a)
    models = {"antic": {}}
    for s in (0.10, 0.15, 0.20, 0.25, 0.30, 0.40, 0.50):
        models[f"nou_s{s:.2f}"] = {"sa": s, "sd": s}
    models["nou_s0.30_DC"] = {"sa": 0.30, "sd": 0.30, "rho": -0.05}
    res = resum(cv(m, models, k=a.k))
    pd.set_option("display.width", 200)
    print(res.round(4).to_string(index=False))
