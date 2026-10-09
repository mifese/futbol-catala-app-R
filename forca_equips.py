"""
forca_equips.py
===============
Model de força d'equip compartit per les prediccions (generar_prediccions.py) i
per l'API (main.py: rating d'equip, tilt, prèvia de partit).

Model (Poisson jeràrquic amb avantatge de camp, estimat de manera CONJUNTA):

    gols local    ~ Poisson( exp(mu + h + atac[local]   - defensa[visitant]) )
    gols visitant ~ Poisson( exp(mu     + atac[visitant] - defensa[local])  )

    atac[i]    ~ N(0, sigma_a^2)      defensa[i] ~ N(0, sigma_d^2)
    h          ~ N(h0, tau^2)         (avantatge de camp, compartit pel grup)

S'estima el MODE A POSTERIORI (Newton) i la covariància de Laplace, de manera que:
  * cada equip es compara amb la força dels rivals que ha tingut (abans no);
  * l'encongiment cap a la mitjana ve donat pel prior (no per una constant n/(n+6));
  * es pot mostrejar la incertesa dels paràmetres a cada simulació de lliga.

Només depèn de numpy.
"""
from __future__ import annotations

import math
import re
from dataclasses import dataclass

import numpy as np

# Valors per defecte, validats amb backtest_prediccions.py (validació creuada sobre
# els 27 grups). Es poden sobreescriure per categoria.
SIGMA_ATAC = 0.30
SIGMA_DEF = 0.30
TAU_H = 0.12          # desviació del prior de l'avantatge de camp al voltant de la mitjana de la categoria
H_PER_DEFECTE = 0.18  # log-avantatge de camp si no hi ha cap dada de la categoria (estimat: ~0.16-0.21)

MAX_GOLS = 12


# ---------------------------------------------------------------------------------
# Noms d'equip: matches porta " A"/" B", team_match_stats no
# ---------------------------------------------------------------------------------
def norm_equip(n) -> str:
    return re.sub(r"\s+[AB]$", "", str(n or "").strip(), flags=re.I).upper()


# ---------------------------------------------------------------------------------
# Ajust
# ---------------------------------------------------------------------------------
@dataclass
class Ajust:
    equips: list              # noms (en l'ordre dels índexs)
    mu: float
    h: float
    atac: np.ndarray
    defensa: np.ndarray
    cov: np.ndarray           # covariància de Laplace de [mu, h, atac..., defensa...]
    n_partits: int
    sigma_a: float
    sigma_d: float

    @property
    def n(self):
        return len(self.equips)

    def idx(self, equip) -> int | None:
        try:
            return self._map[norm_equip(equip)]
        except (AttributeError, KeyError):
            self._map = {norm_equip(e): i for i, e in enumerate(self.equips)}
            return self._map.get(norm_equip(equip))

    def theta(self) -> np.ndarray:
        return np.concatenate([[self.mu, self.h], self.atac, self.defensa])

    def lambdas(self, local, visitant, theta=None):
        """(lambda_local, lambda_visitant). Equip desconegut → força mitjana (0)."""
        th = self.theta() if theta is None else theta
        n = self.n
        i, j = self.idx(local), self.idx(visitant)
        a_i = th[2 + i] if i is not None else 0.0
        d_i = th[2 + n + i] if i is not None else 0.0
        a_j = th[2 + j] if j is not None else 0.0
        d_j = th[2 + n + j] if j is not None else 0.0
        mu, h = th[0], th[1]
        return np.exp(mu + h + a_i - d_j), np.exp(mu + a_j - d_i)

    def mostrejar(self, S: int, rng: np.random.Generator) -> np.ndarray:
        """S mostres (S, 2+2n) de la posterior (aprox. normal de Laplace)."""
        cov = (self.cov + self.cov.T) / 2
        try:
            L = np.linalg.cholesky(cov + 1e-10 * np.eye(len(cov)))
        except np.linalg.LinAlgError:
            w, V = np.linalg.eigh(cov)
            L = V * np.sqrt(np.clip(w, 0, None))
        return self.theta()[None, :] + rng.standard_normal((S, len(cov))) @ L.T


def ajustar(partits, sigma_a=SIGMA_ATAC, sigma_d=SIGMA_DEF, h_prior=(H_PER_DEFECTE, TAU_H),
            equips=None, iteracions=50) -> Ajust:
    """
    partits: iterable de (local, visitant, gols_local, gols_visitant) ja jugats.
    equips:  llista completa d'equips del grup (així els que encara no han jugat també hi són).
    """
    partits = [(l, v, float(a), float(b)) for l, v, a, b in partits if a is not None and b is not None
               and a == a and b == b]
    if equips is None:
        equips = sorted({p[0] for p in partits} | {p[1] for p in partits})
    equips = list(equips)
    n = len(equips)
    pos = {norm_equip(e): i for i, e in enumerate(equips)}
    hi = np.array([pos[norm_equip(p[0])] for p in partits], dtype=int)
    ai = np.array([pos[norm_equip(p[1])] for p in partits], dtype=int)
    gh = np.array([p[2] for p in partits])
    ga = np.array([p[3] for p in partits])
    m = len(partits)
    dim = 2 + 2 * n

    # prior: mu pla (molt feble), h ~ N(h0, tau), atac/defensa ~ N(0, sigma)
    prec = np.zeros(dim)
    prec[0] = 1e-6
    prec[1] = 1.0 / h_prior[1] ** 2
    prec[2:2 + n] = 1.0 / sigma_a ** 2
    prec[2 + n:] = 1.0 / sigma_d ** 2
    centre = np.zeros(dim)
    centre[1] = h_prior[0]

    th = np.zeros(dim)
    th[1] = h_prior[0]
    th[0] = math.log(max((gh.sum() + ga.sum()) / max(2 * m, 1), 0.3)) if m else math.log(1.2)

    # Matrius de disseny: eta_local = Xh @ th, eta_visitant = Xa @ th
    Xh = np.zeros((m, dim)); Xa = np.zeros((m, dim))
    if m:
        r = np.arange(m)
        Xh[:, 0] = 1; Xh[:, 1] = 1; Xh[r, 2 + hi] = 1; Xh[r, 2 + n + ai] = -1
        Xa[:, 0] = 1; Xa[r, 2 + ai] = 1; Xa[r, 2 + n + hi] = -1

    def construir(th):
        return np.exp(np.clip(Xh @ th, -6, 3)), np.exp(np.clip(Xa @ th, -6, 3))

    def hessiana(lh, la):
        H = np.diag(prec).astype(float)
        if m:
            H += Xh.T @ (Xh * lh[:, None]) + Xa.T @ (Xa * la[:, None])
        return H

    for _ in range(iteracions):
        lh, la = construir(th)
        grad = -prec * (th - centre)
        if m:
            grad += Xh.T @ (gh - lh) + Xa.T @ (ga - la)
        pas = np.linalg.solve(hessiana(lh, la), grad)
        th = th + pas
        if np.max(np.abs(pas)) < 1e-7:
            break

    lh, la = construir(th)
    cov = np.linalg.inv(hessiana(lh, la))
    return Ajust(equips=equips, mu=float(th[0]), h=float(th[1]), atac=th[2:2 + n].copy(),
                 defensa=th[2 + n:].copy(), cov=cov, n_partits=m, sigma_a=sigma_a, sigma_d=sigma_d)


def h_categoria(grups_partits, sigma=SIGMA_ATAC):
    """
    Estima l'avantatge de camp comú d'una categoria ajustant els seus grups plegats
    (cada grup amb el seu propi mu). Retorna log-avantatge (float).
    """
    num, den = 0.0, 0.0
    for partits in grups_partits:
        aj = ajustar(partits, sigma, sigma, h_prior=(H_PER_DEFECTE, 10.0))
        pes = 1.0 / max(aj.cov[1, 1], 1e-9)
        num += pes * aj.h
        den += pes
    return num / den if den else H_PER_DEFECTE


# ---------------------------------------------------------------------------------
# Probabilitats d'un partit
# ---------------------------------------------------------------------------------
def _pois_pmf(lam, kmax=MAX_GOLS):
    k = np.arange(kmax + 1)
    logp = -lam + k * np.log(np.maximum(lam, 1e-12)) - np.array([math.lgamma(x + 1) for x in k])
    p = np.exp(logp)
    return p / p.sum()


def prob_1x2(lh, la, rho=0.0):
    """P(local), P(empat), P(visitant) amb Poisson independent (+ correcció Dixon-Coles opcional)."""
    M = np.outer(_pois_pmf(lh), _pois_pmf(la))
    if rho:
        M[0, 0] *= max(1 - lh * la * rho, 1e-6)
        M[0, 1] *= 1 + lh * rho
        M[1, 0] *= 1 + la * rho
        M[1, 1] *= max(1 - rho, 1e-6)
        M /= M.sum()
    return float(np.tril(M, -1).sum()), float(np.trace(M)), float(np.triu(M, 1).sum())


def marcador_probable(lh, la):
    M = np.outer(_pois_pmf(lh), _pois_pmf(la))
    i, j = np.unravel_index(np.argmax(M), M.shape)
    return int(i), int(j)


# ---------------------------------------------------------------------------------
# Rating d'equip 0-100 (substitueix el min-max de (atac+defensa)/2)
# ---------------------------------------------------------------------------------
def rating_equips(aj: Ajust) -> dict:
    """
    Força neta = atac + defensa (diferència de gols esperada per partit en escala log,
    contra un rival mitjà). Es converteix a 0-100 amb la funció de distribució normal
    (50 = equip mitjà del grup; ~84 = una desviació estàndard per sobre). Mai es
    reescala min-max: un equip amb 0 partits jugats està a 50 i el rating té
    significat absolut dins el grup.
    """
    from math import erf, sqrt
    forca = aj.atac + aj.defensa
    sd = float(np.std(forca)) if len(forca) > 1 else 0.0
    ref = max(sd, 0.12)          # evita que 2 jornades de soroll ho escalin tot
    out = {}
    for i, e in enumerate(aj.equips):
        z = forca[i] / ref
        out[e] = int(round(100 * 0.5 * (1 + erf(z / sqrt(2)))))
    return out


def punts_esperats_partit(aj: Ajust, local, visitant):
    lh, la = aj.lambdas(local, visitant)
    pl, pe, pv = prob_1x2(lh, la)
    return 3 * pl + pe, 3 * pv + pe, (pl, pe, pv), (lh, la)


# ---------------------------------------------------------------------------------
# Selecció empírica de sigma (evidència marginal de Laplace, agregada per categoria)
# ---------------------------------------------------------------------------------
def log_evidencia(aj_args, sigma, h_prior):
    """log p(dades | sigma) aproximada (Laplace) per a un grup. aj_args = (partits, equips)."""
    partits, equips = aj_args
    aj = ajustar(partits, sigma, sigma, h_prior=h_prior, equips=equips)
    n = aj.n
    th = aj.theta()
    pos = {norm_equip(e): i for i, e in enumerate(aj.equips)}
    ll = 0.0
    for l, v, a, b in partits:
        i, j = pos[norm_equip(l)], pos[norm_equip(v)]
        lh = math.exp(th[0] + th[1] + th[2 + i] - th[2 + n + j])
        la = math.exp(th[0] + th[2 + j] - th[2 + n + i])
        ll += a * math.log(lh) - lh - math.lgamma(a + 1) + b * math.log(la) - la - math.lgamma(b + 1)
    quad = float(np.sum((th[2:] ** 2) / sigma ** 2)) / 2
    prior_norm = -2 * n * math.log(sigma)
    H = np.linalg.inv(aj.cov)
    sign, logdet = np.linalg.slogdet(H)
    return ll - quad + prior_norm - 0.5 * logdet


def estimar_sigma(grups, h_prior, graella=(0.12, 0.16, 0.20, 0.25, 0.30, 0.36, 0.43, 0.52, 0.62)):
    """grups: llista de (partits, equips). Retorna (sigma òptima, {sigma: log-evidència})."""
    sc = {}
    for s in graella:
        sc[s] = sum(log_evidencia(g, s, h_prior) for g in grups if len(g[0]) >= 6)
    return max(sc, key=sc.get), sc
