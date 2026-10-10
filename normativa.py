"""
normativa.py
============
Normativa d'ascensos, play-off i descensos de la temporada 2026/27 (Pla de Competicions FCF),
tal com la va resumir l'usuari. Un únic lloc per canviar-la si el reglament varia.

    PRIMERA  (3 grups): campió -> ascens directe. Play-off: 3 segons + millor tercer (2 eliminatòries,
                        la primera per sorteig); puja 1 equip. Descens: 13è-16è.
    SEGONA   (6 grups): campió -> ascens directe. Play-off per grup: 2n-5è (2n-5è i 3r-4t, després final);
                        puja el guanyador. Descens: 13è-16è.
    TERCERA  (18 grups): campió -> ascens directe. Play-off: 6 millors segons + 6 millors tercers; la 1a
                        eliminatòria no pot enfrontar dos tercers ni dos equips del mateix grup.
                        Descens segons territorial (vegeu TERRITORIS_TERCERA).

Tot són eliminatòries a doble partit; la tornada es juga al camp de l'equip millor classificat.
Per comparar equips de grups diferents: posició, punts (o coeficient) i diferència de gols.

SUPÒSITS (marcats per revisar amb el PDF oficial):
  * TERCERA_RONDES: el resum parla de "dues eliminatòries" però també de 6 ascensos amb 12 equips;
    amb 12 equips, 1 ronda dóna 6 guanyadors. Es modela 1 ronda. Si calen 2, canvia TERCERA_RONDES.
  * Barcelona: els 4 pitjors 12ns baixen (el reglament ho condiciona al nombre de grups de Quarta).
  * No es modelen els descensos/ascensos "no compensats".
"""

# Grup de Tercera -> territorial. Comprovat amb els equips de cada grup de les dades 26/27.
TERRITORIS_TERCERA = {
    **{g: "Girona" for g in (1, 2, 3)},
    **{g: "Barcelona" for g in range(4, 14)},
    14: "Lleida", 15: "Lleida",
    16: "Tarragona", 17: "Tarragona",
    18: "Ebre",
}

TERCERA_RONDES = 1          # eliminatòries del play-off de Tercera (veure supòsits)
N_PIRORS_12_BARCELONA = 4   # 12ns de Barcelona que baixen


def n_descens_directes(cat: str, grup: int) -> int:
    """Nombre de places de descens directe (últims classificats) d'un grup."""
    if cat == "TERCERA":
        return 4 if TERRITORIS_TERCERA.get(grup, "Barcelona") in ("Girona", "Barcelona") else 2
    return 4


def descens_12_barcelona(cat: str, grup: int) -> bool:
    return cat == "TERCERA" and TERRITORIS_TERCERA.get(grup) == "Barcelona"
