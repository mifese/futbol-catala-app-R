"""
main.py — Backend FastAPI per Futbol Català
Executa amb: uvicorn main:app --reload
Documentació automàtica: http://localhost:8000/docs
"""

from fastapi import FastAPI, HTTPException, Query
from fastapi.middleware.cors import CORSMiddleware
from supabase import create_client, Client
from dotenv import load_dotenv
import os

load_dotenv()

# ============================================================================
# CONFIGURACIÓ
# ============================================================================

SUPABASE_URL = os.environ.get("SUPABASE_URL")
SUPABASE_KEY = os.environ.get("SUPABASE_KEY")

if not SUPABASE_URL or not SUPABASE_KEY:
    raise RuntimeError("Cal definir SUPABASE_URL i SUPABASE_KEY al fitxer .env")

supabase: Client = create_client(SUPABASE_URL, SUPABASE_KEY)

app = FastAPI(
    title="Futbol Català API",
    description="API de dades del futbol amateur català (Tercera, Segona i Primera Catalana)",
    version="1.0.0",
)

# CORS: permet que el frontend (Next.js) pugui fer peticions al backend
app.add_middleware(
    CORSMiddleware,
    allow_origins=["*"],   # En producció canviar per la URL del frontend
    allow_methods=["GET", "POST"],
    allow_headers=["*"],
)

# ============================================================================
# UTILITATS
# ============================================================================

CATEGORIES_VALIDES = {"TERCERA", "SEGONA", "PRIMERA"}

def validar_categoria(categoria: str) -> str:
    cat = categoria.upper()
    if cat not in CATEGORIES_VALIDES:
        raise HTTPException(
            status_code=400,
            detail=f"Categoria '{categoria}' no vàlida. Usa: TERCERA, SEGONA o PRIMERA"
        )
    return cat

def supabase_query(table: str, filters: dict, columns: str = "*", limit: int = 1000):
    """Executa una consulta a Supabase amb filtres i retorna les dades."""
    try:
        q = supabase.table(table).select(columns)
        for key, val in filters.items():
            q = q.eq(key, val)
        q = q.limit(limit)
        res = q.execute()
        return res.data
    except Exception as e:
        raise HTTPException(status_code=500, detail=f"Error consultant Supabase: {e}")


# ============================================================================
# ENDPOINTS GENERALS
# ============================================================================

@app.get("/", tags=["Info"])
def root():
    return {
        "api": "Futbol Català",
        "versio": "1.0.0",
        "docs": "/docs",
        "categories": ["TERCERA", "SEGONA", "PRIMERA"],
    }


@app.get("/categories", tags=["Info"])
def get_categories():
    """Retorna les categories i grups disponibles."""
    try:
        res = supabase.table("matches").select("categoria, grup").execute()
        resum = {}
        for row in res.data:
            cat  = row["categoria"]
            grup = row["grup"]
            if cat not in resum:
                resum[cat] = []
            if grup not in resum[cat]:
                resum[cat].append(grup)
        for cat in resum:
            resum[cat] = sorted(resum[cat])
        return resum
    except Exception as e:
        raise HTTPException(status_code=500, detail=str(e))


# ============================================================================
# CLASSIFICACIÓ
# ============================================================================

@app.get("/classificacio/{categoria}/{grup}", tags=["Classificació"])
def get_classificacio_actual(categoria: str, grup: int):
    """
    Classificació actual d'un grup (última jornada disponible).
    Exemple: /classificacio/TERCERA/1
    """
    cat = validar_categoria(categoria)

    # Obtenim la jornada màxima disponible
    try:
        res = supabase.table("standings_by_round") \
            .select("jornada") \
            .eq("categoria", cat) \
            .eq("grup", grup) \
            .order("jornada", desc=True) \
            .limit(1) \
            .execute()
        if not res.data:
            raise HTTPException(status_code=404, detail="No hi ha dades per aquest grup")
        jornada_max = res.data[0]["jornada"]
    except HTTPException:
        raise
    except Exception as e:
        raise HTTPException(status_code=500, detail=str(e))

    data = supabase_query(
        "standings_by_round",
        {"categoria": cat, "grup": grup, "jornada": jornada_max},
        columns="team,position,played,wins,draws,losses,goals_for,goals_against,goal_diff,points",
    )
    data.sort(key=lambda x: x["position"])
    return {"categoria": cat, "grup": grup, "jornada": jornada_max, "classificacio": data}


@app.get("/classificacio/{categoria}/{grup}/historial", tags=["Classificació"])
def get_classificacio_historial(categoria: str, grup: int):
    """
    Evolució de la classificació jornada a jornada.
    Exemple: /classificacio/TERCERA/1/historial
    """
    cat = validar_categoria(categoria)
    data = supabase_query(
        "standings_by_round",
        {"categoria": cat, "grup": grup},
        columns="team,jornada,position,points,played",
        limit=5000,
    )
    return {"categoria": cat, "grup": grup, "historial": data}


# ============================================================================
# PARTITS
# ============================================================================






# ============================================================================
# EQUIPS
# ============================================================================

@app.get("/equip/{categoria}/{grup}/{equip}", tags=["Equips"])
def get_fitxa_equip(categoria: str, grup: int, equip: str):
    """
    Fitxa completa d'un equip: estadístiques i historial de partits.
    Exemple: /equip/TERCERA/1/BASE ROSES, C.F. A
    """
    cat = validar_categoria(categoria)

    # Estadístiques agregades
    try:
        res = supabase.table("team_match_stats") \
            .select("*") \
            .eq("categoria", cat) \
            .eq("grup", grup) \
            .eq("team", equip) \
            .limit(10000).execute()
        partits = res.data
    except Exception as e:
        raise HTTPException(status_code=500, detail=str(e))

    if not partits:
        raise HTTPException(status_code=404, detail=f"Equip '{equip}' no trobat")

    # Calcular estadístiques globals
    jugats    = len(partits)
    guanyats  = sum(1 for p in partits if p["goals_for"] > p["goals_against"])
    empatats  = sum(1 for p in partits if p["goals_for"] == p["goals_against"])
    perduts   = sum(1 for p in partits if p["goals_for"] < p["goals_against"])
    gols_a    = sum(p["goals_for"]      for p in partits)
    gols_c    = sum(p["goals_against"]  for p in partits)
    punts     = guanyats * 3 + empatats
    grogues   = sum(p["yellow_cards"]   for p in partits)
    vermelles = sum(p["red_cards"]      for p in partits)

    return {
        "categoria": cat,
        "grup":      grup,
        "equip":     equip,
        "resum": {
            "jugats": jugats, "guanyats": guanyats, "empatats": empatats, "perduts": perduts,
            "gols_a_favor": gols_a, "gols_en_contra": gols_c,
            "diferencia_gols": gols_a - gols_c, "punts": punts,
            "targetes_grogues": grogues, "targetes_vermelles": vermelles,
        },
        "partits": sorted(partits, key=lambda x: x["jornada"]),
    }


@app.get("/equips/{categoria}/{grup}", tags=["Equips"])
def get_equips(categoria: str, grup: int):
    """Llista tots els equips d'un grup."""
    cat = validar_categoria(categoria)
    try:
        res = supabase.table("matches") \
            .select("local_team") \
            .eq("categoria", cat) \
            .eq("grup", grup) \
            .execute()
        equips = sorted(set(r["local_team"] for r in res.data))
        return {"categoria": cat, "grup": grup, "equips": equips}
    except Exception as e:
        raise HTTPException(status_code=500, detail=str(e))


# ============================================================================
# JUGADORS
# ============================================================================

@app.get("/jugadors/{categoria}/{grup}", tags=["Jugadors"])
def get_jugadors(
    categoria: str,
    grup: int,
    equip: str | None = Query(default=None, description="Filtra per equip"),
    min_minuts: int   = Query(default=0,    description="Mínim de minuts jugats"),
):
    """
    Jugadors d'un grup amb estadístiques agregades.
    Exemple: /jugadors/TERCERA/1?equip=BASE ROSES, C.F. A&min_minuts=90
    """
    cat = validar_categoria(categoria)
    try:
        q = supabase.table("player_stats") \
            .select("player,team,matches_played,starts,total_minutes,goals,goals_per_90,cards_per_90") \
            .eq("categoria", cat) \
            .eq("grup", grup)
        if equip:
            q = q.eq("team", equip)
        res = q.execute()
        jugadors = [j for j in res.data if j["total_minutes"] >= min_minuts]
        jugadors.sort(key=lambda x: x["goals"], reverse=True)
        return {"categoria": cat, "grup": grup, "jugadors": jugadors}
    except Exception as e:
        raise HTTPException(status_code=500, detail=str(e))


@app.get("/jugador/{categoria}/{grup}/{jugador}", tags=["Jugadors"])
def get_fitxa_jugador(categoria: str, grup: int, jugador: str):
    """
    Fitxa completa d'un jugador: estadístiques agregades i historial per partit.
    Exemple: /jugador/TERCERA/1/GARCIA RODRIGUEZ, JAIRO DANIEL
    """
    cat = validar_categoria(categoria)

    # Stats agregades
    try:
        res_stats = supabase.table("player_stats") \
            .select("*") \
            .eq("categoria", cat) \
            .eq("grup", grup) \
            .eq("player", jugador) \
            .execute()
    except Exception as e:
        raise HTTPException(status_code=500, detail=str(e))

    if not res_stats.data:
        raise HTTPException(status_code=404, detail=f"Jugador '{jugador}' no trobat")

    # Historial per partit
    historial = supabase_query(
        "player_match_stats",
        {"categoria": cat, "grup": grup, "player": jugador},
        columns="jornada,match_date,team,starter,minutes_played,goals,yellow_cards,red_cards",
        limit=500,
    )
    historial.sort(key=lambda x: x["jornada"])

    # Events del jugador (gols i targetes)
    try:
        res_ev = supabase.table("matches_events") \
            .select("jornada,event_type,minute,team,detail") \
            .eq("categoria", cat) \
            .eq("grup", grup) \
            .eq("player", jugador) \
            .limit(10000).execute()
        events = res_ev.data
    except Exception:
        events = []

    return {
        "categoria": cat,
        "grup":      grup,
        "jugador":   jugador,
        "stats":     res_stats.data[0],
        "historial": historial,
        "events":    events,
    }


# ============================================================================
# ESTADÍSTIQUES GLOBALS
# ============================================================================

@app.get("/stats/golejadors/{categoria}/{grup}", tags=["Estadístiques"])
def get_golejadors(categoria: str, grup: int, limit: int = Query(default=20, le=100)):
    """Top golejadors d'un grup. Exemple: /stats/golejadors/TERCERA/1?limit=10"""
    cat = validar_categoria(categoria)
    try:
        res = supabase.table("player_stats") \
            .select("player,team,goals,total_minutes,goals_per_90,matches_played") \
            .eq("categoria", cat) \
            .eq("grup", grup) \
            .gt("goals", 0) \
            .order("goals", desc=True) \
            .limit(limit) \
            .execute()
        return {"categoria": cat, "grup": grup, "golejadors": res.data}
    except Exception as e:
        raise HTTPException(status_code=500, detail=str(e))


@app.get("/stats/jornada/{categoria}/{grup}/{jornada}", tags=["Estadístiques"])
def get_stats_jornada(categoria: str, grup: int, jornada: int):
    """Resum d'una jornada: resultats, gols i targetes."""
    cat = validar_categoria(categoria)
    partits  = supabase_query("matches",       {"categoria": cat, "grup": grup, "jornada": jornada},
                              columns="local_team,away_team,goals_home,goals_away")
    events   = supabase_query("matches_events",{"categoria": cat, "grup": grup, "jornada": jornada},
                              columns="event_type,minute,team,player,detail", limit=2000)

    gols      = [e for e in events if e["event_type"] == "Gol"]
    grogues   = [e for e in events if e["event_type"] == "Targeta Groga"]
    vermelles = [e for e in events if e["event_type"] == "Targeta Vermella"]

    return {
        "categoria": cat, "grup": grup, "jornada": jornada,
        "partits": partits,
        "resum": {
            "total_gols": len(gols),
            "total_grogues": len(grogues),
            "total_vermelles": len(vermelles),
        },
        "gols": gols,
    }

# ============================================================================
# INICI — KPIs + classificació + golejadors + última jornada
# ============================================================================

@app.get("/inici/{categoria}/{grup}", tags=["Inici"])
def get_inici(categoria: str, grup: int):
    cat = validar_categoria(categoria)

    # Partits jugats
    partits = supabase_query("matches", {"categoria": cat, "grup": grup},
                             columns="jornada,local_team,away_team,goals_home,goals_away", limit=5000)
    # Usem team_match_stats per saber quins partits estan jugats (font de veritat)
    jornades_jugades_set = _jornades_jugades_reals(cat, grup)
    jugats  = [p for p in partits if p["jornada"] in jornades_jugades_set]
    equips  = len(set(p["local_team"] for p in partits))
    jornada_max_resultats = max(jornades_jugades_set, default=0)

    # Usar team_match_stats per calcular gols i resultats (font fiable)
    try:
        tms_all = supabase.table("team_match_stats") \
            .select("team,jornada,goals_for,goals_against,home_away") \
            .eq("categoria", cat).eq("grup", grup).limit(10000).execute().data
    except Exception:
        tms_all = []
    # Agrupar per home (evitem doble comptar) → agafem només les files Home
    tms_home = [r for r in tms_all if r.get("home_away") == "Home"]
    total_gols    = sum(r["goals_for"] + r["goals_against"] for r in tms_home)
    gols_local    = sum(r["goals_for"]                      for r in tms_home)
    gols_visitant = sum(r["goals_against"]                  for r in tms_home)
    vic_local     = sum(1 for r in tms_home if r["goals_for"] > r["goals_against"])
    empats        = sum(1 for r in tms_home if r["goals_for"] == r["goals_against"])
    n_jugats      = len(tms_home)

    # Jornada màxima disponible a standings_by_round
    try:
        res_jmax = supabase.table("standings_by_round") \
            .select("jornada") \
            .eq("categoria", cat).eq("grup", grup) \
            .order("jornada", desc=True).limit(1).execute()
        jornada_standings = res_jmax.data[0]["jornada"] if res_jmax.data else jornada_max_resultats
    except Exception:
        jornada_standings = jornada_max_resultats

    # Classificació actual des de standings_by_round
    try:
        res_cl = supabase.table("standings_by_round") \
            .select("team,position,played,wins,draws,losses,goals_for,goals_against,goal_diff,points") \
            .eq("categoria", cat).eq("grup", grup).eq("jornada", jornada_standings) \
            .order("position").execute()
        classificacio = res_cl.data
    except Exception:
        classificacio = []

    # Top golejadors
    try:
        res_gol = supabase.table("player_stats") \
            .select("player,team,goals,total_minutes,goals_per_90") \
            .eq("categoria", cat).eq("grup", grup) \
            .gt("goals", 0).order("goals", desc=True).limit(10).execute()
        golejadors = res_gol.data
    except Exception:
        golejadors = []

    # Jugadors únics
    try:
        res_jug = supabase.table("player_stats") \
            .select("player") \
            .eq("categoria", cat).eq("grup", grup).execute()
        n_jugadors = len(res_jug.data)
    except Exception:
        n_jugadors = 0

    # Última jornada AMB resultats
    # Última jornada: filtrar des de matches per jornada màxima jugada
    ultima_jornada = [p for p in partits if p["jornada"] == jornada_max_resultats]
    # Reconstruir marcadors des de team_match_stats per l'última jornada
    tms_uj = {r["team"]: r for r in tms_all if r["jornada"] == jornada_max_resultats and r.get("home_away")=="Home"}
    ultima_jornada_result = []
    for p in ultima_jornada:
        row = tms_uj.get(p["local_team"])
        if row:
            ultima_jornada_result.append({
                "local_team": p["local_team"], "away_team": p["away_team"],
                "goals_home": row["goals_for"], "goals_away": row["goals_against"]
            })
        else:
            ultima_jornada_result.append({
                "local_team": p["local_team"], "away_team": p["away_team"],
                "goals_home": p.get("goals_home"), "goals_away": p.get("goals_away")
            })
    ultima_jornada = ultima_jornada_result

    return {
        "categoria": cat, "grup": grup,
        "kpis": {
            "equips":          equips,
            "jugadors":        n_jugadors,
            "jornades":        jornada_max_resultats,
            "total_gols":      total_gols,
            "gols_local":      gols_local,
            "gols_visitant":   gols_visitant,
            "avg_gols_partit": round(total_gols / n_jugats, 2) if n_jugats else 0,
            "pct_vic_local":   round(vic_local / n_jugats * 100, 1) if n_jugats else 0,
            "pct_empat":       round(empats / n_jugats * 100, 1) if n_jugats else 0,
        },
        "classificacio":  classificacio,
        "golejadors":     golejadors,
        "ultima_jornada": ultima_jornada,
    }


# ============================================================================
# CLASSIFICACIÓ — Casa / Fora + Head2Head + Evolució posicions
# ============================================================================

@app.get("/classificacio/{categoria}/{grup}/casa-fora", tags=["Classificació"])
def get_classificacio_casa_fora(categoria: str, grup: int):
    """Classificació separada per partits a casa i a fora."""
    cat = validar_categoria(categoria)
    partits = supabase_query("team_match_stats", {"categoria": cat, "grup": grup},
                             columns="team,home_away,goals_for,goals_against,yellow_cards,red_cards", limit=5000)

    def calcular(dades):
        equips = {}
        for p in dades:
            t = p["team"]
            if t not in equips:
                equips[t] = {"team": t, "played": 0, "wins": 0, "draws": 0,
                             "losses": 0, "goals_for": 0, "goals_against": 0, "points": 0}
            e = equips[t]
            e["played"]        += 1
            e["goals_for"]     += p["goals_for"]
            e["goals_against"] += p["goals_against"]
            gf, gc = p["goals_for"], p["goals_against"]
            if gf > gc:   e["wins"]   += 1; e["points"] += 3
            elif gf == gc: e["draws"]  += 1; e["points"] += 1
            else:          e["losses"] += 1
        result = list(equips.values())
        for e in result:
            e["goal_diff"] = e["goals_for"] - e["goals_against"]
        result.sort(key=lambda x: (-x["points"], -x["goal_diff"], -x["goals_for"]))
        for i, e in enumerate(result):
            e["position"] = i + 1
        return result

    casa = calcular([p for p in partits if p["home_away"] == "Home"])
    fora = calcular([p for p in partits if p["home_away"] == "Away"])
    total = calcular(partits)

    return {"categoria": cat, "grup": grup, "total": total, "casa": casa, "fora": fora}


@app.get("/classificacio/{categoria}/{grup}/head2head", tags=["Classificació"])
def get_head2head(categoria: str, grup: int):
    """Matriu de confrontacions directes entre tots els equips."""
    cat = validar_categoria(categoria)
    partits = supabase_query("matches", {"categoria": cat, "grup": grup},
                             columns="local_team,away_team,goals_home,goals_away,jornada", limit=5000)
    # Usar team_match_stats com a font de veritat per saber quins partits són jugats
    jornades_jugades = _jornades_jugades_reals(cat, grup)
    jugats = [p for p in partits if p["jornada"] in jornades_jugades]
    equips = sorted(set(p["local_team"] for p in partits))

    # Matriu: resultat de local_team vs away_team
    matriu = {}
    for p in jugats:
        h, a = p["local_team"], p["away_team"]
        key = f"{h}||{a}"
        if p["goals_home"] > p["goals_away"]:  res = "W"
        elif p["goals_home"] == p["goals_away"]: res = "D"
        else: res = "L"
        matriu[key] = {
            "goals_home": p["goals_home"],
            "goals_away": p["goals_away"],
            "result": res,
            "label": f"{p['goals_home']}-{p['goals_away']}"
        }

    return {"categoria": cat, "grup": grup, "equips": equips, "matriu": matriu}


@app.get("/classificacio/{categoria}/{grup}/evolucio", tags=["Classificació"])
def get_evolucio_posicions(categoria: str, grup: int):
    """Evolució de posicions per jornada per a tots els equips."""
    cat = validar_categoria(categoria)
    data = supabase_query("standings_by_round", {"categoria": cat, "grup": grup},
                          columns="team,jornada,position,points", limit=10000)
    # Agrupar per equip
    per_equip = {}
    for row in data:
        t = row["team"]
        if t not in per_equip:
            per_equip[t] = []
        per_equip[t].append({"jornada": row["jornada"], "position": row["position"], "points": row["points"]})
    for t in per_equip:
        per_equip[t].sort(key=lambda x: x["jornada"])
    return {"categoria": cat, "grup": grup, "equips": per_equip}



# ============================================================================
# PREDICCIONS DE CLASSIFICACIÓ FINAL
# ============================================================================

@app.get("/classificacio/{categoria}/{grup}/prediccions", tags=["Classificació"])
def get_prediccions(categoria: str, grup: int):
    """Prediccions de classificació final basades en simulació Monte Carlo."""
    cat = validar_categoria(categoria)
    try:
        res = supabase.table("prediccions") \
            .select("team,punts_actuals,punts_esperats,pos_esperada,prob_campió,prob_top3,prob_descens") \
            .eq("categoria", cat).eq("grup", grup) \
            .order("pos_esperada").execute()
        return {"categoria": cat, "grup": grup, "prediccions": res.data}
    except Exception as e:
        raise HTTPException(status_code=500, detail=str(e))


# ============================================================================
# EQUIPS — Endpoints avançats
# ============================================================================

@app.get("/equip/{categoria}/{grup}/{equip}/avançat", tags=["Equips"])
def get_fitxa_equip_avançat(categoria: str, grup: int, equip: str):
    """
    Fitxa completa d'un equip amb rating, radar, tilt i sèries de dades per gràfics.
    """
    cat = validar_categoria(categoria)

    # Tots els partits de l'equip
    try:
        res_tms = supabase.table("team_match_stats") \
            .select("*") \
            .eq("categoria", cat).eq("grup", grup).eq("team", equip) \
            .order("jornada").limit(10000).execute()
        partits = res_tms.data
    except Exception as e:
        raise HTTPException(status_code=500, detail=str(e))

    if not partits:
        raise HTTPException(status_code=404, detail=f"Equip '{equip}' no trobat")

    # Tots els partits del grup (per calcular rating relatiu)
    try:
        res_all = supabase.table("team_match_stats") \
            .select("team,goals_for,goals_against,home_away,yellow_cards,red_cards,jornada") \
            .eq("categoria", cat).eq("grup", grup).limit(10000).execute()
        tots = res_all.data
    except Exception:
        tots = partits

    # Tots els events del grup (per gols per part)
    try:
        res_ev = supabase.table("matches_events") \
            .select("team,event_type,minute,home_team,away_team,jornada") \
            .eq("categoria", cat).eq("grup", grup) \
            .eq("event_type", "Gol").limit(10000).execute()
        events_gols = res_ev.data
    except Exception:
        events_gols = []

    # ── Rating de l'equip ──────────────────────────────────────────────────
    import math

    def calc_rating(tots_data):
        from collections import defaultdict
        gf_sum = defaultdict(float); ga_sum = defaultdict(float); n = defaultdict(int)
        for p in tots_data:
            t = p["team"]
            gf_sum[t] += p["goals_for"]; ga_sum[t] += p["goals_against"]; n[t] += 1
        global_avg = (sum(gf_sum.values()) + sum(ga_sum.values())) / (2 * sum(n.values()) + 0.01)
        scores = {}
        for t in n:
            shrink = n[t] / (n[t] + 5)
            avg_gf = gf_sum[t] / n[t]; avg_ga = ga_sum[t] / n[t]
            atk = (math.log(avg_gf + 0.1) - math.log(global_avg + 0.1)) * shrink
            dfs = -(math.log(avg_ga + 0.1) - math.log(global_avg + 0.1)) * shrink
            scores[t] = (atk + dfs) / 2
        if len(scores) < 2:
            return {t: 50 for t in scores}
        mn, mx = min(scores.values()), max(scores.values())
        if mx == mn:
            return {t: 50 for t in scores}
        return {t: round((scores[t] - mn) / (mx - mn) * 100) for t in scores}

    ratings = calc_rating(tots)
    rating_equip = ratings.get(equip, 50)

    # ── Radar de l'equip ──────────────────────────────────────────────────
    def scale_0100(vals):
        mn, mx = min(vals), max(vals)
        if mx == mn: return [50.0] * len(vals)
        return [round((v - mn) / (mx - mn) * 100, 1) for v in vals]

    from collections import defaultdict
    radar_raw = {}
    for p in tots:
        t = p["team"]
        if t not in radar_raw:
            radar_raw[t] = {"gf":[], "ga":[], "pts_h":[], "pts_a":[], "cards":[]}
        pts = 3 if p["goals_for"] > p["goals_against"] else (1 if p["goals_for"] == p["goals_against"] else 0)
        radar_raw[t]["gf"].append(p["goals_for"])
        radar_raw[t]["ga"].append(p["goals_against"])
        radar_raw[t]["cards"].append(p["yellow_cards"] + p["red_cards"] * 3)
        if p["home_away"] == "Home": radar_raw[t]["pts_h"].append(pts)
        else:                        radar_raw[t]["pts_a"].append(pts)

    global_avg = sum(sum(v["gf"]) for v in radar_raw.values()) / max(sum(len(v["gf"]) for v in radar_raw.values()), 1)

    teams_list = sorted(radar_raw.keys())
    def safe_mean(lst): return sum(lst)/len(lst) if lst else 0

    atk_vals    = [math.log(safe_mean(radar_raw[t]["gf"])+0.1) - math.log(global_avg+0.1) for t in teams_list]
    def_vals    = [-(math.log(safe_mean(radar_raw[t]["ga"])+0.1) - math.log(global_avg+0.1)) for t in teams_list]
    home_vals   = [safe_mean(radar_raw[t]["pts_h"]) for t in teams_list]
    away_vals   = [safe_mean(radar_raw[t]["pts_a"]) for t in teams_list]
    fair_vals   = [-safe_mean(radar_raw[t]["cards"]) for t in teams_list]

    # Gols per part de l'equip
    gols_1a = sum(1 for e in events_gols if e["team"] == equip and e["minute"] and e["minute"] <= 45)
    gols_2a = sum(1 for e in events_gols if e["team"] == equip and e["minute"] and e["minute"] > 45)
    n_p_eq  = len(partits)
    all_gols_1a = {}; all_gols_2a = {}
    for e in events_gols:
        t = e["team"]
        if not t: continue
        if e["minute"] and e["minute"] <= 45: all_gols_1a[t] = all_gols_1a.get(t, 0) + 1
        else:                                  all_gols_2a[t] = all_gols_2a.get(t, 0) + 1
    n_p_all = {t: sum(1 for p in tots if p["team"] == t) for t in set(p["team"] for p in tots)}
    first_vals = [(all_gols_1a.get(t,0) / max(n_p_all.get(t,1),1)) for t in teams_list]
    second_vals = [(all_gols_2a.get(t,0) / max(n_p_all.get(t,1),1)) for t in teams_list]

    radar_scaled = {t: {} for t in teams_list}
    for t, a, d, h, aw, f, f1, f2 in zip(teams_list,
        scale_0100(atk_vals), scale_0100(def_vals), scale_0100(home_vals),
        scale_0100(away_vals), scale_0100(fair_vals),
        scale_0100(first_vals), scale_0100(second_vals)):
        radar_scaled[t] = {"atac":a,"defensa":d,"casa":h,"fora":aw,"fairplay":f,"primera_part":f1,"segona_part":f2}

    radar = radar_scaled.get(equip, {k: 50 for k in ["atac","defensa","casa","fora","fairplay","primera_part","segona_part"]})

    # ── Tilt ──────────────────────────────────────────────────────────────
    DRAW_P = 20/3
    def calc_tilt(partits_equip, ratings_all, n_recent=5):
        rows = []
        for p in partits_equip:
            opp = p["opponent"]
            r_eq  = ratings_all.get(equip, 50)
            r_opp = ratings_all.get(opp, 50)
            si = 10**(r_eq/50); sj = 10**(r_opp/50)
            denom = si + sj + DRAW_P
            pts_exp = 3*(si/denom) + 1*(DRAW_P/denom)
            pts_real = 3 if p["goals_for"] > p["goals_against"] else (1 if p["goals_for"] == p["goals_against"] else 0)
            rows.append({"jornada": p["jornada"], "surprise": pts_real - pts_exp, "pts_real": pts_real, "pts_exp": pts_exp})
        if not rows: return {"tilt": 0, "pts_real_avg": 0, "pts_expected_avg": 0}
        rows.sort(key=lambda x: x["jornada"])
        recent = rows[-n_recent:]
        weights = [2**(i) for i in range(len(recent))]
        tilt = sum(r["surprise"]*w for r,w in zip(recent,weights)) / sum(weights)
        return {
            "tilt": round(tilt, 2),
            "pts_real_avg": round(sum(r["pts_real"] for r in recent)/len(recent), 2),
            "pts_expected_avg": round(sum(r["pts_exp"] for r in recent)/len(recent), 2),
            "n_recents": len(recent)
        }

    tilt = calc_tilt(partits, ratings)

    # ── Sèries per gràfics ────────────────────────────────────────────────
    punts_acumulats = []
    pts = 0
    for p in sorted(partits, key=lambda x: x["jornada"]):
        pts += 3 if p["goals_for"] > p["goals_against"] else (1 if p["goals_for"] == p["goals_against"] else 0)
        punts_acumulats.append({"jornada": p["jornada"], "punts": pts, "oponent": p["opponent"],
                                 "gols_favor": p["goals_for"], "gols_contra": p["goals_against"]})

    # Stats agregades
    jugats   = len(partits)
    guanyats = sum(1 for p in partits if p["goals_for"] > p["goals_against"])
    empatats = sum(1 for p in partits if p["goals_for"] == p["goals_against"])
    perduts  = sum(1 for p in partits if p["goals_for"] < p["goals_against"])
    gols_a   = sum(p["goals_for"]     for p in partits)
    gols_c   = sum(p["goals_against"] for p in partits)
    punts    = guanyats * 3 + empatats
    grogues  = sum(p["yellow_cards"]  for p in partits)
    vermelles= sum(p["red_cards"]     for p in partits)

    # Casa vs Fora
    casa = [p for p in partits if p["home_away"] == "Home"]
    fora = [p for p in partits if p["home_away"] == "Away"]
    def stats_bloc(bloc):
        n = len(bloc)
        if n == 0: return {"jugats":0,"guanyats":0,"empatats":0,"perduts":0,"gols_a":0,"gols_c":0,"punts":0}
        g = sum(1 for p in bloc if p["goals_for"] > p["goals_against"])
        e = sum(1 for p in bloc if p["goals_for"] == p["goals_against"])
        pe= sum(1 for p in bloc if p["goals_for"] < p["goals_against"])
        return {"jugats":n,"guanyats":g,"empatats":e,"perduts":pe,
                "gols_a":sum(p["goals_for"] for p in bloc),
                "gols_c":sum(p["goals_against"] for p in bloc),"punts":g*3+e}

    # Gols per part
    gols_1a_equip  = sum(1 for e in events_gols if e.get("team") == equip and e.get("minute") and e["minute"] <= 45)
    gols_2a_equip  = sum(1 for e in events_gols if e.get("team") == equip and e.get("minute") and e["minute"] > 45)
    gols_rebuts_1a = sum(1 for e in events_gols if e.get("team") != equip and e.get("team") is not None
                         and (e.get("home_team") == equip or e.get("away_team") == equip)
                         and e.get("minute") and e["minute"] <= 45)
    gols_rebuts_2a = sum(1 for e in events_gols if e.get("team") != equip and e.get("team") is not None
                         and (e.get("home_team") == equip or e.get("away_team") == equip)
                         and e.get("minute") and e["minute"] > 45)

    return {
        "categoria": cat, "grup": grup, "equip": equip,
        "rating": rating_equip,
        "radar":  radar,
        "tilt":   tilt,
        "resum": {
            "jugats": jugats, "guanyats": guanyats, "empatats": empatats, "perduts": perduts,
            "gols_a_favor": gols_a, "gols_en_contra": gols_c,
            "diferencia_gols": gols_a - gols_c, "punts": punts,
            "targetes_grogues": grogues, "targetes_vermelles": vermelles,
        },
        "casa": stats_bloc(casa),
        "fora": stats_bloc(fora),
        "gols_per_part": {
            "marcats_1a": gols_1a_equip, "marcats_2a": gols_2a_equip,
            "rebuts_1a":  gols_rebuts_1a,"rebuts_2a":  gols_rebuts_2a,
        },
        "punts_acumulats": punts_acumulats,
        "partits": sorted(partits, key=lambda x: x["jornada"]),
    }


@app.get("/equip/{categoria}/{grup}/{equip}/plantilla", tags=["Equips"])
def get_plantilla_equip(categoria: str, grup: int, equip: str):
    """Jugadors de l'equip amb estadístiques, rating i impacte."""
    cat = validar_categoria(categoria)
    try:
        res = supabase.table("player_stats") \
            .select("player,team,matches_played,starts,total_minutes,goals,goals_per_90,cards_per_90") \
            .eq("categoria", cat).eq("grup", grup).eq("team", equip) \
            .order("total_minutes", desc=True).execute()
        jugadors = res.data
    except Exception as e:
        raise HTTPException(status_code=500, detail=str(e))

    try:
        impact_data, ratings_data, _ = _calc_jugadors(cat, grup)
        for j in jugadors:
            k = (j["player"], j["team"])
            rd = ratings_data.get(k) or {}
            im = impact_data.get(k) or {}
            j["rating"]  = rd.get("rating_global")
            imp = im.get("impacte")
            j["impacte"] = round(imp, 2) if imp is not None else None
    except Exception:
        for j in jugadors:
            j.setdefault("rating", None)
            j.setdefault("impacte", None)

    return {"categoria": cat, "grup": grup, "equip": equip, "jugadors": jugadors}


@app.get("/equips/{categoria}/{grup}/comparador", tags=["Equips"])
def get_comparador_equips(categoria: str, grup: int, equip1: str, equip2: str):
    """Dades per comparar dos equips."""
    cat = validar_categoria(categoria)
    from urllib.parse import unquote
    eq1, eq2 = unquote(equip1), unquote(equip2)

    def get_dades_equip(equip):
        try:
            res = supabase.table("team_match_stats") \
                .select("*").eq("categoria", cat).eq("grup", grup).eq("team", equip) \
                .order("jornada").limit(10000).execute()
            return res.data
        except Exception:
            return []

    # Confrontació directa
    try:
        res_h2h = supabase.table("matches") \
            .select("local_team,away_team,goals_home,goals_away,jornada") \
            .eq("categoria", cat).eq("grup", grup).execute()
        h2h = [p for p in res_h2h.data if
               (p["local_team"] == eq1 and p["away_team"] == eq2) or
               (p["local_team"] == eq2 and p["away_team"] == eq1)]
        jornades_jugades_h2h = _jornades_jugades_reals(cat, grup)
        h2h = [p for p in h2h if p["jornada"] in jornades_jugades_h2h]
    except Exception:
        h2h = []

    return {
        "categoria": cat, "grup": grup,
        "equip1": {"nom": eq1, "partits": get_dades_equip(eq1)},
        "equip2": {"nom": eq2, "partits": get_dades_equip(eq2)},
        "h2h": h2h,
    }


# ── Endpoint ampliat per equip: tot el que necessiten els nous gràfics ────────
@app.get("/equip/{categoria}/{grup}/{equip}/complet", tags=["Equips"])
def get_equip_complet(categoria: str, grup: int, equip: str):
    """Tot el que necessita la fitxa completa d'equip incloent gràfics avançats."""
    cat = validar_categoria(categoria)
    import math
    from collections import defaultdict

    # Partits de l'equip (jugats i pendents)
    try:
        res_matches = supabase.table("matches") \
            .select("jornada,local_team,away_team,goals_home,goals_away") \
            .eq("categoria", cat).eq("grup", grup) \
            .execute()
        tots_partits = res_matches.data
    except Exception as e:
        raise HTTPException(status_code=500, detail=str(e))

    partits_equip_raw = [p for p in tots_partits if p["local_team"] == equip or p["away_team"] == equip]
    # Usem team_match_stats per saber quins partits estan realment jugats
    jornades_jugades_set = _jornades_jugades_reals(cat, grup)
    jugats   = [p for p in partits_equip_raw if p["jornada"] in jornades_jugades_set]
    pendents = [p for p in partits_equip_raw if p["jornada"] not in jornades_jugades_set]

    def resultat_per_equip(p):
        es_local = p["local_team"] == equip
        gf = p["goals_home"] if es_local else p["goals_away"]
        gc = p["goals_away"] if es_local else p["goals_home"]
        opp = p["away_team"] if es_local else p["local_team"]
        cond = "Home" if es_local else "Away"
        return {"jornada": p["jornada"], "opponent": opp, "home_away": cond,
                "goals_for": gf, "goals_against": gc}

    partits_processats = sorted([resultat_per_equip(p) for p in jugats], key=lambda x: x["jornada"])
    pendents_processats = sorted([{"jornada": p["jornada"],
                                    "opponent": p["away_team"] if p["local_team"]==equip else p["local_team"],
                                    "home_away": "Home" if p["local_team"]==equip else "Away"}
                                   for p in pendents], key=lambda x: x["jornada"])

    # Últim partit JUGAT per l'equip (màxima jornada amb resultat)
    ultim  = max(partits_processats, key=lambda x: x["jornada"]) if partits_processats else None
    # Proper partit PENDENT de l'equip (mínima jornada sense resultat)
    proper = min(pendents_processats, key=lambda x: x["jornada"]) if pendents_processats else None

    # Tots els partits del grup per calcular ratings
    try:
        res_tms = supabase.table("team_match_stats") \
            .select("team,goals_for,goals_against,home_away,yellow_cards,red_cards,jornada,opponent,match_id") \
            .eq("categoria", cat).eq("grup", grup).limit(10000).execute()
        tots_tms = res_tms.data
    except Exception:
        tots_tms = []

    partits_tms_equip = [p for p in tots_tms if p["team"] == equip]

    # Events de l'equip: tres queries dirigides per evitar el límit de 1000 de Supabase.
    # Query 1: events on l'equip és el protagonista (gols marcats, targetes pròpies)
    # Query 2+3: tots els events dels partits de l'equip (per calcular gols rebuts)
    try:
        ev_as_team = supabase.table("matches_events") \
            .select("team,event_type,minute,home_team,away_team,jornada,player") \
            .eq("categoria", cat).eq("grup", grup).eq("team", equip) \
            .limit(2000).execute().data or []
    except Exception:
        ev_as_team = []

    try:
        ev_as_home = supabase.table("matches_events") \
            .select("team,event_type,minute,home_team,away_team,jornada,player") \
            .eq("categoria", cat).eq("grup", grup).eq("home_team", equip) \
            .limit(2000).execute().data or []
    except Exception:
        ev_as_home = []

    try:
        ev_as_away = supabase.table("matches_events") \
            .select("team,event_type,minute,home_team,away_team,jornada,player") \
            .eq("categoria", cat).eq("grup", grup).eq("away_team", equip) \
            .limit(2000).execute().data or []
    except Exception:
        ev_as_away = []

    # events_equip: events on l'equip fa l'accio
    events_equip = ev_as_team
    gols_equip   = [e for e in events_equip if e["event_type"] == "Gol"]

    # events: tots els events dels partits de l'equip (local+visitant), deduplicats
    _seen = set()
    events = []
    for e in ev_as_home + ev_as_away:
        key = (e.get("jornada"), e.get("team"), e.get("minute"),
               e.get("event_type"), e.get("player"))
        if key not in _seen:
            _seen.add(key)
            events.append(e)

    # Player stats de l'equip
    try:
        res_ps = supabase.table("player_stats") \
            .select("player,goals,total_minutes,matches_played") \
            .eq("categoria", cat).eq("grup", grup).eq("team", equip).execute()
        player_stats = res_ps.data
    except Exception:
        player_stats = []

    # Player match stats de l'equip (per scatterplot càrrega)
    try:
        res_pms = supabase.table("player_match_stats") \
            .select("player,minutes_played,goals,starter") \
            .eq("categoria", cat).eq("grup", grup).eq("team", equip).limit(10000).execute()
        pms_raw = res_pms.data
    except Exception:
        pms_raw = []

    # ── Ratings ────────────────────────────────────────────────────────────────
    def calc_ratings_all(tms_data):
        gf_s = defaultdict(float); ga_s = defaultdict(float); n = defaultdict(int)
        for p in tms_data:
            t = p["team"]; gf_s[t]+=p["goals_for"]; ga_s[t]+=p["goals_against"]; n[t]+=1
        tot = sum(n.values())
        if tot == 0: return {}
        global_avg = (sum(gf_s.values())+sum(ga_s.values()))/(2*tot+0.01)
        scores = {}
        for t in n:
            sh = n[t]/(n[t]+5)
            atk = (math.log(gf_s[t]/n[t]+0.1)-math.log(global_avg+0.1))*sh
            dfs = -(math.log(ga_s[t]/n[t]+0.1)-math.log(global_avg+0.1))*sh
            scores[t] = (atk+dfs)/2
        if len(scores)<2: return {t:50 for t in scores}
        mn,mx = min(scores.values()),max(scores.values())
        if mx==mn: return {t:50 for t in scores}
        return {t: round((scores[t]-mn)/(mx-mn)*100) for t in scores}

    ratings_all = calc_ratings_all(tots_tms)
    rating_equip = ratings_all.get(equip, 50)

    # ── Radar ──────────────────────────────────────────────────────────────────
    def scale_0100_dict(d):
        vals = list(d.values()); keys = list(d.keys())
        mn,mx = min(vals),max(vals)
        if mx==mn: return {k:50.0 for k in keys}
        return {k: round((d[k]-mn)/(mx-mn)*100,1) for k in keys}

    def safe_mean(lst): return sum(lst)/len(lst) if lst else 0

    radar_raw = defaultdict(lambda: {"gf":[],"ga":[],"pts_h":[],"pts_a":[],"cards":[]})
    for p in tots_tms:
        t=p["team"]; pts=3 if p["goals_for"]>p["goals_against"] else (1 if p["goals_for"]==p["goals_against"] else 0)
        radar_raw[t]["gf"].append(p["goals_for"]); radar_raw[t]["ga"].append(p["goals_against"])
        radar_raw[t]["cards"].append(p["yellow_cards"]+p["red_cards"]*3)
        if p["home_away"]=="Home": radar_raw[t]["pts_h"].append(pts)
        else: radar_raw[t]["pts_a"].append(pts)

    gols_per_equip_part = defaultdict(lambda:{"first":0,"second":0})
    for e in [ev for ev in events if ev["event_type"]=="Gol" and ev["team"] and ev["minute"]]:
        if e["minute"]<=45: gols_per_equip_part[e["team"]]["first"]+=1
        else: gols_per_equip_part[e["team"]]["second"]+=1
    n_partits_equip_map = defaultdict(int)
    for p in tots_tms: n_partits_equip_map[p["team"]]+=1

    teams_list = sorted(radar_raw.keys())
    global_avg_g = sum(sum(v["gf"]) for v in radar_raw.values())/max(sum(len(v["gf"]) for v in radar_raw.values()),1)

    atk_d={t:(math.log(safe_mean(radar_raw[t]["gf"])+0.1)-math.log(global_avg_g+0.1)) for t in teams_list}
    def_d={t:-(math.log(safe_mean(radar_raw[t]["ga"])+0.1)-math.log(global_avg_g+0.1)) for t in teams_list}
    home_d={t:safe_mean(radar_raw[t]["pts_h"]) for t in teams_list}
    away_d={t:safe_mean(radar_raw[t]["pts_a"]) for t in teams_list}
    fair_d={t:-safe_mean(radar_raw[t]["cards"]) for t in teams_list}
    first_d={t:gols_per_equip_part[t]["first"]/max(n_partits_equip_map[t],1) for t in teams_list}
    second_d={t:gols_per_equip_part[t]["second"]/max(n_partits_equip_map[t],1) for t in teams_list}

    radars_scaled = {}
    for k,d in [("atac",atk_d),("defensa",def_d),("casa",home_d),("fora",away_d),
                 ("fairplay",fair_d),("primera_part",first_d),("segona_part",second_d)]:
        sc = scale_0100_dict(d)
        for t in teams_list:
            if t not in radars_scaled: radars_scaled[t]={}
            radars_scaled[t][k]=sc.get(t,50)

    radar = radars_scaled.get(equip,{k:50 for k in ["atac","defensa","casa","fora","fairplay","primera_part","segona_part"]})

    # ── Tilt ───────────────────────────────────────────────────────────────────
    DRAW_P=20/3
    def calc_tilt(p_equip, ratings, n_recent=5):
        rows=[]
        for p in p_equip:
            opp=p["opponent"]; r_eq=ratings.get(equip,50); r_opp=ratings.get(opp,50)
            si=10**(r_eq/50); sj=10**(r_opp/50); denom=si+sj+DRAW_P
            pts_exp=3*(si/denom)+1*(DRAW_P/denom)
            pts_real=3 if p["goals_for"]>p["goals_against"] else (1 if p["goals_for"]==p["goals_against"] else 0)
            rows.append({"jornada":p["jornada"],"surprise":pts_real-pts_exp,"pts_real":pts_real,"pts_exp":pts_exp})
        if not rows: return {"tilt":0,"pts_real_avg":0,"pts_expected_avg":0,"n_recents":0}
        rows.sort(key=lambda x:x["jornada"]); recent=rows[-n_recent:]
        weights=[2**i for i in range(len(recent))]
        tilt=sum(r["surprise"]*w for r,w in zip(recent,weights))/sum(weights)
        return {"tilt":round(tilt,2),"pts_real_avg":round(sum(r["pts_real"] for r in recent)/len(recent),2),
                "pts_expected_avg":round(sum(r["pts_exp"] for r in recent)/len(recent),2),"n_recents":len(recent)}

    tilt = calc_tilt(partits_tms_equip, ratings_all)

    # ── Resum estadístic ───────────────────────────────────────────────────────
    def stats_bloc(bloc):
        n=len(bloc)
        if n==0: return {"jugats":0,"guanyats":0,"empatats":0,"perduts":0,"gols_a":0,"gols_c":0,"punts":0}
        g=sum(1 for p in bloc if p["goals_for"]>p["goals_against"])
        e=sum(1 for p in bloc if p["goals_for"]==p["goals_against"])
        pe=sum(1 for p in bloc if p["goals_for"]<p["goals_against"])
        return {"jugats":n,"guanyats":g,"empatats":e,"perduts":pe,
                "gols_a":sum(p["goals_for"] for p in bloc),
                "gols_c":sum(p["goals_against"] for p in bloc),"punts":g*3+e}

    casa_partits = [p for p in partits_tms_equip if p["home_away"]=="Home"]
    fora_partits = [p for p in partits_tms_equip if p["home_away"]=="Away"]
    resum_total = stats_bloc(partits_tms_equip)
    resum_casa  = stats_bloc(casa_partits)
    resum_fora  = stats_bloc(fora_partits)

    # ── Punts acumulats per jornada ────────────────────────────────────────────
    pts=0; punts_acumulats=[]
    for p in partits_processats:
        pts+=3 if p["goals_for"]>p["goals_against"] else (1 if p["goals_for"]==p["goals_against"] else 0)
        punts_acumulats.append({"jornada":p["jornada"],"punts":pts,"oponent":p["opponent"],
                                  "gols_favor":p["goals_for"],"gols_contra":p["goals_against"]})

    # ── Gols per minut (distribució) ──────────────────────────────────────────
    # IMPORTANT: `events` conté TOTS els events del grup, no només els de l'equip.
    # Per als gols rebuts cal filtrar també per (home_team==equip OR away_team==equip)
    # per assegurar que el gol va ser en un partit on l'equip participava.
    # Idèntic a la lògica del Shiny: filter(team==eq) / filter((home_team==eq|away_team==eq) & team!=eq)
    def _min_val(e):
        """Retorna el minut com a float, o None si no és vàlid."""
        try:
            m = e.get("minute")
            return float(m) if m is not None else None
        except (TypeError, ValueError):
            return None

    def es_gol_marcat(e, mn_b=0, mx_b=999):
        m = _min_val(e)
        return (e.get("event_type") == "Gol"
                and e.get("team") == equip
                and m is not None
                and mn_b <= m <= mx_b)

    def es_gol_rebut(e, mn_b=0, mx_b=999):
        m = _min_val(e)
        return (e.get("event_type") == "Gol"
                and e.get("team") != equip
                and e.get("team") is not None
                and (e.get("home_team") == equip or e.get("away_team") == equip)
                and m is not None
                and mn_b <= m <= mx_b)

    BLOCS=[("1-15",1,15),("16-30",16,30),("31-45",31,45),("46-60",46,60),("61-75",61,75),("76-90",76,90)]
    gols_per_minut=[]
    for label,mn_b,mx_b in BLOCS:
        gols_per_minut.append({
            "bloc":    label,
            "marcats": sum(1 for e in events if es_gol_marcat(e, mn_b, mx_b)),
            "rebuts":  sum(1 for e in events if es_gol_rebut(e,  mn_b, mx_b)),
        })

    # ── Gols per part (marcats i rebuts) ──────────────────────────────────────
    gols_per_part={
        "marcats_1a": sum(1 for e in events if es_gol_marcat(e, 0,  45)),
        "marcats_2a": sum(1 for e in events if es_gol_marcat(e, 46, 999)),
        "rebuts_1a":  sum(1 for e in events if es_gol_rebut(e,  0,  45)),
        "rebuts_2a":  sum(1 for e in events if es_gol_rebut(e,  46, 999)),
    }

    # ── Rendiment vs nivell rival ─────────────────────────────────────────────
    rend_vs_nivell=[]
    for p in partits_tms_equip:
        r_opp=ratings_all.get(p["opponent"],50)
        pts_r=3 if p["goals_for"]>p["goals_against"] else (1 if p["goals_for"]==p["goals_against"] else 0)
        nivell="Rival fort (>60)" if r_opp>60 else ("Rival mig (40-60)" if r_opp>=40 else "Rival feble (<40)")
        rend_vs_nivell.append({"jornada":p["jornada"],"opponent":p["opponent"],"rating_rival":r_opp,"pts":pts_r,"nivell":nivell})

    # Agrupat per nivell
    nivells_agg=defaultdict(lambda:{"pts":[]})
    for r in rend_vs_nivell: nivells_agg[r["nivell"]]["pts"].append(r["pts"])
    rend_vs_nivell_agg=[{"nivell":k,"avg_pts":round(sum(v["pts"])/len(v["pts"]),2),"n":len(v["pts"])}
                         for k,v in nivells_agg.items()]

    # ── Càrrega treball jugadors (scatterplot) ────────────────────────────────
    carrega=[]
    pms_agg=defaultdict(lambda:{"minutes":0,"partits":0,"gols":0})
    for p in pms_raw:
        pl=p["player"]; pms_agg[pl]["minutes"]+=p["minutes_played"]; pms_agg[pl]["partits"]+=1; pms_agg[pl]["gols"]+=p["goals"]
    for pl,v in pms_agg.items():
        if v["minutes"]>0:
            carrega.append({"player":pl,"total_minutes":v["minutes"],"partits":v["partits"],
                             "gols":v["gols"],"avg_minutes":round(v["minutes"]/max(v["partits"],1),1)})
    carrega.sort(key=lambda x:-x["total_minutes"])

    # ── Dependència golejador ─────────────────────────────────────────────────
    # Usar team_match_stats per tenir el total real de gols marcats per l'equip
    total_gols_eq = sum(p["goals_for"] for p in partits_tms_equip)
    top_golejadors=sorted(player_stats,key=lambda x:-x["goals"])[:6]
    deps=[]
    for j in top_golejadors:
        if j["goals"]>0:
            deps.append({"player":j["player"],"goals":j["goals"],
                          "pct":round(j["goals"]/max(total_gols_eq,1)*100,1)})

    # ── Classificació actual de l'equip ───────────────────────────────────────
    posicio = None
    try:
        # Obtenir la jornada màxima disponible a standings_by_round
        res_jmax = supabase.table("standings_by_round")             .select("jornada")             .eq("categoria", cat).eq("grup", grup)             .eq("team", equip)             .order("jornada", desc=True).limit(1).execute()
        if res_jmax.data:
            jornada_std_max = res_jmax.data[0]["jornada"]
            res_pos = supabase.table("standings_by_round")                 .select("position")                 .eq("categoria", cat).eq("grup", grup)                 .eq("team", equip).eq("jornada", jornada_std_max)                 .execute()
            if res_pos.data:
                posicio = res_pos.data[0]["position"]
    except Exception:
        posicio = None

    # ── Gols per jornada (marcats i rebuts) ───────────────────────────────────
    gols_per_jornada = [
        {"jornada": p["jornada"],
         "marcats": p["goals_for"],
         "rebuts":  p["goals_against"]}
        for p in partits_processats
    ]

    return {
        "categoria":cat,"grup":grup,"equip":equip,
        "rating":rating_equip,"posicio":posicio,
        "radar":radar,"tilt":tilt,
        "ultim_partit":ultim,"proper_partit":proper,
        "resum":{"jugats":resum_total["jugats"],"guanyats":resum_total["guanyats"],
                 "empatats":resum_total["empatats"],"perduts":resum_total["perduts"],
                 "gols_a_favor":resum_total["gols_a"],"gols_en_contra":resum_total["gols_c"],
                 "diferencia_gols":resum_total["gols_a"]-resum_total["gols_c"],
                 "punts":resum_total["punts"],
                 "targetes_grogues":sum(p["yellow_cards"] for p in partits_tms_equip),
                 "targetes_vermelles":sum(p["red_cards"] for p in partits_tms_equip)},
        "casa":resum_casa,"fora":resum_fora,
        "gols_per_part":gols_per_part,
        "gols_per_minut":gols_per_minut,
        "gols_per_jornada":gols_per_jornada,
        "punts_acumulats":punts_acumulats,
        "rend_vs_nivell":rend_vs_nivell_agg,
        "carrega_jugadors":carrega,
        "dependencia_golejador":deps,
        "partits_calendari": sorted(
            [{"jornada":p["jornada"],"opponent":p["opponent"],"home_away":p["home_away"],
              "goals_for":p["goals_for"],"goals_against":p["goals_against"]} for p in partits_processats] +
            [{"jornada":p["jornada"],"opponent":p["opponent"],"home_away":p["home_away"],
              "goals_for":None,"goals_against":None} for p in pendents_processats],
            key=lambda x:x["jornada"]),
    }


# ============================================================================
# JUGADORS — Endpoints complets amb càlcul d'impacte i rating
# ============================================================================

# ============================================================================
# JUGADORS — Càlculs idèntics al Shiny (calculate_player_impact + calculate_player_ratings + calculate_player_radar)
# ============================================================================

_calc_jugadors_cache: dict = {}

@app.post("/cache/clear", tags=["Admin"])
def clear_cache():
    """Invalida la cache de càlculs de jugadors (cridar després d'actualitzar dades)."""
    _calc_jugadors_cache.clear()
    return {"ok": True, "message": "Cache invalidada"}

def _calc_jugadors(cat: str, grup: int):
    """
    Implementa exactament:
      calculate_player_impact()  → línies 54-92 del app.R
      calculate_player_ratings() → línies 94-130
      calculate_player_radar()   → línies 206-241
    Resultat cached en memòria (s'invalida al reiniciar el servidor).
    """
    cache_key = (cat, grup)
    if cache_key in _calc_jugadors_cache:
        return _calc_jugadors_cache[cache_key]
    from collections import defaultdict

    # ── 1. Carregar dades ────────────────────────────────────────────────────
    try:
        pms_raw = supabase.table("player_match_stats") \
            .select("player,team,jornada,starter,minutes_played,goals,yellow_cards,red_cards") \
            .eq("categoria", cat).eq("grup", grup).limit(50000).execute().data
    except Exception:
        pms_raw = []

    try:
        tms_raw = supabase.table("team_match_stats") \
            .select("team,jornada,goals_for,goals_against") \
            .eq("categoria", cat).eq("grup", grup).limit(10000).execute().data
    except Exception:
        tms_raw = []

    if not pms_raw or not tms_raw:
        return {}, {}, {}

    # ── 2. calculate_player_impact ───────────────────────────────────────────
    # team_match_w: punts per equip+jornada (distinct)
    team_j_pts = {}
    for r in tms_raw:
        pts = 3 if r["goals_for"] > r["goals_against"] else (1 if r["goals_for"] == r["goals_against"] else 0)
        team_j_pts[(r["team"], r["jornada"])] = pts

    # all_players_teams: jugadors únics per equip
    player_teams = {}
    for r in pms_raw:
        player_teams[(r["player"], r["team"])] = True

    # player_participation: minuts per (player, team, jornada) — suma si duplicats
    part = defaultdict(int)
    for r in pms_raw:
        part[(r["player"], r["team"], r["jornada"])] += r["minutes_played"]

    # player_complete_record: per cada (player,team) × totes les jornades de l'equip
    # → jugà (minuts>0) o no
    # Jornades per equip
    team_jornades = defaultdict(set)
    for (team, jornada) in [k for k in team_j_pts]:
        team_jornades[team].add(jornada)

    impact_data = {}
    for (player, team) in player_teams:
        pts_jugant = []
        pts_sense  = []
        for j in team_jornades[team]:
            pts = team_j_pts.get((team, j))
            if pts is None:
                continue
            mins = part.get((player, team, j), 0)
            if mins > 0:
                pts_jugant.append(pts)
            else:
                pts_sense.append(pts)

        avg_jugant = sum(pts_jugant) / len(pts_jugant) if pts_jugant else None
        avg_sense  = sum(pts_sense)  / len(pts_sense)  if pts_sense  else None
        # impacte = NA si games_not_played == 0 (igual que app.R línia 86-88)
        impacte = (round(avg_jugant - avg_sense, 4)
                   if avg_jugant is not None and avg_sense is not None and len(pts_sense) > 0
                   else None)
        impact_data[(player, team)] = {
            "avg_points_when_playing":     round(avg_jugant, 4) if avg_jugant is not None else None,
            "avg_points_when_not_playing": round(avg_sense, 4)  if avg_sense  is not None else None,
            "impacte":        impacte,
            "games_played":   len(pts_jugant),
            "games_not_played": len(pts_sense),
        }

    # ── 3. calculate_player_ratings ──────────────────────────────────────────
    # Carregar player_stats com a font de veritat per a goals, minutes, matches
    # player_match_stats pot estar incompleta (menys jornades que les reals),
    # mentre player_stats conté els totals correctes de tota la temporada.
    try:
        ps_all = supabase.table("player_stats") \
            .select("player,team,goals,total_minutes,matches_played,starts,goals_per_90,cards_per_90") \
            .eq("categoria", cat).eq("grup", grup).execute().data
        ps_map = {(r["player"], r["team"]): r for r in ps_all}
    except Exception:
        ps_map = {}

    agg = defaultdict(lambda: {"goals":0,"minutes":0,"yellows":0,"reds":0,"matches":0,"team":""})
    for r in pms_raw:
        k = (r["player"], r["team"])
        agg[k]["goals"]   += r["goals"]
        agg[k]["minutes"] += r["minutes_played"]
        agg[k]["yellows"] += r["yellow_cards"]
        agg[k]["reds"]    += r["red_cards"]
        agg[k]["matches"] += 1
        agg[k]["team"]     = r["team"]

    # Afegir jugadors que apareixen a player_stats però no a player_match_stats
    # i sobreescriure goals i minutes amb els valors correctes de player_stats
    for (pl, tm), ps in ps_map.items():
        k = (pl, tm)
        if k not in agg:
            agg[k]["team"] = tm
        # Sobreescriure sempre amb player_stats (font de veritat per a totals)
        agg[k]["goals"]   = ps.get("goals") or 0
        agg[k]["minutes"] = ps.get("total_minutes") or agg[k]["minutes"]
        agg[k]["matches"] = ps.get("matches_played") or agg[k]["matches"]

    rows = []
    for (player, team), a in agg.items():
        m = a["minutes"]
        # Usar goals_per_90 i cards_per_90 de player_stats si disponibles (ja calculats correctament)
        ps_ref = ps_map.get((player, team), {})
        g90 = ps_ref.get("goals_per_90") if ps_ref.get("goals_per_90") is not None else ((a["goals"] / m * 90) if m > 0 else 0.0)
        y90 = ps_ref.get("cards_per_90") if ps_ref.get("cards_per_90") is not None else ((a["yellows"] / m * 90) if m > 0 else 0.0)
        # impacte = 0 si és NA (replace_na(impacte, 0), línia 111)
        imp = impact_data.get((player, team), {}).get("impacte") or 0.0
        # rating_off_raw (línia 113)
        rating_off_raw = 0.4 * g90 + 0.4 * max(-1.0, min(2.0, imp)) + 0.2 * (m / 900.0)
        # rating_def_raw + clamp (línies 117-118)
        rating_def_raw = 1.0 - 0.15 * y90 + 0.1 * max(0.0, imp)
        rating_def     = max(0.5, min(1.5, rating_def_raw))
        rows.append({
            "player": player, "team": team,
            "rating_off_raw": rating_off_raw,
            "rating_def":     rating_def,
            "g90": g90, "y90": y90,
            "goals":   a["goals"],   "minutes": m,
            "yellows": a["yellows"], "reds": a["reds"],
            "matches": a["matches"],
        })

    if not rows:
        return impact_data, {}, {}

    # rating_off = z-score * 0.3 + 1 (línies 115-116)
    off_vals  = [r["rating_off_raw"] for r in rows]
    mean_off  = sum(off_vals) / len(off_vals)
    var_off   = sum((v - mean_off) ** 2 for v in off_vals) / max(len(off_vals) - 1, 1)
    sd_off    = var_off ** 0.5
    for r in rows:
        r["rating_off"] = (r["rating_off_raw"] - mean_off) / (sd_off + 0.01) * 0.3 + 1.0

    # rating_global_raw (línia 119)
    for r in rows:
        r["rating_global_raw"] = 0.65 * r["rating_off"] + 0.35 * r["rating_def"]

    # Normalitzar 0-100 (línies 120-127)
    rg_vals = [r["rating_global_raw"] for r in rows]
    rng_min, rng_max = min(rg_vals), max(rg_vals)
    for r in rows:
        if rng_max > rng_min:
            r["rating_global"] = round((r["rating_global_raw"] - rng_min) / (rng_max - rng_min) * 100)
        else:
            r["rating_global"] = 50

    ratings_data = {(r["player"], r["team"]): r for r in rows}

    # ── 4. calculate_player_radar ─────────────────────────────────────────────
    # Recalcula starts des de pms_raw
    starts_map = defaultdict(int)
    for r in pms_raw:
        starts_map[(r["player"], r["team"])] += r["starter"]

    def scale_0100(vals):
        """Idèntic a scale_0100 del app.R línies 207-212"""
        finite = [v for v in vals if v is not None and v == v]  # no NaN
        if not finite:
            return [50.0] * len(vals)
        rng_mn, rng_mx = min(finite), max(finite)
        if rng_mx == rng_mn:
            return [50.0] * len(vals)
        return [round(max(0.0, min(100.0, (v - rng_mn) / (rng_mx - rng_mn) * 100)), 1)
                if v is not None else 50.0 for v in vals]

    
    keys_all = list(agg.keys())

    g90_all    = []
    y90_all    = []
    imp_clip   = []
    avail_all  = []
    partic_all = []
    fairp_all  = []

    for k in keys_all:
        a = agg[k]
        m = a["minutes"]
        # Usar goals_per_90 i cards_per_90 de player_stats (font de veritat)
        # agg ja té goals/minutes sobreescrits per player_stats, però
        # g90 directe des de player_stats és el més fiable
        ps_ref = ps_map.get(k, {})
        g90 = ps_ref.get("goals_per_90") if ps_ref.get("goals_per_90") is not None \
              else ((a["goals"] / m * 90) if m > 0 else 0.0)
        cards_p90 = ps_ref.get("cards_per_90") if ps_ref.get("cards_per_90") is not None \
                    else ((a["yellows"] / m * 90) if m > 0 else 0.0)
        y90 = cards_p90
        r90 = (a["reds"] / max(m / 90, 1))
        imp = impact_data.get(k, {}).get("impacte") or 0.0
        starts = starts_map.get(k, 0)
        matches_ref = ps_ref.get("matches_played") or max(a["matches"], 1)
        avail  = starts / max(matches_ref, 1)
        partic = m / max(matches_ref * 90, 1)
        # fairplay = -cards_per_90 - 3*(reds/pmax(total_minutes/90,1))
        fairp  = -y90 - 3.0 * r90

        g90_all.append(g90)
        y90_all.append(y90)
        imp_clip.append(max(-2.0, min(3.0, imp)))
        avail_all.append(avail)
        partic_all.append(partic)
        fairp_all.append(fairp)

    sc_gol  = scale_0100(g90_all)
    sc_imp  = scale_0100(imp_clip)
    sc_tit  = scale_0100(avail_all)
    sc_min  = scale_0100(partic_all)
    sc_fair = scale_0100(fairp_all)

    radar_data = {}
    for i, k in enumerate(keys_all):
        radar_data[k] = {
            "radar_gol":         sc_gol[i],
            "radar_impacte":     sc_imp[i],
            "radar_titularitat": sc_tit[i],
            "radar_minuts":      sc_min[i],
            "radar_fairplay":    sc_fair[i],
        }

    result = (impact_data, ratings_data, radar_data)
    _calc_jugadors_cache[cache_key] = result
    return result


# ── Endpoint buscador ─────────────────────────────────────────────────────────
@app.get("/jugadors/{categoria}/{grup}/complets", tags=["Jugadors"])
def get_jugadors_complets(categoria: str, grup: int):
    """Taula de jugadors amb rating, impacte i radar (per al buscador)."""
    cat = validar_categoria(categoria)
    from concurrent.futures import ThreadPoolExecutor

    def _fetch_ps():
        try:
            return supabase.table("player_stats") \
                .select("player,team,matches_played,starts,total_minutes,goals,goals_per_90,cards_per_90") \
                .eq("categoria", cat).eq("grup", grup).execute().data
        except Exception:
            return []

    with ThreadPoolExecutor(max_workers=2) as ex:
        fut_calc = ex.submit(_calc_jugadors, cat, grup)
        fut_ps   = ex.submit(_fetch_ps)
        impact_data, ratings_data, radar_data = fut_calc.result()
        ps = fut_ps.result()
    result = []
    for j in ps:
        k = (j["player"], j["team"])
        rd = ratings_data.get(k, {})
        im = impact_data.get(k, {})
        ra = radar_data.get(k, {})
        result.append({
            **j,
            "rating":  rd.get("rating_global"),
            "impacte": round(im.get("impacte"), 2) if im.get("impacte") is not None else None,
            "avg_pts_jugant": round(im.get("avg_points_when_playing"),  2) if im.get("avg_points_when_playing")  is not None else None,
            "avg_pts_sense":  round(im.get("avg_points_when_not_playing"), 2) if im.get("avg_points_when_not_playing") is not None else None,
            "games_played":     im.get("games_played", 0),
            "games_not_played": im.get("games_not_played", 0),
            "radar": ra,
        })

    result.sort(key=lambda x: -(x["rating"] or 0))
    return {"categoria": cat, "grup": grup, "jugadors": result}


# ── Endpoint fitxa individual ─────────────────────────────────────────────────
@app.get("/jugador/{categoria}/{grup}/{jugador}/complet", tags=["Jugadors"])
def get_jugador_complet(categoria: str, grup: int, jugador: str):
    """
    Fitxa completa d'un jugador. Conté exactament el que mostra el modal del Shiny:
      - rating, impacte, radar
      - historial per partit (amb rival i resultat)
      - gols acumulats
      - timing gols (distribució per franja)
      - events (gols, targetes) per jornada
    """
    cat = validar_categoria(categoria)

    # Stats bàsiques
    try:
        ps_row = supabase.table("player_stats") \
            .select("player,team,matches_played,starts,total_minutes,goals,goals_per_90,cards_per_90") \
            .eq("categoria", cat).eq("grup", grup).eq("player", jugador).execute().data
        if not ps_row:
            raise HTTPException(404, f"Jugador '{jugador}' no trobat")
        stats = ps_row[0]
        team  = stats["team"]
    except HTTPException:
        raise
    except Exception as e:
        raise HTTPException(500, str(e))

    # Càlculs del grup sencer (necessari per normalitzar)
    impact_data, ratings_data, radar_data = _calc_jugadors(cat, grup)
    k = (jugador, team)
    im = impact_data.get(k, {})
    rd = ratings_data.get(k, {})
    ra = radar_data.get(k, {})

    # Historial per partit
    try:
        hist_raw = supabase.table("player_match_stats") \
            .select("jornada,match_date,starter,minutes_played,goals,yellow_cards,red_cards") \
            .eq("categoria", cat).eq("grup", grup).eq("player", jugador) \
            .order("jornada").limit(10000).execute().data
    except Exception:
        hist_raw = []

    # Gols acumulats
    gols_acum = 0
    for h in hist_raw:
        gols_acum += h["goals"]
        h["gols_acumulats"] = gols_acum

    # Events del jugador
    try:
        events = supabase.table("matches_events") \
            .select("jornada,event_type,minute,detail") \
            .eq("categoria", cat).eq("grup", grup).eq("player", jugador).limit(10000).execute().data
    except Exception:
        events = []

    # Timing gols — 7 franges com al Shiny (0-15, 16-30, 31-45, 46-60, 61-75, 76-90, 90+)
    BLOCS = [("0-15'",0,15),("16-30'",16,30),("31-45'",31,45),
             ("46-60'",46,60),("61-75'",61,75),("76-90'",76,90),("90+'",91,200)]
    gols_ev = [e for e in events if e["event_type"] == "Gol" and e.get("minute") is not None]
    timing = [{"bloc": l, "gols": sum(1 for e in gols_ev if mn <= e["minute"] <= mx)}
              for l, mn, mx in BLOCS]

    # Enriquir historial amb info del rival (igual que modal_ultims_partits del Shiny)
    try:
        partits_eq = supabase.table("matches") \
            .select("jornada,local_team,away_team,goals_home,goals_away") \
            .eq("categoria", cat).eq("grup", grup).execute().data
    except Exception:
        partits_eq = []

    part_map = {p["jornada"]: p for p in partits_eq
                if p["local_team"] == team or p["away_team"] == team}

    for h in hist_raw:
        p = part_map.get(h["jornada"])
        if p:
            es_local = p["local_team"] == team
            h["rival"]      = p["away_team"] if es_local else p["local_team"]
            h["home_away"]  = "Casa" if es_local else "Fora"
            h["goals_team"] = p["goals_home"] if es_local else p["goals_away"]
            h["goals_opp"]  = p["goals_away"] if es_local else p["goals_home"]
        else:
            h["rival"] = "—"; h["home_away"] = "—"
            h["goals_team"] = None; h["goals_opp"] = None

    # Calcular targetes totals sumant l'historial (player_stats no les té per separat)
    yellow_total = sum(h.get("yellow_cards") or 0 for h in hist_raw)
    red_total    = sum(h.get("red_cards")    or 0 for h in hist_raw)

    return {
        "categoria": cat, "grup": grup, "jugador": jugador,
        "stats":  {**stats,
                   "rating":          rd.get("rating_global"),
                   "impacte":         round(im.get("impacte"), 2)         if im.get("impacte")                    is not None else None,
                   "avg_pts_jugant":  round(im.get("avg_points_when_playing"),  2) if im.get("avg_points_when_playing")  is not None else None,
                   "avg_pts_sense":   round(im.get("avg_points_when_not_playing"), 2) if im.get("avg_points_when_not_playing") is not None else None,
                   "games_played":    im.get("games_played", 0),
                   "games_not_played":im.get("games_not_played", 0),
                   "yellow_cards":    yellow_total,
                   "red_cards":       red_total,
                   },
        "radar":   ra,
        "historial": hist_raw,
        "events":    events,
        "timing_gols": timing,
    }


# ── Endpoint comparador ───────────────────────────────────────────────────────
@app.get("/jugadors/{categoria}/{grup}/comparador", tags=["Jugadors"])
def get_comparador_jugadors(categoria: str, grup: int, jug1: str, jug2: str):
    """Dades per comparar dos jugadors (crida l'endpoint individual per a cadascun)."""
    from urllib.parse import unquote
    cat = validar_categoria(categoria)
    d1 = get_jugador_complet(categoria, grup, unquote(jug1))
    d2 = get_jugador_complet(categoria, grup, unquote(jug2))
    return {"d1": d1, "d2": d2}

# ============================================================================
# ESTADÍSTIQUES — Tots els endpoints (3 pestanyes: Lliga, Equips, Jugadors)
# ============================================================================

@app.get("/estadistiques/{categoria}/{grup}", tags=["Estadístiques"])
def get_estadistiques(categoria: str, grup: int):
    """
    Totes les dades per les 3 pestanyes d'estadístiques.
    Inclou KPIs globals + totes les sèries de dades per cada gràfic.
    """
    cat = validar_categoria(categoria)
    import math
    from collections import defaultdict

    # ── Carregar dades base ───────────────────────────────────────────────────
    try:
        tms = supabase.table("team_match_stats") \
            .select("team,jornada,goals_for,goals_against,home_away,yellow_cards,red_cards") \
            .eq("categoria", cat).eq("grup", grup).limit(10000).execute().data
    except Exception as e:
        raise HTTPException(500, str(e))

    try:
        matches = supabase.table("matches") \
            .select("jornada,local_team,away_team,goals_home,goals_away") \
            .eq("categoria", cat).eq("grup", grup).limit(10000).execute().data
        jornades_jugades_stats = _jornades_jugades_reals(cat, grup)
        jugats = [m for m in matches if m["jornada"] in jornades_jugades_stats]
    except Exception:
        matches, jugats = [], []

    try:
        events = supabase.table("matches_events") \
            .select("event_type,minute,team,player") \
            .eq("categoria", cat).eq("grup", grup).limit(50000).execute().data
        gols_ev = [e for e in events if e["event_type"] == "Gol" and e.get("minute") is not None]
    except Exception:
        events, gols_ev = [], []

    try:
        pms = supabase.table("player_match_stats") \
            .select("player,team,jornada,goals,yellow_cards,red_cards,minutes_played") \
            .eq("categoria", cat).eq("grup", grup).limit(50000).execute().data
    except Exception:
        pms = []

    try:
        ps = supabase.table("player_stats") \
            .select("player,team,goals,total_minutes,matches_played,goals_per_90") \
            .eq("categoria", cat).eq("grup", grup).limit(10000).execute().data
    except Exception:
        ps = []

    # team_points per cada registre de tms
    for p in tms:
        p["team_points"] = 3 if p["goals_for"] > p["goals_against"] else (1 if p["goals_for"] == p["goals_against"] else 0)

    # ── KPIs globals ──────────────────────────────────────────────────────────
    total_gols     = len(gols_ev)
    n_jugats       = len(jugats)
    avg_gols       = round(total_gols / n_jugats, 2) if n_jugats else 0
    max_gols       = max((m["goals_home"] + m["goals_away"] for m in jugats), default=0)
    total_targetes = sum(p["yellow_cards"] + p["red_cards"] for p in pms)

    # ── TAB LLIGA ─────────────────────────────────────────────────────────────

    # 1. Distribució gols per minut (bins de 5')
    minut_bins = defaultdict(int)
    for e in gols_ev:
        b = min(int(e["minute"] // 5) * 5, 115)
        minut_bins[b] += 1
    gols_minut = [{"minut": k, "gols": v} for k, v in sorted(minut_bins.items())]

    # 2. Gols per jornada
    gols_j = defaultdict(int)
    for p in tms:
        gols_j[p["jornada"]] += p["goals_for"]
    gols_jornada = [{"jornada": j, "gols": v} for j, v in sorted(gols_j.items())]
    avg_gols_j = round(sum(v for _, v in gols_j.items()) / max(len(gols_j), 1), 1)

    # 3. Targetes per jornada
    targ_j = defaultdict(lambda: {"grogues": 0, "vermelles": 0})
    for p in tms:
        targ_j[p["jornada"]]["grogues"]  += p["yellow_cards"]
        targ_j[p["jornada"]]["vermelles"] += p["red_cards"]
    targetes_jornada = [{"jornada": j, **v} for j, v in sorted(targ_j.items())]

    # 4. Resultats més comuns
    res_count = defaultdict(int)
    for m in jugats:
        res_count[f"{m['goals_home']}-{m['goals_away']}"] += 1
    resultats_comuns = sorted([{"resultat": k, "freq": v} for k, v in res_count.items()],
                               key=lambda x: -x["freq"])[:12]
    for r in resultats_comuns:
        parts = r["resultat"].split("-")
        h, a = int(parts[0]), int(parts[1])
        r["color"] = "#27ae60" if h > a else ("#e74c3c" if h < a else "#95a5a6")

    # ── TAB EQUIPS ────────────────────────────────────────────────────────────

    # 5. Gols a favor vs en contra per equip
    equip_gols = defaultdict(lambda: {"GF": 0, "GC": 0})
    for p in tms:
        equip_gols[p["team"]]["GF"] += p["goals_for"]
        equip_gols[p["team"]]["GC"] += p["goals_against"]
    gols_equip = sorted([{"team": t, **v} for t, v in equip_gols.items()],
                        key=lambda x: -x["GF"])

    # 6. Casa vs Fora — punts promig
    equip_cf = defaultdict(lambda: {"Casa": [], "Fora": []})
    for p in tms:
        k = "Casa" if p["home_away"] == "Home" else "Fora"
        equip_cf[p["team"]][k].append(p["team_points"])
    casa_fora = []
    for t, cf in equip_cf.items():
        avg_c = round(sum(cf["Casa"]) / max(len(cf["Casa"]), 1), 2)
        avg_f = round(sum(cf["Fora"]) / max(len(cf["Fora"]), 1), 2)
        casa_fora.append({"team": t, "casa": avg_c, "fora": avg_f})
    casa_fora.sort(key=lambda x: -(x["casa"] + x["fora"]))

    # 7. Targetes per equip
    equip_targ = defaultdict(lambda: {"grogues": 0, "vermelles": 0})
    for p in tms:
        equip_targ[p["team"]]["grogues"]  += p["yellow_cards"]
        equip_targ[p["team"]]["vermelles"] += p["red_cards"]
    targetes_equip = sorted([{"team": t, **v} for t, v in equip_targ.items()],
                             key=lambda x: -(x["grogues"] + x["vermelles"]))

    # 8. Targetes vs Punts (scatter)
    equip_tp = defaultdict(lambda: {"targetes": 0, "punts": 0})
    for p in tms:
        equip_tp[p["team"]]["targetes"] += p["yellow_cards"] + 3 * p["red_cards"]
        equip_tp[p["team"]]["punts"]    += p["team_points"]
    targetes_punts = [{"team": t, **v} for t, v in equip_tp.items()]

    # 9. Ranking millors atacs i defenses (latents)
    global_avg = sum(p["goals_for"] for p in tms) / max(len(tms), 1)
    equip_lat = defaultdict(lambda: {"n": 0, "gf": 0, "gc": 0})
    for p in tms:
        equip_lat[p["team"]]["n"]  += 1
        equip_lat[p["team"]]["gf"] += p["goals_for"]
        equip_lat[p["team"]]["gc"] += p["goals_against"]
    latents = []
    for t, v in equip_lat.items():
        n = v["n"]
        avg_gf = v["gf"] / n
        avg_gc = v["gc"] / n
        shrink = n / (n + 5)
        attack  = (math.log(avg_gf + 0.1) - math.log(global_avg + 0.1)) * shrink
        defense = -(math.log(avg_gc + 0.1) - math.log(global_avg + 0.1)) * shrink
        latents.append({"team": t, "n_matches": n,
                        "attack": round(attack, 3), "defense": round(defense, 3),
                        "avg_gf": round(avg_gf, 2), "avg_gc": round(avg_gc, 2)})

    # Color per quadrant latents
    for l in latents:
        if l["attack"] >= 0 and l["defense"] >= 0:
            l["color"] = "#27ae60"
        elif l["attack"] >= 0:
            l["color"] = "#e67e22"
        elif l["defense"] >= 0:
            l["color"] = "#3498db"
        else:
            l["color"] = "#e74c3c"

    # 10. Rating + Tilt per scatter
    # Reutilitzar _calc_jugadors per obtenir tilt si el tenim
    # Rating dels equips
    rng_min_lat = min((l["attack"] + l["defense"]) / 2 for l in latents) if latents else 0
    rng_max_lat = max((l["attack"] + l["defense"]) / 2 for l in latents) if latents else 1
    for l in latents:
        raw = (l["attack"] + l["defense"]) / 2
        if rng_max_lat > rng_min_lat:
            l["rating"] = round((raw - rng_min_lat) / (rng_max_lat - rng_min_lat) * 100)
        else:
            l["rating"] = 50

    # Tilt per equip
    DRAW_P = 20 / 3
    rating_lkp = {l["team"]: l["rating"] for l in latents}
    tilt_data = {}
    tms_per_equip = defaultdict(list)
    for p in tms:
        tms_per_equip[p["team"]].append(p)
    for team, partits in tms_per_equip.items():
        partits_sorted = sorted(partits, key=lambda x: x["jornada"])
        recent = partits_sorted[-5:]
        if not recent:
            continue
        weights = [2**i for i in range(len(recent))]
        surprises = []
        for p in recent:
            r_eq  = rating_lkp.get(team, 50)
            r_opp = rating_lkp.get(p.get("opponent", ""), 50) if p.get("opponent") else 50
            si = 10**(r_eq / 50); sj = 10**(r_opp / 50); denom = si + sj + DRAW_P
            pts_exp = 3 * (si / denom) + 1 * (DRAW_P / denom)
            surprises.append(p["team_points"] - pts_exp)
        tilt = sum(s * w for s, w in zip(surprises, weights)) / sum(weights)
        tilt_data[team] = round(tilt, 2)

    # Ranking atac i defensa
    ranking_atac    = sorted(latents, key=lambda x: -x["attack"])
    ranking_defensa = sorted(latents, key=lambda x: -x["defense"])

    # ── TAB JUGADORS ──────────────────────────────────────────────────────────

    # 11. Evolució top 8 golejadors
    # 8 queries paral·leles (concurrent.futures) en lloc de 8 seqüencials
    top8 = sorted(ps, key=lambda x: -(x.get("goals") or 0))[:8]

    def fetch_player_hist(nom):
        try:
            return nom, supabase.table("player_match_stats") \
                .select("jornada,goals") \
                .eq("categoria", cat).eq("grup", grup).eq("player", nom) \
                .order("jornada").limit(500).execute().data
        except Exception:
            return nom, []

    from concurrent.futures import ThreadPoolExecutor
    top10_evolucio = []
    with ThreadPoolExecutor(max_workers=8) as ex:
        futures = {ex.submit(fetch_player_hist, j["player"]): j["player"] for j in top8}
        for fut in futures:
            nom, hist = fut.result()
            cum = 0
            for h in sorted(hist, key=lambda x: int(x["jornada"])):
                cum += h.get("goals") or 0
                top10_evolucio.append({
                    "player":    nom,
                    "jornada":   int(h["jornada"]),
                    "gols_acum": cum,
                })

    # 12. Top 10 targetes
    # pms ja té TOTES les files amb limit(50000) — agreguem directament en Python.
    # Igual que com es fa a la fitxa individual del jugador (yellow_cards + red_cards).
    targ_agg = {}
    for p in pms:
        nom = p.get("player") or ""
        if not nom:
            continue
        if nom not in targ_agg:
            targ_agg[nom] = {"grogues": 0, "vermelles": 0}
        targ_agg[nom]["grogues"]  += (p.get("yellow_cards") or 0)
        targ_agg[nom]["vermelles"] += (p.get("red_cards")   or 0)
    top_targ = sorted(
        [{"player": k, "grogues": v["grogues"], "vermelles": v["vermelles"],
          "total": v["grogues"] + v["vermelles"] * 2}
         for k, v in targ_agg.items() if v["grogues"] + v["vermelles"] > 0],
        key=lambda x: -x["total"]
    )[:10]

    # 13. Scatter: Gols vs Minuts (mínim 2 gols) + regressió lineal
    scatter_gols = [{"player": p["player"], "team": p["team"],
                     "goals": p["goals"], "total_minutes": p["total_minutes"],
                     "goals_per_90": round(p["goals_per_90"], 2)}
                    for p in ps if p["goals"] >= 2 and p["total_minutes"] > 0]
    scatter_gols.sort(key=lambda x: -x["goals"])
    # Regressió lineal simple
    if len(scatter_gols) >= 2:
        xs = [p["total_minutes"] for p in scatter_gols]
        ys = [p["goals"] for p in scatter_gols]
        n_s = len(xs)
        mx, my = sum(xs) / n_s, sum(ys) / n_s
        num = sum((xs[i] - mx) * (ys[i] - my) for i in range(n_s))
        den = sum((xs[i] - mx) ** 2 for i in range(n_s))
        slope = num / den if den else 0
        intercept = my - slope * mx
        x_min, x_max = min(xs), max(xs)
        reg_line = [{"x": x_min, "y": round(intercept + slope * x_min, 2)},
                    {"x": x_max, "y": round(intercept + slope * x_max, 2)}]
    else:
        reg_line = []

    # 14. Eficiència golejadora (minuts per gol, top 15, mínim 2 gols)
    eficiencia = sorted(
        [{"player": p["player"], "team": p["team"],
          "goals": p["goals"], "total_minutes": p["total_minutes"],
          "min_per_gol": round(p["total_minutes"] / p["goals"], 1)}
         for p in ps if p["goals"] >= 2 and p["total_minutes"] > 0],
        key=lambda x: x["min_per_gol"]
    )[:15]

    return {
        "categoria": cat, "grup": grup,
        # KPIs
        "kpis": {
            "total_gols": total_gols, "avg_gols": avg_gols,
            "max_gols": max_gols,     "total_targetes": total_targetes,
            "n_partits_jugats": n_jugats,
        },
        # Tab Lliga
        "gols_minut":      gols_minut,
        "gols_jornada":    gols_jornada,    "avg_gols_jornada": avg_gols_j,
        "targetes_jornada": targetes_jornada,
        "resultats_comuns": resultats_comuns,
        # Tab Equips
        "gols_equip":     gols_equip,
        "casa_fora":      casa_fora,
        "targetes_equip": targetes_equip,
        "targetes_punts": targetes_punts,
        "ranking_atac":   ranking_atac,
        "ranking_defensa":ranking_defensa,
        "latents":        latents,
        "tilt_data":      [{"team": t, "tilt": v} for t, v in tilt_data.items()],
        # Tab Jugadors
        "top10_evolucio": top10_evolucio,
        "top_targetes":   top_targ,
        "scatter_gols":   scatter_gols,
        "reg_line":       reg_line,
        "eficiencia":     eficiencia,
    }


# ============================================================================
# PARTITS — Endpoints complets (jornades, acta, prèvia)
# ============================================================================

def _is_jugat(goals_home, goals_away) -> bool:
    """
    Determina si un partit ha estat jugat.
    A Supabase, els partits no jugats poden tenir goals_home=NULL (None en Python)
    o goals_home=0 si hi ha hagut un error d'inserció.
    Usem team_match_stats com a font de veritat: si el partit apareix allà, és jugat.
    Com que no podem cridar team_match_stats des d'aquí, usem la convenció:
    goals_home is not None → jugat. Els partits amb 0-0 real SÍ estaran a matches_events.
    """
    return goals_home is not None


def _jornades_jugades_reals(cat: str, grup: int) -> set:
    """
    Retorna el set de jornades que realment s'han jugat.
    Usa team_match_stats com a font de veritat (només té partits jugats).
    Limit=5000 per evitar el tall de 1000 files per defecte de Supabase.
    """
    try:
        res = supabase.table("team_match_stats") \
            .select("jornada") \
            .eq("categoria", cat).eq("grup", grup) \
            .limit(5000).execute()
        return set(r["jornada"] for r in res.data)
    except Exception:
        return set()


@app.get("/partits/{categoria}/{grup}/jornades", tags=["Partits"])
def get_jornades(categoria: str, grup: int):
    """Llista de jornades disponibles i la jornada actual (màxima jugada)."""
    cat = validar_categoria(categoria)
    try:
        res = supabase.table("matches") \
            .select("jornada") \
            .eq("categoria", cat).eq("grup", grup).execute()
        data = res.data
    except Exception as e:
        raise HTTPException(500, str(e))
    jornades = sorted(set(r["jornada"] for r in data))
    # Usem team_match_stats com a font de veritat per saber quines jornades s'han jugat
    jornades_jugades = _jornades_jugades_reals(cat, grup)
    jornada_actual = max(jornades_jugades) if jornades_jugades else (jornades[-1] if jornades else 1)
    return {"jornades": jornades, "jornada_actual": jornada_actual}


@app.get("/partits/{categoria}/{grup}/jornada/{jornada}", tags=["Partits"])
def get_partits_jornada(categoria: str, grup: int, jornada: int):
    """
    Partits d'una jornada. Estratègia robusta per detectar si és jugat:
    1. Carrega team_match_stats de TOTA la temporada (sense filtre jornada) → índex local
    2. Un partit és jugat si l'equip local apareix a team_match_stats amb la jornada correcta
    3. El marcador ve de team_match_stats (goals_for/goals_against de l'equip local)
    Això evita: (a) limits de 1000 files en subquery, (b) goals_home=0 per no jugats
    """
    cat = validar_categoria(categoria)

    # ── Partits de la jornada ─────────────────────────────────────────────────
    try:
        res_m = supabase.table("matches") \
            .select("jornada,local_team,away_team,goals_home,goals_away,venue") \
            .eq("categoria", cat).eq("grup", grup).eq("jornada", jornada) \
            .order("local_team").execute()
        partits = res_m.data
    except Exception as e:
        raise HTTPException(500, str(e))

    # ── Info addicional ───────────────────────────────────────────────────────
    try:
        res_i = supabase.table("matches_info") \
            .select("home_team,away_team,date,time,referee") \
            .eq("categoria", cat).eq("grup", grup).eq("jornada", jornada).execute()
        info_map = {(r["home_team"], r["away_team"]): r for r in res_i.data}
    except Exception:
        info_map = {}

    # ── team_match_stats: índex (team, jornada) → {goals_for, goals_against} ──
    # Carrega TOTA la temporada per evitar el límit de Supabase en subqueries
    try:
        res_tms = supabase.table("team_match_stats") \
            .select("team,jornada,goals_for,goals_against") \
            .eq("categoria", cat).eq("grup", grup) \
            .limit(5000).execute()
        # Índex: (team, jornada) → fila
        tms_idx = {(r["team"], r["jornada"]): r for r in res_tms.data}
        # Set d'equips que han jugat en AQUESTA jornada concreta
        equips_j = set(r["team"] for r in res_tms.data if r["jornada"] == jornada)
    except Exception:
        tms_idx   = {}
        equips_j  = set()

    result = []
    for p in partits:
        info    = info_map.get((p["local_team"], p["away_team"]), {})
        # Jugat = AMBDÓS equips apareixen a team_match_stats per aquesta jornada
        jugat   = p["local_team"] in equips_j and p["away_team"] in equips_j
        if jugat:
            row_h      = tms_idx.get((p["local_team"], jornada))
            goals_home = row_h["goals_for"]      if row_h else None
            goals_away = row_h["goals_against"]  if row_h else None
        else:
            goals_home = None
            goals_away = None
        result.append({
            "local_team": p["local_team"],
            "away_team":  p["away_team"],
            "goals_home": goals_home,
            "goals_away": goals_away,
            "venue":   p.get("venue") or "",
            "date":    info.get("date") or "",
            "time":    info.get("time") or "",
            "referee": info.get("referee") or "",
            "jugat":   jugat,
        })
    return {"categoria": cat, "grup": grup, "jornada": jornada, "partits": result}


@app.get("/partits/{categoria}/{grup}/acta", tags=["Partits"])
def get_acta_partit(categoria: str, grup: int,
                    home: str = Query(...), away: str = Query(...), jornada: int = Query(...)):
    """
    Acta completa d'un partit jugat.
    Estratègia robusta: filtra per (categoria, grup, jornada) i després filtra
    localment per equips, evitant problemes amb caràcters especials i limits de Supabase.
    """
    cat = validar_categoria(categoria)
    from urllib.parse import unquote
    home_t, away_t = unquote(home), unquote(away)

    # ── Marcador des de team_match_stats ──────────────────────────────────────
    # Filtra per jornada + equip local directament (query simple i fiable)
    goals_home, goals_away = None, None
    try:
        res_h = supabase.table("team_match_stats") \
            .select("goals_for,goals_against") \
            .eq("categoria", cat).eq("grup", grup) \
            .eq("jornada", jornada).eq("team", home_t).limit(10000).execute()
        if res_h.data:
            goals_home = res_h.data[0]["goals_for"]
            goals_away = res_h.data[0]["goals_against"]
    except Exception:
        pass

    # Si no ha funcionat, provar amb l'equip visitant (per si el nom local té accent)
    if goals_home is None:
        try:
            res_a = supabase.table("team_match_stats") \
                .select("goals_for,goals_against") \
                .eq("categoria", cat).eq("grup", grup) \
                .eq("jornada", jornada).eq("team", away_t).limit(10000).execute()
            if res_a.data:
                goals_away = res_a.data[0]["goals_for"]
                goals_home = res_a.data[0]["goals_against"]
        except Exception:
            pass

    # ── Info addicional ───────────────────────────────────────────────────────
    extra_info = {}
    try:
        res_i = supabase.table("matches_info") \
            .select("date,time,referee") \
            .eq("categoria", cat).eq("grup", grup).eq("jornada", jornada) \
            .execute()
        # Filtrar localment (evita problemes amb caràcters especials en noms)
        for r in res_i.data:
            if r.get("home_team","").strip() == home_t.strip() or \
               r.get("away_team","").strip() == away_t.strip():
                extra_info = r
                break
        if not extra_info and res_i.data:
            # Fallback: primer resultat de la jornada si no coincideix exactament
            extra_info = res_i.data[0]
    except Exception:
        pass

    # ── Venue ─────────────────────────────────────────────────────────────────
    venue = ""
    try:
        res_m = supabase.table("matches") \
            .select("venue") \
            .eq("categoria", cat).eq("grup", grup) \
            .eq("jornada", jornada).eq("local_team", home_t) \
            .execute()
        venue = (res_m.data[0].get("venue") or "") if res_m.data else ""
    except Exception:
        pass

    # ── Alineacions ─────────────────────────────────────────────────────────
    # La taula "matches_lineups" NO existeix a Supabase.
    # Reconstruïm l'alineació des de player_match_stats (taula confirmada):
    #   starter=1 → Titular
    #   starter=0 AND minutes_played>0 → Suplent que va entrar
    #   starter=0 AND minutes_played=0 → Suplent que no va entrar (no el mostrem)
    def fetch_lineup(team_name):
        try:
            res = supabase.table("player_match_stats") \
                .select("player,starter,minutes_played,goals,yellow_cards,red_cards") \
                .eq("categoria", cat).eq("grup", grup) \
                .eq("jornada", jornada).eq("team", team_name) \
                .limit(30).execute()
            players = []
            for p in (res.data or []):
                mins = p.get("minutes_played") or 0
                start = p.get("starter") or 0
                if start == 1 or mins > 0:
                    # Construir stats textuals
                    stats_parts = []
                    if (p.get("goals") or 0) > 0:
                        stats_parts.append(f"{p['goals']} gol(s)")
                    if (p.get("yellow_cards") or 0) > 0:
                        stats_parts.append("Groga")
                    if (p.get("red_cards") or 0) > 0:
                        stats_parts.append("Vermella")
                    if start == 1 and mins < 88 and mins > 0:
                        stats_parts.append(f"Substituït m.{mins}'")
                    elif start == 0 and mins > 0:
                        stats_parts.append(f"Entrada m.{90-mins}'")
                    players.append({
                        "player":        p["player"],
                        "shirt_number":  "",
                        "position":      "Titular" if start == 1 else "Suplent",
                        "stats":         ", ".join(stats_parts),
                        "minutes_played": mins,
                    })
            return players
        except Exception:
            return []

    lineup_h_raw = fetch_lineup(home_t)
    lineup_a_raw = fetch_lineup(away_t)

    def build_lineup(raw):
        # Ordenar: titulars per minuts desc, suplents per minuts desc
        titulars = sorted([p for p in raw if p["position"] == "Titular"],
                          key=lambda x: -(x.get("minutes_played") or 0))
        suplents = sorted([p for p in raw if p["position"] == "Suplent"],
                          key=lambda x: -(x.get("minutes_played") or 0))
        return {"titulars": titulars, "suplents": suplents}

    # ── Esdeveniments ─────────────────────────────────────────────────────────
    # Filtra per (categoria, grup, jornada, home_team) — usa home_team que és menys ambigua
    # i filtra localment per seguretat
    events = []
    try:
        # Intentar filtrar per home_team directament
        res_ev = supabase.table("matches_events") \
            .select("event_type,minute,team,player,detail") \
            .eq("categoria", cat).eq("grup", grup) \
            .eq("jornada", jornada).eq("home_team", home_t) \
            .limit(200).execute()
        events = res_ev.data or []
    except Exception:
        events = []

    # Fallback: si no hi ha events, filtrar per equip local o visitant
    if not events:
        try:
            res_ev2 = supabase.table("matches_events") \
                .select("event_type,minute,team,player,detail,home_team,away_team") \
                .eq("categoria", cat).eq("grup", grup) \
                .eq("jornada", jornada).eq("team", home_t) \
                .limit(100).execute()
            ev_h = res_ev2.data or []
            res_ev3 = supabase.table("matches_events") \
                .select("event_type,minute,team,player,detail,home_team,away_team") \
                .eq("categoria", cat).eq("grup", grup) \
                .eq("jornada", jornada).eq("team", away_t) \
                .limit(100).execute()
            ev_a = res_ev3.data or []
            events = ev_h + ev_a
        except Exception:
            events = []

    events.sort(key=lambda x: x.get("minute") or 0)

    return {
        "categoria": cat, "grup": grup, "jornada": jornada,
        "local_team": home_t, "away_team": away_t,
        "goals_home": goals_home,
        "goals_away": goals_away,
        "venue":   venue,
        "date":    extra_info.get("date") or "",
        "time":    extra_info.get("time") or "",
        "referee": extra_info.get("referee") or "",
        "lineup_home": build_lineup(lineup_h_raw),
        "lineup_away": build_lineup(lineup_a_raw),
        "events": events,
    }


@app.get("/partits/{categoria}/{grup}/previa", tags=["Partits"])
def get_previa_partit(categoria: str, grup: int,
                      home: str = Query(...), away: str = Query(...)):
    """
    Prèvia d'un partit no jugat: classificació, gols, targetes,
    top golejadors, top per rating, radars d'equip i gols per franja.
    """
    cat = validar_categoria(categoria)
    from urllib.parse import unquote
    import math
    from collections import defaultdict
    home_t, away_t = unquote(home), unquote(away)

    # Standings actuals
    try:
        res_std = supabase.table("standings_by_round") \
            .select("team,position,points,played,wins,draws,losses,goals_for,goals_against") \
            .eq("categoria", cat).eq("grup", grup).execute()
        std_all = res_std.data
        # Usar team_match_stats per saber la jornada màxima real
        jmax_real = max(_jornades_jugades_reals(cat, grup), default=0)
        j_max = max((r["jornada"] for r in std_all if "jornada" in r), default=jmax_real) if std_all else jmax_real
        std = {r["team"]: r for r in std_all if r.get("jornada") == j_max} \
            if any("jornada" in r for r in std_all) else {r["team"]: r for r in std_all}
        if not std:
            std = {r["team"]: r for r in std_all}
    except Exception:
        std = {}

    # Team match stats (per gols, targetes, punts)
    try:
        tms = supabase.table("team_match_stats") \
            .select("team,jornada,goals_for,goals_against,home_away,yellow_cards,red_cards") \
            .eq("categoria", cat).eq("grup", grup).limit(10000).execute().data
        for p in tms:
            p["team_points"] = 3 if p["goals_for"] > p["goals_against"] else (1 if p["goals_for"] == p["goals_against"] else 0)
    except Exception:
        tms = []

    def team_stats(team):
        d = [p for p in tms if p["team"] == team]
        n = max(len(d), 1)
        return {
            "gf": sum(p["goals_for"] for p in d),
            "gc": sum(p["goals_against"] for p in d),
            "gf_pg": round(sum(p["goals_for"] for p in d) / n, 2),
            "gc_pg": round(sum(p["goals_against"] for p in d) / n, 2),
            "yellows": sum(p["yellow_cards"] for p in d),
            "reds": sum(p["red_cards"] for p in d),
            "y_pg": round(sum(p["yellow_cards"] for p in d) / n, 2),
            "n": n,
        }

    # Player stats top 5 golejadors
    try:
        ps_h = supabase.table("player_stats") \
            .select("player,goals,goals_per_90") \
            .eq("categoria", cat).eq("grup", grup).eq("team", home_t) \
            .gt("goals", 0).order("goals", desc=True).limit(5).execute().data
        ps_a = supabase.table("player_stats") \
            .select("player,goals,goals_per_90") \
            .eq("categoria", cat).eq("grup", grup).eq("team", away_t) \
            .gt("goals", 0).order("goals", desc=True).limit(5).execute().data
    except Exception:
        ps_h, ps_a = [], []

    # Top jugadors per rating
    impact_data, ratings_data, radar_data = _calc_jugadors(cat, grup)
    def top_rating(team, n=5):
        rated = [(k, v) for k, v in ratings_data.items() if k[1] == team]
        rated.sort(key=lambda x: -(x[1].get("rating_global") or 0))
        return [{"player": k[0], "rating": v.get("rating_global")} for k, v in rated[:n]]

    # Radar d'equip (calculate_team_radar del Shiny)
    def calc_team_radar(team):
        global_avg = sum(p["goals_for"] for p in tms) / max(len(tms), 1)
        d = [p for p in tms if p["team"] == team]
        if not d: return {k: 50 for k in ["atac","defensa","casa","fora","fairplay","primera_part","segona_part"]}
        n = len(d)
        avg_gf = sum(p["goals_for"] for p in d) / n
        avg_gc = sum(p["goals_against"] for p in d) / n
        shrink = n / (n + 5)
        attack  = (math.log(avg_gf + 0.1) - math.log(global_avg + 0.1)) * shrink
        defense = -(math.log(avg_gc + 0.1) - math.log(global_avg + 0.1)) * shrink
        pts_h = [p["team_points"] for p in d if p["home_away"] == "Home"]
        pts_a = [p["team_points"] for p in d if p["home_away"] == "Away"]
        cards = [p["yellow_cards"] + p["red_cards"] * 3 for p in d]
        return {
            "_attack": attack, "_defense": defense,
            "_home":   sum(pts_h) / max(len(pts_h), 1),
            "_away":   sum(pts_a) / max(len(pts_a), 1),
            "_fair":   -sum(cards) / n,
        }

    # Calcular radar per tots els equips (per normalitzar)
    equips_unics = list(set(p["team"] for p in tms))
    raw_radars = {t: calc_team_radar(t) for t in equips_unics}

    def scale_radar_key(key):
        vals = [raw_radars[t].get(key, 0) for t in equips_unics]
        mn, mx = min(vals), max(vals)
        if mx == mn: return {t: 50 for t in equips_unics}
        return {t: round((raw_radars[t].get(key, 0) - mn) / (mx - mn) * 100, 1) for t in equips_unics}

    # Gols per franja (per radar de gols per minut)
    try:
        evs_home = supabase.table("matches_events") \
            .select("event_type,minute,team") \
            .eq("categoria", cat).eq("grup", grup).eq("home_team", home_t) \
            .eq("event_type", "Gol").limit(10000).execute().data
        evs_home += supabase.table("matches_events") \
            .select("event_type,minute,team") \
            .eq("categoria", cat).eq("grup", grup).eq("away_team", home_t) \
            .eq("event_type", "Gol").limit(10000).execute().data
        evs_away = supabase.table("matches_events") \
            .select("event_type,minute,team") \
            .eq("categoria", cat).eq("grup", grup).eq("home_team", away_t) \
            .eq("event_type", "Gol").limit(10000).execute().data
        evs_away += supabase.table("matches_events") \
            .select("event_type,minute,team") \
            .eq("categoria", cat).eq("grup", grup).eq("away_team", away_t) \
            .eq("event_type", "Gol").limit(10000).execute().data
    except Exception:
        evs_home, evs_away = [], []

    PERIODES = [("0-15'",0,15),("16-30'",16,30),("31-45'",31,45),
                ("46-60'",46,60),("61-75'",61,75),("76-90'",76,90),("90+'",91,200)]

    def gols_franja(evs, team):
        gols = [e for e in evs if e["team"] == team and e.get("minute") is not None]
        return [{"periode": l, "n": sum(1 for e in gols if mn <= e["minute"] <= mx)}
                for l, mn, mx in PERIODES]

    # Radar normalitzat per als dos equips
    radar_keys = [("atac","_attack"),("defensa","_defense"),("casa","_home"),
                  ("fora","_away"),("fairplay","_fair")]
    # afegir primera i segona part
    gols_1a = defaultdict(int); gols_2a = defaultdict(int)
    n_partits_t = defaultdict(int)
    try:
        all_gol_evs = supabase.table("matches_events") \
            .select("team,minute,event_type") \
            .eq("categoria", cat).eq("grup", grup).eq("event_type","Gol").limit(10000).execute().data
        for e in all_gol_evs:
            if e.get("minute") and e.get("team"):
                if e["minute"] <= 45: gols_1a[e["team"]] += 1
                else: gols_2a[e["team"]] += 1
        for p in tms: n_partits_t[p["team"]] += 1
    except Exception:
        pass

    for t in equips_unics:
        raw_radars[t]["_first"]  = gols_1a[t] / max(n_partits_t[t], 1)
        raw_radars[t]["_second"] = gols_2a[t] / max(n_partits_t[t], 1)

    sc = {}
    for rk, rawk in radar_keys + [("primera_part","_first"),("segona_part","_second")]:
        sc[rk] = scale_radar_key(rawk)

    def get_radar(team):
        return {rk: sc[rk].get(team, 50) for rk, _ in radar_keys + [("primera_part","_first"),("segona_part","_second")]}

    # Tilt (reutilitzem la lògica de l'endpoint d'estadístiques)
    DRAW_P = 20 / 3
    def calc_tilt(team):
        global_avg2 = sum(p["goals_for"] for p in tms) / max(len(tms), 1)
        equip_lat = defaultdict(lambda: {"n":0,"gf":0,"gc":0})
        for p in tms:
            equip_lat[p["team"]]["n"]  += 1
            equip_lat[p["team"]]["gf"] += p["goals_for"]
            equip_lat[p["team"]]["gc"] += p["goals_against"]
        latents = {}
        for t, v in equip_lat.items():
            n2 = v["n"]
            a = v["gf"]/n2; c = v["gc"]/n2
            shrink2 = n2/(n2+5)
            atk2  = (math.log(a+0.1)-math.log(global_avg2+0.1))*shrink2
            dfs2  = -(math.log(c+0.1)-math.log(global_avg2+0.1))*shrink2
            latents[t] = (atk2+dfs2)/2
        rng_mn2 = min(latents.values()); rng_mx2 = max(latents.values())
        rating_lkp2 = {t: round((v-rng_mn2)/(rng_mx2-rng_mn2)*100) if rng_mx2>rng_mn2 else 50
                       for t, v in latents.items()}
        partits = sorted([p for p in tms if p["team"]==team], key=lambda x:x["jornada"])
        recent = partits[-5:]
        if not recent: return None
        weights = [2**i for i in range(len(recent))]
        surprises = []
        for p in recent:
            opp = p.get("opponent","")
            r_eq  = rating_lkp2.get(team, 50)
            r_opp = rating_lkp2.get(opp, 50)
            si = 10**(r_eq/50); sj = 10**(r_opp/50); denom = si+sj+DRAW_P
            pts_exp = 3*(si/denom)+1*(DRAW_P/denom)
            surprises.append(p["team_points"] - pts_exp)
        return round(sum(s*w for s,w in zip(surprises,weights))/sum(weights), 2)

    tilt_h = calc_tilt(home_t)
    tilt_a = calc_tilt(away_t)

    return {
        "categoria": cat, "grup": grup,
        "local_team": home_t, "away_team": away_t,
        "standings": {
            "home": std.get(home_t, {}),
            "away": std.get(away_t, {}),
        },
        "tilt": {"home": tilt_h, "away": tilt_a},
        "stats": {
            "home": team_stats(home_t),
            "away": team_stats(away_t),
        },
        "golejadors": {"home": ps_h, "away": ps_a},
        "top_rating":  {"home": top_rating(home_t), "away": top_rating(away_t)},
        "radar": {
            "home": get_radar(home_t),
            "away": get_radar(away_t),
        },
        "gols_franja": {
            "home": gols_franja(evs_home, home_t),
            "away": gols_franja(evs_away, away_t),
        },
    }