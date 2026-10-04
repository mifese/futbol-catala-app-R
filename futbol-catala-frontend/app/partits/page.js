"use client";
import { useEffect, useState, useCallback } from "react";
import { useGrup } from "../components/GrupContext";

const API = process.env.NEXT_PUBLIC_API_URL || "http://localhost:8000";
const r2 = (v) => v != null ? Math.round(v * 100) / 100 : "—";

// ─── Radar equip 7 eixos ──────────────────────────────────────────────────────
function RadarEquipDual({ rh, ra, nomH, nomA }) {
  const labels = ["Atac","Defensa","Casa","Fora","Fair-Play","1a Part","2a Part"];
  const keys   = ["atac","defensa","casa","fora","fairplay","primera_part","segona_part"];
  const n = labels.length, cx = 210, cy = 200, R = 130;
  const angle = (i) => -Math.PI/2 + (2*Math.PI/n)*i;
  const pt = (i, pct) => ({ x: cx + R*(pct/100)*Math.cos(angle(i)), y: cy + R*(pct/100)*Math.sin(angle(i)) });
  const grid = (pct) => keys.map((_,i) => `${i===0?"M":"L"} ${pt(i,pct).x} ${pt(i,pct).y}`).join(" ") + " Z";
  const dp = (r) => keys.map((k,i) => `${i===0?"M":"L"} ${pt(i,r?.[k]??50).x} ${pt(i,r?.[k]??50).y}`).join(" ") + " Z";
  return (
    <svg viewBox="0 0 420 400" className="w-full">
      {[25,50,75,100].map(p => <path key={p} d={grid(p)} fill="none" stroke="#e2e8f0" strokeWidth={1}/>)}
      {keys.map((_,i) => { const e=pt(i,100); return <line key={i} x1={cx} y1={cy} x2={e.x} y2={e.y} stroke="#e2e8f0" strokeWidth={1}/>; })}
      <path d={dp(rh)} fill="rgba(26,107,138,0.2)" stroke="rgba(26,107,138,1)" strokeWidth={2}/>
      <path d={dp(ra)} fill="rgba(192,57,43,0.15)" stroke="rgba(192,57,43,1)" strokeWidth={2} strokeDasharray="5,3"/>
      {keys.map((k,i) => { const p=pt(i,rh?.[k]??50); return <circle key={`h-${k}`} cx={p.x} cy={p.y} r={4} fill="rgba(26,107,138,1)"/>; })}
      {keys.map((k,i) => { const p=pt(i,ra?.[k]??50); return <circle key={`a-${k}`} cx={p.x} cy={p.y} r={4} fill="rgba(192,57,43,1)"/>; })}
      {labels.map((l,i) => { const p=pt(i,R*1.24); return <text key={l} x={p.x} y={p.y} textAnchor="middle" dominantBaseline="middle" fontSize={11} fill="#475569" fontWeight="500">{l}</text>; })}
    </svg>
  );
}

// ─── Gols per franja bilateral ────────────────────────────────────────────────
function GolsFranja({ dataH, dataA, nomH, nomA }) {
  if (!dataH?.length) return null;
  const maxV = Math.max(...dataH.map(d=>d.n), ...dataA.map(d=>d.n), 1);
  const H = dataH.length * 40 + 40, W = 500, cx = W/2;
  const bW = (v) => (v/maxV) * (cx - 80);
  return (
    <svg viewBox={`0 0 ${W} ${H}`} className="w-full">
      <line x1={cx} x2={cx} y1={15} y2={H-15} stroke="#e2e8f0" strokeWidth={1}/>
      <text x={cx-8} y={12} textAnchor="end" fontSize={9} fill="#1a6b8a" fontWeight="600">← {nomH.substring(0,16)}</text>
      <text x={cx+8} y={12} textAnchor="start" fontSize={9} fill="#c0392b" fontWeight="600">{nomA.substring(0,16)} →</text>
      {dataH.map((d,i) => {
        const y = 20 + i*40, bh = 16;
        const w1 = bW(d.n), w2 = bW(dataA[i]?.n||0);
        return (
          <g key={i}>
            <text x={cx} y={y+bh/2+5} textAnchor="middle" fontSize={10} fill="#475569" fontWeight="500">{d.periode}</text>
            <rect x={cx-w1-40} y={y} width={w1} height={bh} fill="rgba(26,107,138,0.8)" rx={3}/>
            <text x={cx-w1-44} y={y+bh/2+5} textAnchor="end" fontSize={11} fontWeight="700" fill="rgba(26,107,138,1)">{d.n}</text>
            <rect x={cx+40} y={y} width={w2} height={bh} fill="rgba(192,57,43,0.8)" rx={3}/>
            <text x={cx+w2+44} y={y+bh/2+5} textAnchor="start" fontSize={11} fontWeight="700" fill="rgba(192,57,43,1)">{dataA[i]?.n||0}</text>
          </g>
        );
      })}
    </svg>
  );
}

// ─── Fila comparativa ─────────────────────────────────────────────────────────
const CompRow = ({ label, vh, va, invert=false }) => {
  const n1=parseFloat(vh), n2=parseFloat(va);
  const ok=!isNaN(n1)&&!isNaN(n2);
  const c1=ok?(invert?(n1<n2?"text-green-600":n1>n2?"text-red-500":"text-slate-600"):(n1>n2?"text-green-600":n1<n2?"text-red-500":"text-slate-600")):"text-slate-600";
  const c2=ok?(invert?(n2<n1?"text-green-600":n2>n1?"text-red-500":"text-slate-600"):(n2>n1?"text-green-600":n2<n1?"text-red-500":"text-slate-600")):"text-slate-600";
  return (
    <div className="flex items-center py-1.5 border-b border-slate-50 text-sm">
      <span className={`w-16 text-right font-bold ${c1}`}>{vh??""}</span>
      <span className="flex-1 text-center text-xs text-slate-400 px-1">{label}</span>
      <span className={`w-16 font-bold ${c2}`}>{va??""}</span>
    </div>
  );
};

// ─── Camp de futbol visual ────────────────────────────────────────────────────
function CampFutbol({ lineup, color, side }) {
  // Posicions aproximades dels 11 titulars en un camp
  // Formació 4-4-2 per defecte, ajustat pel nombre
  const positions = [
    {row:0,col:2},           // Porter
    {row:1,col:0},{row:1,col:1},{row:1,col:3},{row:1,col:4}, // Defenses
    {row:2,col:0},{row:2,col:1},{row:2,col:3},{row:2,col:4}, // Mig
    {row:3,col:1},{row:3,col:3},                              // Davanters
  ];
  const rows = [0.85, 0.65, 0.40, 0.18]; // posició vertical 0-1
  const cols5 = [0.08, 0.27, 0.5, 0.73, 0.92]; // posicions horitzontals

  const titulars = lineup?.titulars || [];
  const suplents = lineup?.suplents || [];

  return (
    <div className="relative">
      {/* Camp verd */}
      <svg viewBox="0 0 260 360" className="w-full">
        {/* Fons del camp */}
        <rect x={0} y={0} width={260} height={360} fill="#2d6a1f" rx={4}/>
        {/* Línies del camp */}
        <rect x={15} y={10} width={230} height={340} fill="none" stroke="white" strokeWidth={1.5} opacity={0.6}/>
        <line x1={15} x2={245} y1={180} y2={180} stroke="white" strokeWidth={1.5} opacity={0.6}/>
        <circle cx={130} cy={180} r={30} fill="none" stroke="white" strokeWidth={1.5} opacity={0.6}/>
        <circle cx={130} cy={180} r={3} fill="white" opacity={0.6}/>
        {/* Àrea gran local */}
        <rect x={65} y={10} width={130} height={55} fill="none" stroke="white" strokeWidth={1.2} opacity={0.5}/>
        <rect x={95} y={10} width={70} height={28} fill="none" stroke="white" strokeWidth={1.2} opacity={0.5}/>
        {/* Àrea gran visitant */}
        <rect x={65} y={295} width={130} height={55} fill="none" stroke="white" strokeWidth={1.2} opacity={0.5}/>
        <rect x={95} y={322} width={70} height={28} fill="none" stroke="white" strokeWidth={1.2} opacity={0.5}/>
        {/* Jugadors */}
        {titulars.slice(0,11).map((p, i) => {
          const pos = positions[i] || {row:2,col:2};
          const x = cols5[pos.col] * 260;
          const y = (side==="home" ? rows[pos.row] : 1-rows[pos.row]) * 360;
          const dorsal = p.shirt_number;
          return (
            <g key={i}>
              <circle cx={x} cy={y} r={14} fill={color} stroke="white" strokeWidth={2} opacity={0.92}/>
              <text x={x} y={y+1} textAnchor="middle" dominantBaseline="middle" fontSize={10} fill="white" fontWeight="900">{dorsal}</text>
              <text x={x} y={y+20} textAnchor="middle" fontSize={7.5} fill="white" fontWeight="600" opacity={0.9}>
                {(p.player||"").split(",")[0].substring(0,10)}
              </text>
            </g>
          );
        })}
      </svg>
    </div>
  );
}

// ─── Targeta de jugador a l'alineació ────────────────────────────────────────
function PlayerRow({ p, color }) {
  const hasCard = p.stats && p.stats !== "null";
  return (
    <div className="flex items-center gap-2 py-1.5 border-b border-slate-50 hover:bg-slate-50 transition-colors">
      <span className="w-7 h-7 rounded-full flex items-center justify-center text-xs font-black text-white shrink-0" style={{background:color}}>
        {p.shirt_number}
      </span>
      <span className="flex-1 text-sm text-slate-800">{p.player}</span>
      {hasCard && (
        <span className="text-xs text-slate-500 shrink-0">
          {p.stats?.includes("Groga") ? "🟨" : p.stats?.includes("Vermella") ? "🟥" : ""}
          {p.stats && !p.stats.includes("Groga") && !p.stats.includes("Vermella") ? p.stats : ""}
        </span>
      )}
    </div>
  );
}

// ─── Acta visual ─────────────────────────────────────────────────────────────
function ActaVisual({ data }) {
  const [vistaAlineacio, setVistaAlineacio] = useState("llista"); // "llista" | "camp"
  if (!data) return null;

  const { local_team, away_team, goals_home, goals_away, date, time, referee, venue, lineup_home, lineup_away, events } = data;
  const gh = goals_home ?? 0, ga = goals_away ?? 0;
  const result = gh > ga ? "home" : gh < ga ? "away" : "draw";

  const eventIcon = (type) => ({ "Gol": "⚽", "Substitució": "🔄", "Targeta Groga": "🟨", "Targeta Vermella": "🟥", "Groga": "🟨", "Vermella": "🟥" }[type] || "❓");
  const eventColor = (type) => ({ "Gol": "border-green-400 bg-green-50", "Substitució": "border-blue-300 bg-blue-50", "Targeta Groga": "border-yellow-400 bg-yellow-50", "Targeta Vermella": "border-red-500 bg-red-50", "Groga": "border-yellow-400 bg-yellow-50", "Vermella": "border-red-500 bg-red-50" }[type] || "border-slate-200 bg-slate-50");

  // Separar events per equip i per cronologia
  const evHome = events?.filter(e => e.team === local_team) || [];
  const evAway = events?.filter(e => e.team === away_team) || [];

  return (
    <div className="space-y-4">
      {/* Marcador gran */}
      <div className="bg-gradient-to-br from-slate-800 to-slate-900 text-white rounded-2xl p-6">
        <div className="grid grid-cols-3 items-center gap-4">
          <div className="text-right">
            <p className="font-bold text-lg leading-tight">{local_team}</p>
            <p className="text-xs text-slate-400 mt-0.5">🏠 Casa</p>
            {/* Gols home */}
            <div className="mt-2 flex flex-wrap gap-1 justify-end">
              {evHome.filter(e=>e.event_type==="Gol").map((e,i)=>(
                <span key={i} className="text-xs text-green-400">⚽{e.minute}'</span>
              ))}
            </div>
          </div>
          <div className="text-center">
            <div className="flex items-center justify-center gap-3">
              <span className={`text-6xl font-black ${result==="home"?"text-green-400":result==="draw"?"text-yellow-400":"text-slate-400"}`}>{gh}</span>
              <span className="text-slate-500 text-2xl">-</span>
              <span className={`text-6xl font-black ${result==="away"?"text-green-400":result==="draw"?"text-yellow-400":"text-slate-400"}`}>{ga}</span>
            </div>
            {(date||time) && <p className="text-xs text-slate-400 mt-2">{date}{time?` · ${time}`:""}</p>}
            {referee && <p className="text-xs text-slate-500 mt-0.5">🏴 {referee}</p>}
            {venue && <p className="text-xs text-slate-500 mt-0.5">📍 {venue.substring(0,30)}</p>}
          </div>
          <div className="text-left">
            <p className="font-bold text-lg leading-tight">{away_team}</p>
            <p className="text-xs text-slate-400 mt-0.5">✈️ Fora</p>
            <div className="mt-2 flex flex-wrap gap-1">
              {evAway.filter(e=>e.event_type==="Gol").map((e,i)=>(
                <span key={i} className="text-xs text-green-400">⚽{e.minute}'</span>
              ))}
            </div>
          </div>
        </div>
      </div>

      {/* Alineacions */}
      <div>
        <div className="flex gap-2 mb-3">
          <button onClick={()=>setVistaAlineacio("llista")} className={`px-3 py-1.5 rounded-lg text-xs font-medium transition-all ${vistaAlineacio==="llista"?"bg-slate-800 text-white":"bg-slate-100 text-slate-600 hover:bg-slate-200"}`}>📋 Llista</button>
          <button onClick={()=>setVistaAlineacio("camp")}  className={`px-3 py-1.5 rounded-lg text-xs font-medium transition-all ${vistaAlineacio==="camp" ?"bg-slate-800 text-white":"bg-slate-100 text-slate-600 hover:bg-slate-200"}`}>⚽ Camp</button>
        </div>

        {vistaAlineacio === "camp" ? (
          <div className="grid grid-cols-2 gap-4">
            <div>
              <p className="text-xs font-semibold text-[#1a6b8a] text-center mb-2">{local_team}</p>
              <CampFutbol lineup={lineup_home} color="#1a6b8a" side="home"/>
            </div>
            <div>
              <p className="text-xs font-semibold text-[#c0392b] text-center mb-2">{away_team}</p>
              <CampFutbol lineup={lineup_away} color="#c0392b" side="away"/>
            </div>
          </div>
        ) : (
          <div className="grid grid-cols-2 gap-4">
            {/* Local */}
            <div className="bg-white rounded-2xl border border-slate-200 overflow-hidden">
              <div className="bg-[#1a6b8a] text-white px-4 py-2.5">
                <p className="font-semibold text-sm">{local_team}</p>
                <p className="text-xs opacity-70">🏠 Casa</p>
              </div>
              <div className="p-3">
                <p className="text-xs font-semibold text-slate-400 uppercase tracking-wide mb-2">Titulars</p>
                {lineup_home?.titulars?.map((p,i) => <PlayerRow key={i} p={p} color="#1a6b8a"/>)}
                {lineup_home?.suplents?.length > 0 && (
                  <>
                    <p className="text-xs font-semibold text-slate-400 uppercase tracking-wide mt-3 mb-2">Suplents</p>
                    {lineup_home.suplents.map((p,i) => <PlayerRow key={i} p={p} color="#7fb3cc"/>)}
                  </>
                )}
              </div>
            </div>
            {/* Visitant */}
            <div className="bg-white rounded-2xl border border-slate-200 overflow-hidden">
              <div className="bg-[#c0392b] text-white px-4 py-2.5">
                <p className="font-semibold text-sm">{away_team}</p>
                <p className="text-xs opacity-70">✈️ Fora</p>
              </div>
              <div className="p-3">
                <p className="text-xs font-semibold text-slate-400 uppercase tracking-wide mb-2">Titulars</p>
                {lineup_away?.titulars?.map((p,i) => <PlayerRow key={i} p={p} color="#c0392b"/>)}
                {lineup_away?.suplents?.length > 0 && (
                  <>
                    <p className="text-xs font-semibold text-slate-400 uppercase tracking-wide mt-3 mb-2">Suplents</p>
                    {lineup_away.suplents.map((p,i) => <PlayerRow key={i} p={p} color="#d98080"/>)}
                  </>
                )}
              </div>
            </div>
          </div>
        )}
      </div>

      {/* Cronologia d'events */}
      {events?.length > 0 && (
        <div className="bg-white rounded-2xl border border-slate-200 overflow-hidden">
          <div className="bg-slate-50 px-4 py-2.5 border-b border-slate-200">
            <p className="text-sm font-semibold text-slate-700">📜 Cronologia del Partit</p>
          </div>
          <div className="p-3 space-y-1.5">
            {events.map((e, i) => (
              <div key={i} className={`flex items-start gap-3 p-2 rounded-lg border-l-4 ${eventColor(e.event_type)}`}>
                <span className="text-lg shrink-0">{eventIcon(e.event_type)}</span>
                <div className="flex-1 min-w-0">
                  <div className="flex items-center gap-2">
                    <span className="font-black text-slate-800 text-sm">{e.minute}'</span>
                    <span className="font-semibold text-slate-700 text-sm truncate">{e.player}</span>
                  </div>
                  <div className="flex items-center gap-2 mt-0.5">
                    <span className="text-xs text-slate-500">{e.team}</span>
                    {e.detail && e.detail !== "null" && <span className="text-xs text-slate-400">· {e.detail}</span>}
                  </div>
                </div>
              </div>
            ))}
          </div>
        </div>
      )}
    </div>
  );
}

// ─── Prèvia visual ────────────────────────────────────────────────────────────
function PreviaVisual({ data }) {
  if (!data) return null;
  const { local_team, away_team, standings, tilt, stats, golejadors, top_rating, radar, gols_franja } = data;
  const sh = standings?.home || {}, sa = standings?.away || {};

  const tiltColor = (v) => v == null ? "text-slate-400" : v > 0.3 ? "text-green-600" : v < -0.3 ? "text-red-500" : "text-yellow-600";

  return (
    <div className="space-y-4">
      {/* Capçalera prèvia */}
      <div className="bg-gradient-to-br from-slate-800 to-slate-900 text-white rounded-2xl p-6">
        <div className="grid grid-cols-3 items-center gap-4 text-center">
          <div>
            <p className="font-bold text-lg">{local_team}</p>
            <p className="text-slate-400 text-sm">🏠 Casa</p>
            <p className="text-3xl font-black text-[#3498db] mt-2">#{sh.position ?? "—"}</p>
            <p className="text-slate-300 text-sm">{sh.points ?? "—"} pts</p>
          </div>
          <div>
            <p className="text-4xl text-slate-500 font-light">VS</p>
            <p className="text-xs text-slate-500 mt-2">⚡ Prèvia</p>
          </div>
          <div>
            <p className="font-bold text-lg">{away_team}</p>
            <p className="text-slate-400 text-sm">✈️ Fora</p>
            <p className="text-3xl font-black text-[#e74c3c] mt-2">#{sa.position ?? "—"}</p>
            <p className="text-slate-300 text-sm">{sa.points ?? "—"} pts</p>
          </div>
        </div>
      </div>

      {/* 3 caixes comparatives */}
      <div className="grid grid-cols-1 md:grid-cols-3 gap-4">
        {/* Classificació */}
        <div className="bg-white rounded-2xl border border-slate-200 p-4">
          <p className="text-xs font-semibold text-slate-500 uppercase tracking-wide mb-2">📊 Classificació</p>
          <div className="flex text-xs text-slate-400 justify-between mb-1 px-1">
            <span className="truncate max-w-[80px]">{local_team.substring(0,12)}</span>
            <span className="truncate max-w-[80px] text-right">{away_team.substring(0,12)}</span>
          </div>
          <CompRow label="Posició"   vh={sh.position}        va={sa.position}        invert/>
          <CompRow label="Punts"     vh={sh.points}          va={sa.points}/>
          <CompRow label="PJ"        vh={sh.played}          va={sa.played}          invert/>
          <CompRow label="Victòries" vh={sh.wins}            va={sa.wins}/>
          <CompRow label="Empats"    vh={sh.draws}           va={sa.draws}           invert/>
          <CompRow label="Derrotes"  vh={sh.losses}          va={sa.losses}          invert/>
          {/* Tilt */}
          <div className="flex items-center py-1.5 mt-1 text-sm">
            <span className={`w-16 text-right font-bold ${tiltColor(tilt?.home)}`}>{tilt?.home != null ? (tilt.home > 0 ? `+${tilt.home}` : tilt.home) : "N/A"}</span>
            <span className="flex-1 text-center text-xs text-slate-400 px-1">⚡ Tilt</span>
            <span className={`w-16 font-bold ${tiltColor(tilt?.away)}`}>{tilt?.away != null ? (tilt.away > 0 ? `+${tilt.away}` : tilt.away) : "N/A"}</span>
          </div>
        </div>

        {/* Gols */}
        <div className="bg-white rounded-2xl border border-slate-200 p-4">
          <p className="text-xs font-semibold text-slate-500 uppercase tracking-wide mb-2">🥅 Gols</p>
          <div className="flex text-xs text-slate-400 justify-between mb-1 px-1">
            <span></span><span></span>
          </div>
          <CompRow label="Gols marcats"   vh={stats?.home?.gf}      va={stats?.away?.gf}/>
          <CompRow label="Gols/partit"    vh={r2(stats?.home?.gf_pg)} va={r2(stats?.away?.gf_pg)}/>
          <CompRow label="Gols rebuts"    vh={stats?.home?.gc}      va={stats?.away?.gc}      invert/>
          <CompRow label="Rebuts/partit"  vh={r2(stats?.home?.gc_pg)} va={r2(stats?.away?.gc_pg)} invert/>
          <CompRow label="Diferència"     vh={(stats?.home?.gf??0)-(stats?.home?.gc??0)} va={(stats?.away?.gf??0)-(stats?.away?.gc??0)}/>
        </div>

        {/* Targetes */}
        <div className="bg-white rounded-2xl border border-slate-200 p-4">
          <p className="text-xs font-semibold text-slate-500 uppercase tracking-wide mb-2">🟨 Targetes</p>
          <CompRow label="🟨 Grogues total" vh={stats?.home?.yellows}   va={stats?.away?.yellows}   invert/>
          <CompRow label="Grogues/partit"   vh={r2(stats?.home?.y_pg)}  va={r2(stats?.away?.y_pg)}  invert/>
          <CompRow label="🟥 Vermelles"     vh={stats?.home?.reds}      va={stats?.away?.reds}      invert/>
        </div>
      </div>

      {/* Top golejadors + Top per rating */}
      <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
        <div className="bg-white rounded-2xl border border-slate-200 p-4">
          <p className="text-sm font-semibold text-slate-700 mb-3">⭐ Top Golejadors</p>
          <div className="grid grid-cols-2 gap-3">
            {[[local_team, golejadors?.home, "#1a6b8a"],[away_team, golejadors?.away, "#c0392b"]].map(([nom,top,col])=>(
              <div key={nom}>
                <p className="text-xs font-semibold mb-1.5 truncate" style={{color:col}}>{nom}</p>
                {(top||[]).map((p,i)=>(
                  <div key={i} className="flex items-center gap-1.5 text-xs py-1 border-b border-slate-50">
                    <span className="font-bold text-green-600">{p.goals}⚽</span>
                    <span className="truncate text-slate-700">{p.player.split(",")[0].substring(0,18)}</span>
                  </div>
                ))}
              </div>
            ))}
          </div>
        </div>

        <div className="bg-white rounded-2xl border border-slate-200 p-4">
          <p className="text-sm font-semibold text-slate-700 mb-3">🏆 Millors Jugadors per Rating</p>
          <div className="grid grid-cols-2 gap-3">
            {[[local_team, top_rating?.home, "#1a6b8a"],[away_team, top_rating?.away, "#c0392b"]].map(([nom,top,col])=>(
              <div key={nom}>
                <p className="text-xs font-semibold mb-1.5 truncate" style={{color:col}}>{nom}</p>
                {(top||[]).map((p,i)=>(
                  <div key={i} className="flex items-center gap-1.5 text-xs py-1 border-b border-slate-50">
                    <span className="font-bold text-[#1a6b8a]">★{p.rating}</span>
                    <span className="truncate text-slate-700">{p.player.split(",")[0].substring(0,18)}</span>
                  </div>
                ))}
              </div>
            ))}
          </div>
        </div>
      </div>

      {/* Radars i Gols per franja */}
      <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
        <div className="bg-white rounded-2xl border border-slate-200 p-4">
          <p className="text-sm font-semibold text-slate-700 mb-2">⬡ Radars comparats</p>
          <div className="flex gap-4 text-xs justify-center mb-2">
            <span className="flex items-center gap-1.5"><span className="w-3 h-3 rounded-full bg-[#1a6b8a] inline-block"/>{local_team.substring(0,18)}</span>
            <span className="flex items-center gap-1.5"><span className="w-3 h-3 rounded-full bg-[#c0392b] inline-block"/>{away_team.substring(0,18)}</span>
          </div>
          <RadarEquipDual rh={radar?.home} ra={radar?.away} nomH={local_team} nomA={away_team}/>
        </div>

        <div className="bg-white rounded-2xl border border-slate-200 p-4">
          <p className="text-sm font-semibold text-slate-700 mb-2">⏱ Quan es marquen els gols</p>
          <p className="text-xs text-slate-400 mb-3">Gols marcats per franja horària</p>
          <GolsFranja dataH={gols_franja?.home} dataA={gols_franja?.away} nomH={local_team} nomA={away_team}/>
        </div>
      </div>
    </div>
  );
}

// ─── Targeta de partit ────────────────────────────────────────────────────────
function PartitCard({ p, seleccionat, onClick }) {
  const jugat = p.jugat;
  const isSel = seleccionat?.local_team===p.local_team && seleccionat?.away_team===p.away_team;
  const gh = p.goals_home ?? 0, ga = p.goals_away ?? 0;
  return (
    <button onClick={onClick} className={`w-full text-left rounded-2xl border-2 p-3 mb-2 transition-all cursor-pointer ${isSel?"border-[#1a6b8a] bg-blue-50":"border-slate-200 bg-white hover:border-slate-300 hover:shadow-sm"}`}>
      <div className="flex items-center gap-2">
        {/* Equip local */}
        <div className="flex-1 text-right">
          <p className="font-bold text-sm text-slate-800 leading-tight">{p.local_team}</p>
          <p className="text-xs text-slate-400">Casa</p>
        </div>
        {/* Centre: marcador o hora */}
        <div className="flex-shrink-0 w-20 text-center">
          {jugat ? (
            <p className={`text-2xl font-black leading-none ${isSel?"text-[#1a6b8a]":"text-slate-700"}`}>
              {gh} - {ga}
            </p>
          ) : (
            <p className="text-sm text-slate-400 font-semibold">{p.time || "- - -"}</p>
          )}
        </div>
        {/* Equip visitant */}
        <div className="flex-1">
          <p className="font-bold text-sm text-slate-800 leading-tight">{p.away_team}</p>
          <p className="text-xs text-slate-400">Fora</p>
        </div>
      </div>
      {/* Info extra */}
      {(p.date || p.referee) && (
        <p className="text-xs text-slate-400 text-center mt-1.5">
          {p.date}{p.referee ? ` · ${p.referee}` : ""}
        </p>
      )}
      {!jugat && p.venue && (
        <p className="text-xs text-slate-400 text-center mt-1">📍 {p.venue.substring(0,35)}</p>
      )}
    </button>
  );
}

// ─── Component principal ──────────────────────────────────────────────────────
export default function PartitsPage() {
  const { categoria, grup } = useGrup();
  const [jornades,      setJornades]      = useState([]);
  const [jornadaActual, setJornadaActual] = useState(null);
  const [jornadaSel,    setJornadaSel]    = useState(null);
  const [partits,       setPartits]       = useState([]);
  const [seleccionat,   setSeleccionat]   = useState(null);
  const [detail,        setDetail]        = useState(null);  // acta o prèvia
  const [loadingJorn,   setLoadingJorn]   = useState(true);
  const [loadingPartits,setLoadingPartits]= useState(false);
  const [loadingDetail, setLoadingDetail] = useState(false);

  // Carregar jornades
  useEffect(() => {
    setLoadingJorn(true);
    setSeleccionat(null); setDetail(null); setPartits([]);
    fetch(`${API}/partits/${categoria}/${grup}/jornades`)
      .then(r => r.json())
      .then(d => {
        setJornades(d.jornades || []);
        setJornadaActual(d.jornada_actual);
        setJornadaSel(d.jornada_actual);
        setLoadingJorn(false);
      })
      .catch(() => setLoadingJorn(false));
  }, [categoria, grup]);

  // Carregar partits quan canvia jornada
  useEffect(() => {
    if (!jornadaSel) return;
    setLoadingPartits(true);
    setSeleccionat(null); setDetail(null);
    fetch(`${API}/partits/${categoria}/${grup}/jornada/${jornadaSel}`)
      .then(r => r.json())
      .then(d => { setPartits(d.partits || []); setLoadingPartits(false); })
      .catch(() => setLoadingPartits(false));
  }, [jornadaSel, categoria, grup]);

  // Obrir acta o prèvia
  const obrirPartit = useCallback((p) => {
    setSeleccionat(p);
    setDetail(null);
    setLoadingDetail(true);
    const endpoint = p.jugat
      ? `${API}/partits/${categoria}/${grup}/acta?home=${encodeURIComponent(p.local_team)}&away=${encodeURIComponent(p.away_team)}&jornada=${jornadaSel}`
      : `${API}/partits/${categoria}/${grup}/previa?home=${encodeURIComponent(p.local_team)}&away=${encodeURIComponent(p.away_team)}`;
    fetch(endpoint)
      .then(r => r.json())
      .then(d => { setDetail({ type: p.jugat ? "acta" : "previa", data: d }); setLoadingDetail(false); })
      .catch(() => setLoadingDetail(false));
  }, [categoria, grup, jornadaSel]);

  return (
    <div>
      <h1 className="text-2xl font-bold text-slate-900 mb-5">📅 Partits</h1>

      {loadingJorn && <div className="flex items-center justify-center py-20 text-slate-400"><div className="animate-spin text-2xl mr-3">⟳</div></div>}

      {!loadingJorn && (
        <div className="flex gap-5">
          {/* Columna esquerra: selector jornada + cards */}
          <div className="w-72 shrink-0">
            {/* Selector de jornada */}
            <div className="bg-white rounded-2xl border border-slate-200 p-3 mb-3">
              <p className="text-xs font-semibold text-slate-500 uppercase tracking-wide mb-2">Jornada</p>
              <div className="flex items-center gap-2">
                <button onClick={() => setJornadaSel(j => Math.max((jornades[0]||1), j-1))}
                  className="w-8 h-8 rounded-lg bg-slate-100 hover:bg-slate-200 text-slate-600 font-bold transition-colors">‹</button>
                <select value={jornadaSel||""} onChange={e=>setJornadaSel(Number(e.target.value))}
                  className="flex-1 text-center font-bold text-slate-800 border border-slate-200 rounded-lg py-1.5 bg-white focus:outline-none focus:border-green-400">
                  {jornades.map(j=><option key={j} value={j}>Jornada {j}{j===jornadaActual?" ★":""}</option>)}
                </select>
                <button onClick={() => setJornadaSel(j => Math.min((jornades[jornades.length-1]||j), j+1))}
                  className="w-8 h-8 rounded-lg bg-slate-100 hover:bg-slate-200 text-slate-600 font-bold transition-colors">›</button>
              </div>
            </div>

            {/* Cards dels partits */}
            {loadingPartits
              ? <div className="flex items-center justify-center py-10 text-slate-400"><div className="animate-spin text-xl mr-2">⟳</div></div>
              : partits.map((p) => (
                  <PartitCard key={`${p.local_team}-${p.away_team}`} p={p} seleccionat={seleccionat} onClick={() => obrirPartit(p)}/>
                ))
            }
          </div>

          {/* Columna dreta: acta o prèvia */}
          <div className="flex-1 min-w-0">
            {!seleccionat && !loadingDetail && (
              <div className="flex items-center justify-center h-64 text-slate-400 bg-white rounded-2xl border border-dashed border-slate-200">
                <div className="text-center"><div className="text-4xl mb-2">⚽</div><p className="text-sm">Clica un partit per veure l'acta o la prèvia</p></div>
              </div>
            )}

            {loadingDetail && (
              <div className="flex items-center justify-center py-20 text-slate-400">
                <div className="animate-spin text-2xl mr-3">⟳</div>Carregant...
              </div>
            )}

            {detail && !loadingDetail && (
              <div>
                {/* Títol */}
                <div className="flex items-center gap-2 mb-4">
                  <span className="text-lg">{detail.type==="acta"?"📋":"⚡"}</span>
                  <h2 className="font-bold text-slate-800">
                    {detail.type==="acta" ? "Acta" : "Prèvia"} · J{jornadaSel} · {detail.data.local_team} vs {detail.data.away_team}
                  </h2>
                </div>
                {detail.type==="acta"
                  ? <ActaVisual data={detail.data}/>
                  : <PreviaVisual data={detail.data}/>
                }
              </div>
            )}
          </div>
        </div>
      )}
    </div>
  );
}