"use client";
import { useEffect, useState, useCallback } from "react";
import { useGrup } from "../components/GrupContext";

const API = process.env.NEXT_PUBLIC_API_URL || "http://localhost:8000";

// ── Radar SVG hexagonal ──────────────────────────────────────────────────────
function RadarEquip({ radar, color = "#16a34a" }) {
  const labels = ["Atac","Defensa","Casa","Fora","Fair Play","1a Part","2a Part"];
  const keys   = ["atac","defensa","casa","fora","fairplay","primera_part","segona_part"];
  const n = labels.length;
  const cx = 160, cy = 155, r = 110;
  const rd = radar || {};  // protecció: mai null/undefined
  const angle = (i) => -Math.PI / 2 + i * (2 * Math.PI) / n;
  const pt = (i, pct) => ({ x: cx + r*(pct/100)*Math.cos(angle(i)), y: cy + r*(pct/100)*Math.sin(angle(i)) });
  const gridPath = (pct) => keys.map((_,i)=>`${i===0?"M":"L"} ${pt(i,pct).x} ${pt(i,pct).y}`).join(" ")+" Z";
  const dataPath = keys.map((k,i)=>`${i===0?"M":"L"} ${pt(i,rd[k]||50).x} ${pt(i,rd[k]||50).y}`).join(" ")+" Z";
  return (
    <svg viewBox="0 0 320 310" className="w-full max-w-xs mx-auto">
      {[20,40,60,80,100].map(p=><path key={p} d={gridPath(p)} fill="none" stroke="#e2e8f0" strokeWidth={1}/>)}
      {keys.map((_,i)=>{const e=pt(i,100);return<line key={i} x1={cx} y1={cy} x2={e.x} y2={e.y} stroke="#e2e8f0" strokeWidth={1}/>})}
      <path d={dataPath} fill={color} fillOpacity={0.15} stroke={color} strokeWidth={2}/>
      {keys.map((k,i)=>{const p=pt(i,rd[k]||50);return<circle key={k} cx={p.x} cy={p.y} r={4} fill={color}/>})}
      {labels.map((l,i)=>{const p=pt(i,122);return<text key={l} x={p.x} y={p.y} textAnchor="middle" dominantBaseline="middle" fontSize={10} fill="#475569" fontWeight="500">{l}</text>})}
      {keys.map((k,i)=>{const p=pt(i,rd[k]||50);return<text key={`v-${k}`} x={p.x} y={p.y-9} textAnchor="middle" fontSize={9} fill={color} fontWeight="700">{rd[k]||50}</text>})}
    </svg>
  );
}

// ── Barres verticals ─────────────────────────────────────────────────────────
function BarresV({ dades, keys, colors, labels, height=180 }) {
  if (!dades?.length) return null;
  const W=540, padL=40, padR=90, padT=10, padB=30;
  const gH=height-padT-padB, gW=W-padL-padR;
  const bpg=keys.length, grpW=gW/dades.length, bW=Math.min(22,(grpW-8)/bpg);
  const maxV=Math.max(...dades.flatMap(d=>keys.map(k=>d[k]||0)),0.1);
  const yS=(v)=>padT+gH-((v/maxV)*gH);
  return (
    <svg viewBox={`0 0 ${W} ${height}`} className="w-full">
      {[0,maxV/2,maxV].map(v=>(
        <g key={v}>
          <line x1={padL} x2={W-padR} y1={yS(v)} y2={yS(v)} stroke="#f1f5f9" strokeWidth={1}/>
          <text x={padL-4} y={yS(v)+4} textAnchor="end" fontSize={9} fill="#94a3b8">{Math.round(v*10)/10}</text>
        </g>
      ))}
      {dades.map((d,gi)=>{
        const gx=padL+gi*grpW+(grpW-bpg*bW-(bpg-1)*3)/2;
        return (
          <g key={gi}>
            {keys.map((k,ki)=>{
              const v=d[k]||0, bx=gx+ki*(bW+3), h=gH-(yS(v)-padT);
              return h>0 ? <rect key={k} x={bx} y={yS(v)} width={bW} height={h} fill={colors[ki]} rx={2}/> : null;
            })}
            <text x={padL+gi*grpW+grpW/2} y={height-5} textAnchor="middle" fontSize={9} fill="#64748b">
              {(d.label||d.bloc||d.nivell||"").substring(0,12)}
            </text>
          </g>
        );
      })}
      {labels.map((l,i)=>(
        <g key={l}>
          <rect x={W-padR+5} y={padT+i*16} width={10} height={10} fill={colors[i]} rx={2}/>
          <text x={W-padR+18} y={padT+i*16+9} fontSize={9} fill="#64748b">{l}</text>
        </g>
      ))}
    </svg>
  );
}

// ── Gràfic de línies ─────────────────────────────────────────────────────────
function GraficLinies({ dades, series, height=180 }) {
  if (!dades?.length || dades.length<2) return null;
  const W=540, padL=35, padR=90, padT=15, padB=25, gW=W-padL-padR, gH=height-padT-padB;
  const allVals=dades.flatMap(d=>series.map(s=>d[s.key])).filter(v=>v!=null);
  const maxV=Math.max(...allVals,1);
  const xS=(i)=>padL+(i/(dades.length-1||1))*gW;
  const yS=(v)=>padT+gH-((v/maxV)*gH);
  return (
    <svg viewBox={`0 0 ${W} ${height}`} className="w-full">
      {[0,Math.round(maxV/2),maxV].map((v,gi)=>(
        <g key={gi}>
          <line x1={padL} x2={W-padR} y1={yS(v)} y2={yS(v)} stroke="#f1f5f9" strokeWidth={1}/>
          <text x={padL-4} y={yS(v)+4} textAnchor="end" fontSize={9} fill="#94a3b8">{v}</text>
        </g>
      ))}
      {dades.filter((_,i)=>i===0||i===dades.length-1||i%Math.ceil(dades.length/7)===0).map((d,_,arr)=>{
        const i=dades.indexOf(d);
        return <text key={i} x={xS(i)} y={height-5} textAnchor="middle" fontSize={9} fill="#94a3b8">J{d.jornada}</text>;
      })}
      {series.map(s=>{
        const pts=dades.map((d,i)=>({i,v:d[s.key]})).filter(p=>p.v!=null);
        if(pts.length<2) return null;
        const path=pts.map((p,j)=>`${j===0?"M":"L"} ${xS(p.i)} ${yS(p.v)}`).join(" ");
        return (
          <g key={s.key}>
            <path d={path} fill="none" stroke={s.color} strokeWidth={2.5} strokeLinejoin="round"/>
            {pts.map(p=><circle key={p.i} cx={xS(p.i)} cy={yS(p.v)} r={3} fill={s.color}/>)}
          </g>
        );
      })}
      {series.map((s,i)=>(
        <g key={s.label}>
          <rect x={W-padR+5} y={padT+i*16} width={10} height={10} fill={s.color} rx={2}/>
          <text x={W-padR+18} y={padT+i*16+9} fontSize={9} fill="#64748b">{s.label.substring(0,14)}</text>
        </g>
      ))}
    </svg>
  );
}

// ── Scatterplot interactiu ────────────────────────────────────────────────────
function Scatterplot({ dades }) {
  const [hover,setHover]=useState(null);
  if (!dades?.length) return null;
  const W=540,H=240,padL=40,padR=15,padT=15,padB=35,gW=W-padL-padR,gH=H-padT-padB;
  const maxX=Math.max(...dades.map(d=>d.total_minutes),1);
  const maxY=Math.max(...dades.map(d=>d.partits),1);  // Eix Y = partits jugats
  const xS=(v)=>padL+(v/maxX)*gW, yS=(v)=>padT+gH-((v/maxY)*gH);
  return (
    <svg viewBox={`0 0 ${W} ${H}`} className="w-full" style={{overflow:"visible"}}>
      {[0,Math.round(maxY/2),maxY].map(v=>(
        <g key={v}>
          <line x1={padL} x2={W-padR} y1={yS(v)} y2={yS(v)} stroke="#f1f5f9" strokeWidth={1}/>
          <text x={padL-4} y={yS(v)+4} textAnchor="end" fontSize={9} fill="#94a3b8">{v}</text>
        </g>
      ))}
      {[0,Math.round(maxX/3),Math.round(2*maxX/3),maxX].map(v=>(
        <g key={v}>
          <text x={xS(v)} y={H-padB+14} textAnchor="middle" fontSize={9} fill="#94a3b8">{v}'</text>
        </g>
      ))}
      <text x={padL-28} y={H/2} textAnchor="middle" fontSize={9} fill="#94a3b8" transform={`rotate(-90,${padL-28},${H/2})`}>Partits</text>
      <text x={W/2} y={H-2} textAnchor="middle" fontSize={9} fill="#94a3b8">Minuts totals</text>
      {dades.map((d,i)=>(
        <circle key={i} cx={xS(d.total_minutes)} cy={yS(d.partits)} r={hover===i?7:5}
          fill={d.gols>0?"#16a34a":"#64748b"} fillOpacity={0.75}  
          stroke={hover===i?"#0f172a":"transparent"} strokeWidth={1.5} style={{cursor:"pointer"}}
          onMouseEnter={()=>setHover(i)} onMouseLeave={()=>setHover(null)}/>
      ))}
      {hover!=null&&(()=>{
        const d=dades[hover]; const bx=Math.min(xS(d.total_minutes),W-180), by=Math.max(yS(d.partits)-60,5);
        return (
          <g>
            <rect x={bx} y={by} width={175} height={52} rx={6} fill="white" stroke="#e2e8f0" strokeWidth={1}/>
            <text x={bx+8} y={by+16} fontSize={10} fontWeight="600" fill="#0f172a">{d.player.substring(0,25)}</text>
            <text x={bx+8} y={by+30} fontSize={9} fill="#64748b">{d.total_minutes}' jugats · {d.gols} gols</text>
            <text x={bx+8} y={by+44} fontSize={9} fill="#64748b">{d.partits} partits jugats · {d.avg_minutes}' avg/p</text>
          </g>
        );
      })()}
    </svg>
  );
}

// ── Tilt Box ──────────────────────────────────────────────────────────────────
function TiltBox({ tilt }) {
  if (!tilt) return null;
  const v=tilt.tilt;
  const cls=v>0.3?"text-green-700 bg-green-50 border-green-200":v<-0.3?"text-red-600 bg-red-50 border-red-200":"text-yellow-700 bg-yellow-50 border-yellow-200";
  return (
    <div className={`rounded-xl border p-4 ${cls}`}>
      <div className="flex items-center gap-3">
        <span className="text-2xl">{v>0.3?"🚀":v<-0.3?"📉":"➡️"}</span>
        <div>
          <p className="font-bold">Tilt: {v>0?"+":""}{v} <span className="text-xs font-normal opacity-70">(últims {tilt.n_recents} partits)</span></p>
          <p className="text-xs opacity-75">{tilt.pts_real_avg} pts reals vs {tilt.pts_expected_avg} esperats.{v>0.5?" Supera les expectatives.":v<-0.5?" Per sota de les expectatives.":" Rendiment normal."}</p>
        </div>
      </div>
    </div>
  );
}

const StatBox=({label,value,color="text-slate-800"})=>(
  <div className="bg-slate-50 rounded-xl p-3 text-center">
    <p className={`text-xl font-bold ${color}`}>{value}</p>
    <p className="text-xs text-slate-500 mt-0.5">{label}</p>
  </div>
);

const r2=(v)=>Math.round(v*100)/100;


// ── Radar dual superposat ─────────────────────────────────────────────────────
function RadarDual({ radar1, radar2 }) {
  const labels = ["Atac","Defensa","Casa","Fora","Fair Play","1a Part","2a Part"];
  const keys   = ["atac","defensa","casa","fora","fairplay","primera_part","segona_part"];
  const n = labels.length;
  const cx = 200, cy = 190, r = 140;
  const angle = (i) => -Math.PI / 2 + i * (2 * Math.PI) / n;
  const pt = (i, pct) => ({ x: cx + r*(pct/100)*Math.cos(angle(i)), y: cy + r*(pct/100)*Math.sin(angle(i)) });
  const gridPath = (pct) => keys.map((_,i)=>`${i===0?"M":"L"} ${pt(i,pct).x} ${pt(i,pct).y}`).join(" ")+" Z";
  const dataPath = (radar, keys_list) => keys_list.map((k,i)=>`${i===0?"M":"L"} ${pt(i,radar[k]||50).x} ${pt(i,radar[k]||50).y}`).join(" ")+" Z";
  return (
    <svg viewBox="0 0 400 380" className="w-full max-w-sm mx-auto">
      {[20,40,60,80,100].map(p=><path key={p} d={gridPath(p)} fill="none" stroke="#e2e8f0" strokeWidth={1}/>)}
      {keys.map((_,i)=>{const e=pt(i,100);return<line key={i} x1={cx} y1={cy} x2={e.x} y2={e.y} stroke="#e2e8f0" strokeWidth={1}/>})}
      <path d={dataPath(radar1,keys)} fill="#16a34a" fillOpacity={0.15} stroke="#16a34a" strokeWidth={2}/>
      <path d={dataPath(radar2,keys)} fill="#2563eb" fillOpacity={0.15} stroke="#2563eb" strokeWidth={2} strokeDasharray="5,3"/>
      {keys.map((k,i)=>{const p=pt(i,radar1[k]||50);return<circle key={`r1-${k}`} cx={p.x} cy={p.y} r={4} fill="#16a34a"/>})}
      {keys.map((k,i)=>{const p=pt(i,radar2[k]||50);return<circle key={`r2-${k}`} cx={p.x} cy={p.y} r={4} fill="#2563eb"/>})}
      {labels.map((l,i)=>{const p=pt(i,120);return<text key={l} x={p.x} y={p.y} textAnchor="middle" dominantBaseline="middle" fontSize={11} fill="#475569" fontWeight="500">{l}</text>})}
    </svg>
  );
}

// ── Barres bilaterals horitzontals ────────────────────────────────────────────
function BarresBilaterals({ dades }) {
  if (!dades?.length) return null;
  const maxV = Math.max(...dades.map(d => Math.max(d.val1, d.val2)), 1);
  const H = dades.length * 36 + 40;
  const W = 500, cx = W / 2, padT = 20, rowH = 36, bH = 18;
  const barW = (v) => (v / maxV) * (cx - 80);

  return (
    <svg viewBox={`0 0 ${W} ${H}`} className="w-full">
      {/* Eix central */}
      <line x1={cx} x2={cx} y1={padT-5} y2={H-15} stroke="#e2e8f0" strokeWidth={1}/>
      {/* Etiquetes columnes */}
      <text x={cx-10} y={padT-8} textAnchor="end" fontSize={9} fill="#94a3b8">← Equip 1</text>
      <text x={cx+10} y={padT-8} textAnchor="start" fontSize={9} fill="#94a3b8">Equip 2 →</text>
      {dades.map((d, i) => {
        const y = padT + i * rowH;
        const w1 = barW(d.val1), w2 = barW(d.val2);
        return (
          <g key={i}>
            {/* Etiqueta central */}
            <text x={cx} y={y + rowH/2 + 4} textAnchor="middle" fontSize={10} fill="#475569" fontWeight="500">{d.label}</text>
            {/* Barra esquerra (val1) */}
            <rect x={cx - w1 - 40} y={y + 2} width={w1} height={bH} fill="#16a34a" rx={3} opacity={0.8}/>
            <text x={cx - w1 - 44} y={y + bH/2 + 5} textAnchor="end" fontSize={11} fontWeight="700" fill="#16a34a">{d.val1}</text>
            {/* Barra dreta (val2) */}
            <rect x={cx + 40} y={y + 2} width={w2} height={bH} fill="#2563eb" rx={3} opacity={0.8}/>
            <text x={cx + w2 + 44} y={y + bH/2 + 5} textAnchor="start" fontSize={11} fontWeight="700" fill="#2563eb">{d.val2}</text>
          </g>
        );
      })}
    </svg>
  );
}

// ── Comparador ────────────────────────────────────────────────────────────────
function ComparadorEquips({ equips, categoria, grup }) {
  const [e1,setE1]=useState(""); const [e2,setE2]=useState("");
  const [dades,setDades]=useState(null); const [carregant,setCarregant]=useState(false);

  const comparar=()=>{
    if(!e1||!e2||e1===e2) return;
    setCarregant(true); setDades(null);
    Promise.all([
      fetch(`${API}/equip/${categoria}/${grup}/${encodeURIComponent(e1)}/complet`).then(r=>r.json()),
      fetch(`${API}/equip/${categoria}/${grup}/${encodeURIComponent(e2)}/complet`).then(r=>r.json()),
      fetch(`${API}/equips/${categoria}/${grup}/comparador?equip1=${encodeURIComponent(e1)}&equip2=${encodeURIComponent(e2)}`).then(r=>r.json()),
    ]).then(([d1,d2,h2h])=>{setDades({d1,d2,h2h});setCarregant(false);})
      .catch(()=>setCarregant(false));
  };

  const CompRow=({label,v1,v2,invert=false,fmt=v=>v})=>{
    const n1=parseFloat(v1), n2=parseFloat(v2);
    const c1=isNaN(n1)||isNaN(n2)?"text-slate-700":invert?(n1<n2?"text-green-600":n1>n2?"text-red-500":"text-slate-700"):(n1>n2?"text-green-600":n1<n2?"text-red-500":"text-slate-700");
    const c2=isNaN(n1)||isNaN(n2)?"text-slate-700":invert?(n2<n1?"text-green-600":n2>n1?"text-red-500":"text-slate-700"):(n2>n1?"text-green-600":n2<n1?"text-red-500":"text-slate-700");
    return (
      <div className="flex items-center py-2 border-b border-slate-50 text-sm">
        <span className={`w-16 text-right font-bold ${c1}`}>{fmt(v1)}</span>
        <span className="flex-1 text-center text-xs text-slate-400 px-2">{label}</span>
        <span className={`w-16 font-bold ${c2}`}>{fmt(v2)}</span>
      </div>
    );
  };

  return (
    <div className="space-y-4">
      <div className="bg-white rounded-2xl border border-slate-200 p-4">
        <p className="text-sm font-semibold text-slate-700 mb-3">Selecciona dos equips per comparar</p>
        <div className="flex gap-3 flex-wrap">
          <select value={e1} onChange={ev=>setE1(ev.target.value)} className="flex-1 min-w-[180px] px-3 py-2 text-sm border border-slate-200 rounded-lg focus:outline-none focus:border-green-400 bg-white">
            <option value="">Equip 1...</option>
            {equips.map(eq=><option key={eq} value={eq}>{eq}</option>)}
          </select>
          <select value={e2} onChange={ev=>setE2(ev.target.value)} className="flex-1 min-w-[180px] px-3 py-2 text-sm border border-slate-200 rounded-lg focus:outline-none focus:border-green-400 bg-white">
            <option value="">Equip 2...</option>
            {equips.filter(eq=>eq!==e1).map(eq=><option key={eq} value={eq}>{eq}</option>)}
          </select>
          <button onClick={comparar} disabled={!e1||!e2||e1===e2||carregant}
            className="px-5 py-2 bg-green-600 text-white text-sm font-semibold rounded-lg hover:bg-green-700 disabled:opacity-40 transition-colors">
            {carregant?"Carregant...":"Comparar"}
          </button>
        </div>
      </div>

      {dades&&(()=>{
        const {d1,d2,h2h}=dades; const r1=d1.resum, r2v=d2.resum;
        return (
          <div className="space-y-4">
            {/* Capçalera */}
            <div className="bg-white rounded-2xl border border-slate-200 p-5">
              <div className="grid grid-cols-3 text-center gap-4">
                <div><p className="font-bold text-slate-800 text-sm truncate mb-1">{e1}</p><p className="text-5xl font-black text-green-600">{d1.rating}</p><p className="text-xs text-slate-400">Rating · {d1.posicio?`${d1.posicio}ª pos.`:"—"}</p></div>
                <div className="flex items-center justify-center text-slate-300 text-3xl font-bold">vs</div>
                <div><p className="font-bold text-slate-800 text-sm truncate mb-1">{e2}</p><p className="text-5xl font-black text-blue-600">{d2.rating}</p><p className="text-xs text-slate-400">Rating · {d2.posicio?`${d2.posicio}ª pos.`:"—"}</p></div>
              </div>
            </div>

            {/* Caixes comparatives */}
            <div className="grid grid-cols-1 md:grid-cols-3 gap-4">
              <div className="bg-white rounded-2xl border border-slate-200 p-4">
                <p className="text-xs font-semibold text-slate-500 uppercase tracking-wide mb-3">📊 Classificació</p>
                <div className="flex text-xs text-slate-400 justify-between mb-1"><span className="truncate max-w-[80px]">{e1.split(",")[0].substring(0,12)}</span><span className="truncate max-w-[80px] text-right">{e2.split(",")[0].substring(0,12)}</span></div>
                <CompRow label="Posició"   v1={d1.posicio!=null?d1.posicio:"—"} v2={d2.posicio!=null?d2.posicio:"—"} invert/>
                <CompRow label="Punts"     v1={r1.punts}        v2={r2v.punts}/>
                <CompRow label="Victòries" v1={r1.guanyats}     v2={r2v.guanyats}/>
                <CompRow label="PJ"        v1={r1.jugats}       v2={r2v.jugats}/>
                <CompRow label="Empats"    v1={r1.empatats}     v2={r2v.empatats}/>
                <CompRow label="Derrotes"  v1={r1.perduts}      v2={r2v.perduts} invert/>
              </div>
              <div className="bg-white rounded-2xl border border-slate-200 p-4">
                <p className="text-xs font-semibold text-slate-500 uppercase tracking-wide mb-3">🥅 Gols</p>
                <div className="flex text-xs text-slate-400 justify-between mb-1"><span></span><span></span></div>
                <CompRow label="A favor"    v1={r1.gols_a_favor}    v2={r2v.gols_a_favor}/>
                <CompRow label="En contra"  v1={r1.gols_en_contra}  v2={r2v.gols_en_contra} invert/>
                <CompRow label="Avg/partit" v1={r2(r1.gols_a_favor/Math.max(r1.jugats,1))} v2={r2(r2v.gols_a_favor/Math.max(r2v.jugats,1))} fmt={v=>parseFloat(v).toFixed(2)}/>
                <CompRow label="Diferència" v1={r1.diferencia_gols} v2={r2v.diferencia_gols}/>
              </div>
              <div className="bg-white rounded-2xl border border-slate-200 p-4">
                <p className="text-xs font-semibold text-slate-500 uppercase tracking-wide mb-3">🟨 Targetes</p>
                <CompRow label="Grogues"      v1={r1.targetes_grogues}   v2={r2v.targetes_grogues}   invert/>
                <CompRow label="Grogues/PJ"   v1={r2(r1.targetes_grogues/Math.max(r1.jugats,1))}  v2={r2(r2v.targetes_grogues/Math.max(r2v.jugats,1))} invert fmt={v=>parseFloat(v).toFixed(2)}/>
                <CompRow label="Vermelles"    v1={r1.targetes_vermelles} v2={r2v.targetes_vermelles} invert/>
              </div>
            </div>

            {/* Tilt */}
            <div className="grid grid-cols-2 gap-4">
              <div><p className="text-xs font-semibold text-slate-500 mb-1">⚡ Tilt — {e1.split(",")[0].substring(0,18)}</p><TiltBox tilt={d1.tilt}/></div>
              <div><p className="text-xs font-semibold text-slate-500 mb-1">⚡ Tilt — {e2.split(",")[0].substring(0,18)}</p><TiltBox tilt={d2.tilt}/></div>
            </div>

            {/* Radars superposats */}
            <div className="bg-white rounded-2xl border border-slate-200 p-4">
              <p className="text-sm font-semibold text-slate-700 mb-2">⬡ Radars superposats</p>
              <div className="flex gap-4 text-xs justify-center mb-2">
                <span className="flex items-center gap-1.5"><span className="inline-block w-3 h-3 rounded-full bg-green-500"></span>{e1.split(",")[0].substring(0,20)}</span>
                <span className="flex items-center gap-1.5"><span className="inline-block w-3 h-3 rounded-full bg-blue-500 border border-dashed border-blue-300"></span>{e2.split(",")[0].substring(0,20)}</span>
              </div>
              <RadarDual radar1={d1.radar} radar2={d2.radar}/>
            </div>

            {/* Evolució punts */}
            <div className="bg-white rounded-2xl border border-slate-200 p-4">
              <p className="text-sm font-semibold text-slate-700 mb-3">📈 Evolució de Punts</p>
              {(()=>{
                const n=Math.min(d1.punts_acumulats.length,d2.punts_acumulats.length);
                const merged=d1.punts_acumulats.slice(0,n).map((p,i)=>({
                  jornada:p.jornada,
                  eq1:p.punts,
                  eq2:d2.punts_acumulats[i]?.punts??null,
                }));
                return <GraficLinies dades={merged} series={[{key:"eq1",color:"#16a34a",label:e1.substring(0,15)},{key:"eq2",color:"#2563eb",label:e2.substring(0,15)}]}/>;
              })()}
            </div>

            {/* Punts casa vs fora */}
            <div className="bg-white rounded-2xl border border-slate-200 p-4">
              <p className="text-sm font-semibold text-slate-700 mb-3">🏠 Pts/partit Casa vs Fora</p>
              <BarresV dades={[
                {label:"Casa",eq1:d1.casa.jugats>0?r2(d1.casa.punts/d1.casa.jugats):0, eq2:d2.casa.jugats>0?r2(d2.casa.punts/d2.casa.jugats):0},
                {label:"Fora",eq1:d1.fora.jugats>0?r2(d1.fora.punts/d1.fora.jugats):0, eq2:d2.fora.jugats>0?r2(d2.fora.punts/d2.fora.jugats):0},
              ]} keys={["eq1","eq2"]} colors={["#16a34a","#2563eb"]} labels={[e1.substring(0,15),e2.substring(0,15)]} height={150}/>
            </div>

            {/* Gols per franja — barres horitzontals bilaterals */}
            <div className="bg-white rounded-2xl border border-slate-200 p-4">
              <p className="text-sm font-semibold text-slate-700 mb-1">⏱ Gols Marcats per Franja</p>
              <div className="flex gap-4 text-xs justify-center mb-3">
                <span className="flex items-center gap-1.5"><span className="inline-block w-3 h-3 rounded bg-green-500"></span>{e1.split(",")[0].substring(0,18)}</span>
                <span className="flex items-center gap-1.5"><span className="inline-block w-3 h-3 rounded bg-blue-500"></span>{e2.split(",")[0].substring(0,18)}</span>
              </div>
              <BarresBilaterals dades={d1.gols_per_minut.map((g,i)=>({
                label:g.bloc, val1:g.marcats, val2:d2.gols_per_minut[i]?.marcats||0,
              }))}/>
            </div>

            {/* Top golejadors */}
            <div className="grid grid-cols-2 gap-4">
              {[[e1,d1,"#16a34a"],[e2,d2,"#2563eb"]].map(([nom,d,col])=>(
                <div key={nom} className="bg-white rounded-2xl border border-slate-200 p-4">
                  <p className="text-xs font-semibold text-slate-500 uppercase tracking-wide mb-3">⭐ Top Golejadors</p>
                  <p className="text-xs font-medium mb-2 truncate" style={{color:col}}>{nom.split(",")[0].substring(0,22)}</p>
                  {d.dependencia_golejador.length===0&&<p className="text-xs text-slate-400">Sense gols registrats</p>}
                  {d.dependencia_golejador.map((j,i)=>(
                    <div key={j.player} className="flex items-center gap-2 py-1.5 border-b border-slate-50">
                      <span className="text-xs text-slate-400 w-4">{i+1}</span>
                      <span className="flex-1 text-xs text-slate-700 truncate">{j.player}</span>
                      <span className="text-xs font-bold" style={{color:col}}>{j.goals} ⚽</span>
                      <span className="text-xs text-slate-400 w-14 text-right">{j.pct}% equip</span>
                    </div>
                  ))}
                </div>
              ))}
            </div>

            {/* H2H */}
            {h2h.h2h?.length>0&&(()=>{
              const vic1=h2h.h2h.filter(p=>(p.local_team===e1&&p.goals_home>p.goals_away)||(p.away_team===e1&&p.goals_away>p.goals_home)).length;
              const vic2=h2h.h2h.filter(p=>(p.local_team===e2&&p.goals_home>p.goals_away)||(p.away_team===e2&&p.goals_away>p.goals_home)).length;
              const emp=h2h.h2h.length-vic1-vic2;
              return (
                <div className="bg-white rounded-2xl border border-slate-200 p-4">
                  <p className="text-sm font-semibold text-slate-700 mb-3">⚔️ Confrontació Directa</p>
                  <div className="flex items-center justify-center gap-8 py-4 mb-4 bg-slate-50 rounded-xl">
                    <div className="text-center">
                      <p className="text-xs text-slate-500 max-w-[100px] truncate mb-1">{e1.split(",")[0].substring(0,16)}</p>
                      <p className="text-5xl font-black text-green-600">{vic1}</p>
                      <p className="text-xs text-slate-400">victòries</p>
                    </div>
                    <div className="text-center"><p className="text-3xl font-bold text-slate-300">{emp}</p><p className="text-xs text-slate-400">empats</p></div>
                    <div className="text-center">
                      <p className="text-xs text-slate-500 max-w-[100px] truncate mb-1">{e2.split(",")[0].substring(0,16)}</p>
                      <p className="text-5xl font-black text-blue-600">{vic2}</p>
                      <p className="text-xs text-slate-400">victòries</p>
                    </div>
                  </div>
                  {h2h.h2h.map((p,i)=>(
                    <div key={i} className="flex items-center gap-3 text-sm py-2 border-b border-slate-50">
                      <span className="text-xs text-slate-400 w-8">J{p.jornada}</span>
                      <span className="flex-1 text-right font-medium text-slate-700 truncate">{p.local_team}</span>
                      <span className="font-bold text-slate-900 bg-slate-100 px-3 py-1 rounded shrink-0">{p.goals_home} - {p.goals_away}</span>
                      <span className="flex-1 font-medium text-slate-700 truncate">{p.away_team}</span>
                    </div>
                  ))}
                </div>
              );
            })()}
          </div>
        );
      })()}
    </div>
  );
}

// ── Component principal ───────────────────────────────────────────────────────
const TABS_EQUIP=[{id:"fitxa",label:"📋 Fitxa"},{id:"calendari",label:"📅 Calendari"},{id:"plantilla",label:"👥 Plantilla"},{id:"comparador",label:"⚖️ Comparador"}];

export default function EquipsPage() {
  const { categoria, grup } = useGrup();
  const [equips,setEquips]=useState([]);
  const [seleccionat,setSeleccionat]=useState(null);
  const [fitxa,setFitxa]=useState(null);
  const [tab,setTab]=useState("fitxa");
  const [sortKey,setSortKey]=useState("total_minutes");
  const [sortDir,setSortDir]=useState(-1);
  const [carregant,setCarregant]=useState(true);
  const [carregantFitxa,setCarregantFitxa]=useState(false);
  const [error,setError]=useState(null);

  useEffect(()=>{
    setCarregant(true);setError(null);setSeleccionat(null);setFitxa(null);
    fetch(`${API}/equips/${categoria}/${grup}`)
      .then(r=>r.json()).then(d=>{setEquips(d.equips||[]);setCarregant(false);})
      .catch(e=>{setError(e.message);setCarregant(false);});
  },[categoria,grup]);

  const obrirFitxa=useCallback((equip)=>{
    setSeleccionat(equip);setCarregantFitxa(true);setFitxa(null);setTab("fitxa");
    Promise.all([
      fetch(`${API}/equip/${categoria}/${grup}/${encodeURIComponent(equip)}/complet`)
        .then(r=>{ if(!r.ok) throw new Error(`HTTP ${r.status}`); return r.json(); }),
      fetch(`${API}/equip/${categoria}/${grup}/${encodeURIComponent(equip)}/plantilla`)
        .then(r=>{ if(!r.ok) return {jugadors:[]}; return r.json(); }),
    ]).then(([f,p])=>{
      // Garantir valors per defecte per evitar crashes al render
      const fitxaSegura = {
        rating: 50,
        radar: {},
        tilt: {tilt:0, pts_real_avg:0, pts_expected_avg:0, n_recents:0},
        resum: {jugats:0,guanyats:0,empatats:0,perduts:0,gols_a_favor:0,gols_en_contra:0,diferencia_gols:0,punts:0,targetes_grogues:0,targetes_vermelles:0},
        casa: {jugats:0,guanyats:0,empatats:0,perduts:0,gols_a:0,gols_c:0,punts:0},
        fora: {jugats:0,guanyats:0,empatats:0,perduts:0,gols_a:0,gols_c:0,punts:0},
        gols_per_part: {marcats_1a:0,marcats_2a:0,rebuts_1a:0,rebuts_2a:0},
        gols_per_minut: [],
        gols_per_jornada: [],
        punts_acumulats: [],
        rend_vs_nivell: [],
        carrega_jugadors: [],
        dependencia_golejador: [],
        partits_calendari: [],
        ultim_partit: null,
        proper_partit: null,
        posicio: null,
        ...f,
        plantilla: p.jugadors||[],
      };
      setFitxa(fitxaSegura);
      setCarregantFitxa(false);
    }).catch((err)=>{
      console.error("Error carregant fitxa equip:", err);
      setCarregantFitxa(false);
    });
  },[categoria,grup]);

  const toggleSort=(key)=>{ if(sortKey===key) setSortDir(d=>-d); else {setSortKey(key);setSortDir(-1);} };

  return (
    <div>
      <h1 className="text-2xl font-bold text-slate-900 mb-6">Equips</h1>
      {carregant&&<div className="flex items-center justify-center py-20 text-slate-400"><div className="animate-spin text-2xl mr-3">⟳</div>Carregant...</div>}
      {error&&<div className="bg-red-50 border border-red-200 rounded-xl p-4 text-red-600 text-sm">Error: {error}</div>}

      {!carregant&&!error&&(
        <div className="flex gap-5">
          {/* Llista */}
          <div className="w-52 shrink-0">
            <p className="text-xs font-semibold text-slate-500 uppercase tracking-wide mb-2">{equips.length} equips</p>
            <div className="space-y-1 max-h-[75vh] overflow-y-auto pr-1">
              {equips.map(eq=>(
                <button key={eq} onClick={()=>obrirFitxa(eq)}
                  className={`w-full text-left px-3 py-2 rounded-xl text-xs font-medium transition-all ${seleccionat===eq?"bg-green-600 text-white":"bg-white border border-slate-200 text-slate-700 hover:border-green-400"}`}>
                  {eq}
                </button>
              ))}
            </div>
          </div>

          {/* Contingut */}
          <div className="flex-1 min-w-0">
            {!seleccionat&&(
              <div className="flex items-center justify-center h-64 text-slate-400 bg-white rounded-2xl border border-dashed border-slate-200">
                <div className="text-center"><div className="text-4xl mb-2">🛡️</div><p className="text-sm">Selecciona un equip</p></div>
              </div>
            )}
            {carregantFitxa&&<div className="flex items-center justify-center py-20 text-slate-400"><div className="animate-spin text-2xl mr-3">⟳</div>Carregant...</div>}

            {fitxa&&!carregantFitxa&&(
              <div>
                <div className="flex gap-1 bg-slate-100 p-1 rounded-xl mb-4 w-fit">
                  {TABS_EQUIP.map(t=>(
                    <button key={t.id} onClick={()=>setTab(t.id)}
                      className={`px-3 py-1.5 rounded-lg text-xs font-medium transition-all ${tab===t.id?"bg-white text-slate-800 shadow-sm":"text-slate-500 hover:text-slate-700"}`}>
                      {t.label}
                    </button>
                  ))}
                </div>

                {/* ── FITXA ── */}
                {tab==="fitxa"&&(
                  <div className="space-y-4">
                    {/* Rating + últim/proper + radar */}
                    <div className="grid grid-cols-1 md:grid-cols-3 gap-4">
                      <div className="bg-gradient-to-br from-green-700 to-green-500 text-white rounded-2xl p-5 flex flex-col justify-center">
                        <div className="text-center">
                          <p className="text-7xl font-black leading-none">{fitxa.rating}</p>
                          <p className="text-xs tracking-widest opacity-80 mt-1 uppercase">Rating</p>
                          {fitxa.posicio&&<p className="text-sm mt-1 opacity-90">📍 {fitxa.posicio}ª posició</p>}
                        </div>
                        <div className="mt-3 grid grid-cols-3 gap-1 text-center text-xs">
                          <div><p className="font-bold text-lg">{fitxa.resum.guanyats}</p><p className="opacity-70">V</p></div>
                          <div><p className="font-bold text-lg">{fitxa.resum.empatats}</p><p className="opacity-70">E</p></div>
                          <div><p className="font-bold text-lg">{fitxa.resum.perduts}</p><p className="opacity-70">D</p></div>
                        </div>
                        <div className="mt-3 space-y-2">
                          {fitxa.ultim_partit&&(
                            <div className="bg-white/10 rounded-lg p-2 text-xs">
                              <p className="opacity-60 uppercase tracking-wide text-xs">Últim partit</p>
                              <p className="font-semibold">{fitxa.ultim_partit.home_away==="Home"?"🏠":"✈️"} vs {fitxa.ultim_partit.opponent.split(",")[0].substring(0,18)}</p>
                              <p className="font-bold text-base">{fitxa.ultim_partit.goals_for} - {fitxa.ultim_partit.goals_against}</p>
                            </div>
                          )}
                          {fitxa.proper_partit&&(
                            <div className="bg-white/10 rounded-lg p-2 text-xs">
                              <p className="opacity-60 uppercase tracking-wide text-xs">Proper partit (J{fitxa.proper_partit.jornada})</p>
                              <p className="font-semibold">{fitxa.proper_partit.home_away==="Home"?"🏠":"✈️"} vs {fitxa.proper_partit.opponent.split(",")[0].substring(0,18)}</p>
                            </div>
                          )}
                        </div>
                      </div>
                      <div className="md:col-span-2 bg-white rounded-2xl border border-slate-200 p-4">
                        <p className="text-xs font-semibold text-slate-500 uppercase tracking-wide mb-1">⬡ Radar de Rendiment</p>
                        <RadarEquip radar={fitxa.radar}/>
                      </div>
                    </div>

                    {/* KPIs */}
                    <div className="grid grid-cols-4 gap-3">
                      <StatBox label="Punts"          value={fitxa.resum.punts}                color="text-green-600"/>
                      <StatBox label="Gols a favor"   value={fitxa.resum.gols_a_favor}/>
                      <StatBox label="Gols en contra" value={fitxa.resum.gols_en_contra}/>
                      <StatBox label="Diferència"     value={fitxa.resum.diferencia_gols>0?`+${fitxa.resum.diferencia_gols}`:fitxa.resum.diferencia_gols}
                        color={fitxa.resum.diferencia_gols>0?"text-green-600":"text-red-500"}/>
                    </div>

                    {/* Targetes */}
                    <div className="grid grid-cols-2 gap-3">
                      <StatBox label="Targetes Grogues"   value={`🟨 ${fitxa.resum.targetes_grogues}`}/>
                      <StatBox label="Targetes Vermelles" value={`🟥 ${fitxa.resum.targetes_vermelles}`}/>
                    </div>

                    {/* Tilt */}
                    <TiltBox tilt={fitxa.tilt}/>

                    {/* Rendiment Casa vs Fora */}
                    <div className="bg-white rounded-2xl border border-slate-200 p-4">
                      <p className="text-sm font-semibold text-slate-700 mb-3">🏠 Rendiment Casa vs Fora (pts/partit)</p>
                      <BarresV dades={[
                        {label:"Casa",pts:fitxa.casa.jugats>0?r2(fitxa.casa.punts/fitxa.casa.jugats):0},
                        {label:"Fora",pts:fitxa.fora.jugats>0?r2(fitxa.fora.punts/fitxa.fora.jugats):0},
                      ]} keys={["pts"]} colors={["#16a34a"]} labels={["Pts/partit"]} height={130}/>
                      <div className="grid grid-cols-2 gap-3 mt-3">
                        {[["🏠 Casa",fitxa.casa],["✈️ Fora",fitxa.fora]].map(([t,b])=>(
                          <div key={t} className="text-center bg-slate-50 rounded-xl p-3 text-xs">
                            <p className="font-semibold text-slate-600 mb-1">{t}</p>
                            <div className="flex justify-center gap-3">
                              <span className="text-green-600 font-bold">{b.guanyats}V</span>
                              <span className="text-yellow-600 font-bold">{b.empatats}E</span>
                              <span className="text-red-500 font-bold">{b.perduts}D</span>
                            </div>
                            <p className="text-slate-400 mt-1">{b.gols_a} GF · {b.gols_c} GC · {b.punts} pts</p>
                          </div>
                        ))}
                      </div>
                    </div>

                    {/* Evolució punts */}
                    <div className="bg-white rounded-2xl border border-slate-200 p-4">
                      <p className="text-sm font-semibold text-slate-700 mb-3">📈 Evolució de Punts</p>
                      <GraficLinies dades={fitxa.punts_acumulats} series={[{key:"punts",color:"#16a34a",label:"Punts acumulats"}]}/>
                    </div>

                    {/* Gols marcats i rebuts per jornada */}
                    <div className="bg-white rounded-2xl border border-slate-200 p-4">
                      <p className="text-sm font-semibold text-slate-700 mb-3">⚽ Gols Marcats i Rebuts per Jornada</p>
                      <GraficLinies dades={fitxa.gols_per_jornada||[]} series={[
                        {key:"marcats",color:"#16a34a",label:"Marcats"},
                        {key:"rebuts", color:"#ef4444",label:"Rebuts"},
                      ]}/>
                    </div>

                    {/* Gols per franja */}
                    <div className="bg-white rounded-2xl border border-slate-200 p-4">
                      <p className="text-sm font-semibold text-slate-700 mb-3">⏱ Gols Marcats vs Rebuts per Franja</p>
                      <BarresV dades={fitxa.gols_per_minut} keys={["marcats","rebuts"]} colors={["#16a34a","#ef4444"]} labels={["Marcats","Rebuts"]}/>
                    </div>

                    {/* Efectivitat per parts */}
                    <div className="bg-white rounded-2xl border border-slate-200 p-4">
                      <p className="text-sm font-semibold text-slate-700 mb-3">⚡ Gols per Part (totals)</p>
                      {/* 2 grups (1a Part, 2a Part) x 2 barres (Marcats verd, Rebuts vermell) */}
                      <BarresV
                        dades={[
                          {label:"1a Part", marcats: fitxa.gols_per_part.marcats_1a, rebuts: fitxa.gols_per_part.rebuts_1a},
                          {label:"2a Part", marcats: fitxa.gols_per_part.marcats_2a, rebuts: fitxa.gols_per_part.rebuts_2a},
                        ]}
                        keys={["marcats","rebuts"]}
                        colors={["#16a34a","#ef4444"]}
                        labels={["Marcats","Rebuts"]}
                        height={160}
                      />
                      <div className="grid grid-cols-4 gap-2 mt-3">
                        {[
                          ["1a Part Marcats", fitxa.gols_per_part.marcats_1a, "text-green-600"],
                          ["1a Part Rebuts",  fitxa.gols_per_part.rebuts_1a,  "text-red-500"],
                          ["2a Part Marcats", fitxa.gols_per_part.marcats_2a, "text-green-600"],
                          ["2a Part Rebuts",  fitxa.gols_per_part.rebuts_2a,  "text-red-500"],
                        ].map(([l,v,c])=>(
                          <div key={l} className="bg-slate-50 rounded-xl p-2 text-center">
                            <p className={`text-xl font-bold ${c}`}>{v}</p>
                            <p className="text-xs text-slate-500">{l}</p>
                          </div>
                        ))}
                      </div>
                    </div>

                    {/* Càrrega treball */}
                    <div className="bg-white rounded-2xl border border-slate-200 p-4">
                      <p className="text-sm font-semibold text-slate-700 mb-1">🏃 Càrrega de Treball dels Jugadors</p>
                      <p className="text-xs text-slate-400 mb-3">Eix X: minuts totals · Eix Y: partits jugats · Passa per sobre per veure detalls</p>
                      <Scatterplot dades={fitxa.carrega_jugadors}/>
                    </div>

                    {/* Rendiment vs nivell rival */}
                    <div className="bg-white rounded-2xl border border-slate-200 p-4">
                      <p className="text-sm font-semibold text-slate-700 mb-3">🏆 Rendiment vs Nivell de Rival (pts/partit)</p>
                      <BarresV dades={fitxa.rend_vs_nivell.map(r=>({label:r.nivell,pts:r.avg_pts,n:r.n}))}
                        keys={["pts"]} colors={["#7c3aed"]} labels={["Pts/partit"]} height={140}/>
                    </div>

                    {/* Dependència golejador */}
                    <div className="bg-white rounded-2xl border border-slate-200 p-4">
                      <p className="text-sm font-semibold text-slate-700 mb-3">⭐ Dependència del Golejador</p>
                      {fitxa.dependencia_golejador.length===0
                        ? <p className="text-xs text-slate-400">Sense gols registrats</p>
                        : <>
                          <BarresV dades={fitxa.dependencia_golejador.map(j=>({label:j.player.split(",")[0].substring(0,14),gols:j.goals}))}
                            keys={["gols"]} colors={["#f59e0b"]} labels={["Gols"]} height={160}/>
                          <div className="mt-3 space-y-1">
                            {fitxa.dependencia_golejador.map((j,i)=>(
                              <div key={j.player} className="flex items-center gap-2 text-xs py-1 border-b border-slate-50">
                                <span className="text-slate-400 w-4">{i+1}</span>
                                <span className="flex-1 text-slate-700 truncate">{j.player}</span>
                                <span className="font-bold text-amber-600">{j.goals} ⚽</span>
                                <span className="text-slate-400 w-12 text-right">{j.pct}%</span>
                              </div>
                            ))}
                          </div>
                        </>}
                    </div>

                  </div>
                )}

                {/* ── CALENDARI ── */}
                {tab==="calendari"&&(
                  <div className="bg-white rounded-2xl border border-slate-200 overflow-hidden">
                    <div className="px-4 py-3 border-b border-slate-100 text-xs text-slate-500">
                      {fitxa.partits_calendari.filter(p=>p.goals_for!=null).length} jugats · {fitxa.partits_calendari.filter(p=>p.goals_for==null).length} pendents
                    </div>
                    <div className="overflow-x-auto">
                      <table className="w-full text-sm">
                        <thead>
                          <tr className="bg-slate-50 border-b border-slate-200 text-xs font-semibold text-slate-500 uppercase tracking-wide">
                            <th className="px-4 py-3 text-left">J</th>
                            <th className="px-4 py-3 text-left">Condició</th>
                            <th className="px-4 py-3 text-left">Rival</th>
                            <th className="px-4 py-3 text-center">Resultat</th>
                            <th className="px-4 py-3 text-center">Estat</th>
                          </tr>
                        </thead>
                        <tbody>
                          {fitxa.partits_calendari.map((p,i)=>{
                            // Pendent si goals_for és null/undefined (0-0 és resultat vàlid!)
                            const jugat = p.goals_for !== null && p.goals_for !== undefined;
                            const g=jugat&&p.goals_for>p.goals_against, e=jugat&&p.goals_for===p.goals_against;
                            return (
                              <tr key={i} className={`border-b border-slate-100 ${jugat?"hover:bg-slate-50":"bg-slate-50/40"}`}>
                                <td className="px-4 py-2.5 text-slate-500">J{p.jornada}</td>
                                <td className="px-4 py-2.5">
                                  <span className={`text-xs px-2 py-0.5 rounded-full font-medium ${p.home_away==="Home"?"bg-blue-100 text-blue-700":"bg-orange-100 text-orange-700"}`}>
                                    {p.home_away==="Home"?"🏠 Casa":"✈️ Fora"}
                                  </span>
                                </td>
                                <td className={`px-4 py-2.5 font-medium ${jugat?"text-slate-800":"text-slate-500"}`}>{p.opponent}</td>
                                <td className={`px-4 py-2.5 text-center font-bold ${g?"text-green-600":e?"text-yellow-600":jugat?"text-red-500":"text-slate-300"}`}>
                                  {jugat?`${p.goals_for} - ${p.goals_against}`:<span className="text-slate-400 font-normal text-xs">Pendent</span>}
                                </td>
                                <td className="px-4 py-2.5 text-center">
                                  {jugat
                                    ? <span className={`inline-flex items-center justify-center w-6 h-6 rounded-full text-xs font-bold ${g?"bg-green-100 text-green-700":e?"bg-yellow-100 text-yellow-700":"bg-red-100 text-red-500"}`}>{g?"G":e?"E":"D"}</span>
                                    : <span className="text-xs text-amber-600 bg-amber-50 px-2 py-0.5 rounded-full">🕐 Pendent</span>}
                                </td>
                              </tr>
                            );
                          })}
                        </tbody>
                      </table>
                    </div>
                  </div>
                )}

                {/* ── PLANTILLA ── */}
                {tab==="plantilla"&&fitxa.plantilla&&(
                  <div className="bg-white rounded-2xl border border-slate-200 overflow-hidden">
                    <div className="px-4 py-3 border-b border-slate-100 text-xs text-slate-500">
                      {fitxa.plantilla.length} jugadors · Clica la capçalera de columna per ordenar
                    </div>
                    <div className="overflow-x-auto">
                      <table className="w-full text-sm">
                        <thead>
                          <tr className="bg-slate-50 border-b border-slate-200 text-xs font-semibold text-slate-500 uppercase tracking-wide">
                            {[["player","Jugador",false],["matches_played","PJ",true],["starts","Tit.",true],
                              ["total_minutes","Min.",true],["goals","⚽",true],["goals_per_90","G/90",true],["cards_per_90","T/90",true],
                              ["rating","Rating",true],["impacte","Impacte",true]
                            ].map(([key,label,sortable])=>(
                              <th key={key} onClick={sortable?()=>toggleSort(key):null}
                                className={`px-3 py-3 ${key==="player"?"text-left":"text-center"} ${sortable?"cursor-pointer hover:bg-slate-100 select-none":""}`}>
                                {label}{sortable&&sortKey===key?(sortDir===-1?" ↓":" ↑"):""}
                              </th>
                            ))}
                          </tr>
                        </thead>
                        <tbody>
                          {[...fitxa.plantilla].sort((a,b)=>sortDir*((b[sortKey]||0)-(a[sortKey]||0))).map(j=>(
                            <tr key={j.player} className="border-b border-slate-100 hover:bg-green-50 transition-colors">
                              <td className="px-3 py-2.5 font-medium text-slate-800">{j.player}</td>
                              <td className="px-3 py-2.5 text-center text-slate-600">{j.matches_played}</td>
                              <td className="px-3 py-2.5 text-center text-slate-600">{j.starts}</td>
                              <td className="px-3 py-2.5 text-center text-slate-600">{j.total_minutes}'</td>
                              <td className="px-3 py-2.5 text-center font-semibold text-green-600">{j.goals>0?j.goals:"—"}</td>
                              <td className="px-3 py-2.5 text-center text-slate-500">{j.goals_per_90}</td>
                              <td className="px-3 py-2.5 text-center text-slate-500">{j.cards_per_90}</td>
                              <td className="px-3 py-2.5 text-center">
                                {j.rating!=null ? <span className={`inline-flex items-center justify-center w-8 h-8 rounded-full text-xs font-bold text-white ${j.rating>=70?"bg-green-500":j.rating>=50?"bg-yellow-500":"bg-red-400"}`}>{j.rating}</span> : <span className="text-slate-300 text-xs">—</span>}
                              </td>
                              <td className={`px-3 py-2.5 text-center text-xs font-semibold ${j.impacte>0?"text-green-600":j.impacte<0?"text-red-500":"text-slate-400"}`}>
                                {j.impacte!=null?(j.impacte>0?`+${j.impacte}`:j.impacte):"—"}
                              </td>
                            </tr>
                          ))}
                        </tbody>
                      </table>
                    </div>
                  </div>
                )}

                {/* ── COMPARADOR ── */}
                {tab==="comparador"&&<ComparadorEquips equips={equips} categoria={categoria} grup={grup}/>}
              </div>
            )}
          </div>
        </div>
      )}
    </div>
  );
}