"use client";
import { useEffect, useState } from "react";
import { useGrup } from "../components/GrupContext";

const API = process.env.NEXT_PUBLIC_API_URL || "http://localhost:8000";
const fmt2 = (v) => v != null ? parseFloat(v).toFixed(2) : "—";

// ── Radar 5 eixos ─────────────────────────────────────────────────────────────
function RadarJugador({ radar, color = "#1a6b3a", size = 300 }) {
  const labels = ["Gols/90","Impacte","Titularitat","Minuts","Fair-Play"];
  const keys   = ["radar_gol","radar_impacte","radar_titularitat","radar_minuts","radar_fairplay"];
  const n = labels.length;
  const cx = size/2, cy = size/2, R = size*0.36;
  const angle = (i) => -Math.PI/2 + (2*Math.PI/n)*i;
  const pt = (i, pct) => ({ x: cx + R*(pct/100)*Math.cos(angle(i)), y: cy + R*(pct/100)*Math.sin(angle(i)) });
  const grid = (pct) => keys.map((_,i) => `${i===0?"M":"L"} ${pt(i,pct).x} ${pt(i,pct).y}`).join(" ") + " Z";
  const data = keys.map((k,i) => `${i===0?"M":"L"} ${pt(i,radar?.[k]??50).x} ${pt(i,radar?.[k]??50).y}`).join(" ") + " Z";
  const lblR = R*1.22;
  return (
    <svg viewBox={`0 0 ${size} ${size}`} className="w-full">
      {[25,50,75,100].map(p => <path key={p} d={grid(p)} fill="none" stroke="#e2e8f0" strokeWidth={1}/>)}
      {keys.map((_,i) => { const e=pt(i,100); return <line key={i} x1={cx} y1={cy} x2={e.x} y2={e.y} stroke="#e2e8f0" strokeWidth={1}/>; })}
      <path d={data} fill={color} fillOpacity={0.25} stroke={color} strokeWidth={2}/>
      {keys.map((k,i) => { const p=pt(i,radar?.[k]??50); return <circle key={k} cx={p.x} cy={p.y} r={4} fill={color}/>; })}
      {labels.map((l,i) => { const p=pt(i,lblR/R*100); return <text key={l} x={p.x} y={p.y} textAnchor="middle" dominantBaseline="middle" fontSize={size*0.033} fill="#475569" fontWeight="500">{l}</text>; })}
      {keys.map((k,i) => { const p=pt(i,radar?.[k]??50); return <text key={`v-${k}`} x={p.x} y={p.y-9} textAnchor="middle" fontSize={size*0.03} fill={color} fontWeight="700">{radar?.[k]??50}</text>; })}
    </svg>
  );
}

// ── Radar dual ────────────────────────────────────────────────────────────────
function RadarDual({ r1, r2, name1, name2 }) {
  const labels = ["Gols/90","Impacte","Titularitat","Minuts","Fair-Play"];
  const keys   = ["radar_gol","radar_impacte","radar_titularitat","radar_minuts","radar_fairplay"];
  const n = labels.length;
  // viewBox gran per tenir molt marge per les etiquetes
  const W=520, H=480;
  const cx=W/2, cy=H/2-10, R=130;
  const angle=(i)=>-Math.PI/2+(2*Math.PI/n)*i;
  const pt=(i,pct)=>({x:cx+R*(pct/100)*Math.cos(angle(i)),y:cy+R*(pct/100)*Math.sin(angle(i))});
  const grid=(pct)=>keys.map((_,i)=>`${i===0?"M":"L"} ${pt(i,pct).x} ${pt(i,pct).y}`).join(" ")+" Z";
  const dp=(rad)=>keys.map((k,i)=>`${i===0?"M":"L"} ${pt(i,rad?.[k]??50).x} ${pt(i,rad?.[k]??50).y}`).join(" ")+" Z";

  // Per cada eix: posició de l'etiqueta força enfora, i anchor correcte
  const lblInfo = labels.map((l,i)=>{
    const ai = angle(i);
    const ax = Math.cos(ai), ay = Math.sin(ai);
    // distància etiqueta: prou lluny per no solapar
    const dist = R + 52;
    const lx = cx + dist * ax;
    const ly = cy + dist * ay;
    const anchor = ax > 0.2 ? "start" : ax < -0.2 ? "end" : "middle";
    // ajust vertical: si és dalt (ay negatiu) baixem una mica; si és baix pugem
    const dyLabel = ay < -0.3 ? -8 : ay > 0.3 ? 8 : 0;
    return { l, k: keys[i], lx, ly, anchor, dyLabel, i };
  });

  return (
    <svg viewBox={`0 0 ${W} ${H}`} className="w-full">
      {/* Graella */}
      {[25,50,75,100].map(p=><path key={p} d={grid(p)} fill="none" stroke="#e2e8f0" strokeWidth={1}/>)}
      {/* Eixos */}
      {keys.map((_,i)=>{const e=pt(i,100);return<line key={i} x1={cx} y1={cy} x2={e.x} y2={e.y} stroke="#e2e8f0" strokeWidth={1}/>;})}
      {/* Àrees */}
      <path d={dp(r1)} fill="rgba(26,107,138,0.2)"  stroke="rgba(26,107,138,1)"  strokeWidth={2}/>
      <path d={dp(r2)} fill="rgba(192,57,43,0.15)"  stroke="rgba(192,57,43,1)"   strokeWidth={2} strokeDasharray="5,3"/>
      {/* Punts */}
      {keys.map((k,i)=>{const p=pt(i,r1?.[k]??50);return<circle key={`a-${k}`} cx={p.x} cy={p.y} r={4} fill="rgba(26,107,138,1)"/>;})}
      {keys.map((k,i)=>{const p=pt(i,r2?.[k]??50);return<circle key={`b-${k}`} cx={p.x} cy={p.y} r={4} fill="rgba(192,57,43,1)"/>;})}

      {/* Etiquetes amb valors dels 2 jugadors a sota */}
      {lblInfo.map(({l, k, lx, ly, anchor, dyLabel})=>{
        const v1 = r1?.[k] ?? 50;
        const v2 = r2?.[k] ?? 50;
        // línia 1: nom de l'eix
        // línia 2: valor j1 (blau) · valor j2 (vermell)
        const lineH = 14;
        return (
          <g key={k}>
            <text x={lx} y={ly + dyLabel} textAnchor={anchor} fontSize={11} fill="#374151" fontWeight="600">
              {l}
            </text>
            {/* Valor jugador 1 */}
            <text x={lx} y={ly + dyLabel + lineH} textAnchor={anchor} fontSize={10} fill="rgba(26,107,138,1)" fontWeight="700">
              {v1}
            </text>
            {/* Valor jugador 2 */}
            <text x={lx} y={ly + dyLabel + lineH*2} textAnchor={anchor} fontSize={10} fill="rgba(192,57,43,1)" fontWeight="700">
              {v2}
            </text>
          </g>
        );
      })}
    </svg>
  );
}

// ── Gràfic línies ─────────────────────────────────────────────────────────────
function GraficLinies({ dades, series, height=180 }) {
  if (!dades?.length || dades.length<2) return null;
  const W=540, pL=32, pR=85, pT=12, pB=22, gW=W-pL-pR, gH=height-pT-pB;
  const vals=dades.flatMap(d=>series.map(s=>d[s.key])).filter(v=>v!=null);
  if (!vals.length) return null;
  const maxV=Math.max(...vals,1), xS=(i)=>pL+(i/(dades.length-1||1))*gW, yS=(v)=>pT+gH-(v/maxV)*gH;
  const step=Math.ceil(dades.length/8);
  return (
    <svg viewBox={`0 0 ${W} ${height}`} className="w-full">
      {[0,Math.round(maxV/2),maxV].map((v,gi)=>(
        <g key={gi}>
          <line x1={pL} x2={W-pR} y1={yS(v)} y2={yS(v)} stroke="#f1f5f9" strokeWidth={1}/>
          <text x={pL-4} y={yS(v)+4} textAnchor="end" fontSize={9} fill="#94a3b8">{v}</text>
        </g>
      ))}
      {dades.filter((_,i)=>i===0||i===dades.length-1||i%step===0).map(d=>{
        const i=dades.indexOf(d);
        return <text key={i} x={xS(i)} y={height-4} textAnchor="middle" fontSize={9} fill="#94a3b8">J{d.jornada}</text>;
      })}
      {series.map(s=>{
        const pts=dades.map((d,i)=>({i,v:d[s.key],dot:d[s.dotKey||"__"]>0})).filter(p=>p.v!=null);
        if (pts.length<2) return null;
        const path=pts.map((p,j)=>`${j===0?"M":"L"} ${xS(p.i)} ${yS(p.v)}`).join(" ");
        return (
          <g key={s.key}>
            <path d={path} fill="none" stroke={s.color} strokeWidth={2} strokeLinejoin="round"/>
            {pts.filter(p=>p.dot).map(p=><circle key={`m-${p.i}`} cx={xS(p.i)} cy={yS(p.v)} r={5} fill={s.dotColor||"#e74c3c"}/>)}
            {pts.map(p=><circle key={`d-${p.i}`} cx={xS(p.i)} cy={yS(p.v)} r={2.5} fill={s.color}/>)}
          </g>
        );
      })}
      {series.map((s,i)=>(
        <g key={s.label}>
          <rect x={W-pR+4} y={pT+i*16} width={10} height={10} fill={s.color} rx={2}/>
          <text x={W-pR+17} y={pT+i*16+9} fontSize={9} fill="#64748b">{(s.label||"").substring(0,13)}</text>
        </g>
      ))}
    </svg>
  );
}

// ── Barres verticals ──────────────────────────────────────────────────────────
function BarresV({ dades, keys, colors, labels, height=160 }) {
  if (!dades?.length) return null;
  const W=540, pL=36, pR=85, pT=8, pB=28, gH=height-pT-pB, gW=W-pL-pR;
  const bpg=keys.length, grpW=gW/dades.length, bW=Math.min(20,(grpW-6)/bpg);
  const maxV=Math.max(...dades.flatMap(d=>keys.map(k=>Math.abs(d[k]||0))),0.01);
  const yS=(v)=>pT+gH-(Math.abs(v)/maxV)*gH;
  return (
    <svg viewBox={`0 0 ${W} ${height}`} className="w-full">
      {[0,maxV/2,maxV].map(v=>(
        <g key={v}>
          <line x1={pL} x2={W-pR} y1={yS(v)} y2={yS(v)} stroke="#f1f5f9" strokeWidth={1}/>
          <text x={pL-4} y={yS(v)+4} textAnchor="end" fontSize={9} fill="#94a3b8">{Math.round(v*10)/10}</text>
        </g>
      ))}
      {dades.map((d,gi)=>{
        const gx=pL+gi*grpW+(grpW-bpg*bW-(bpg-1)*2)/2;
        return (
          <g key={gi}>
            {keys.map((k,ki)=>{
              const v=d[k]||0, bx=gx+ki*(bW+2), h=gH-(yS(v)-pT);
              return h>0?<rect key={k} x={bx} y={yS(v)} width={bW} height={h} fill={colors[ki]} rx={2}/>:null;
            })}
            <text x={pL+gi*grpW+grpW/2} y={height-5} textAnchor="middle" fontSize={9} fill="#64748b">
              {(d.label||d.bloc||"").substring(0,9)}
            </text>
          </g>
        );
      })}
      {labels.map((l,i)=>(
        <g key={l}>
          <rect x={W-pR+4} y={pT+i*16} width={10} height={10} fill={colors[i]} rx={2}/>
          <text x={W-pR+17} y={pT+i*16+9} fontSize={9} fill="#64748b">{(l||"").substring(0,13)}</text>
        </g>
      ))}
    </svg>
  );
}

// ── Fila comparativa ──────────────────────────────────────────────────────────
const CompRow = ({label,v1,v2,invert=false}) => {
  const n1=parseFloat(v1), n2=parseFloat(v2);
  const ok=!isNaN(n1)&&!isNaN(n2)&&v1!=="N/A"&&v2!=="N/A";
  const c1=ok?(invert?(n1<n2?"text-green-600":n1>n2?"text-red-500":"text-slate-600"):(n1>n2?"text-green-600":n1<n2?"text-red-500":"text-slate-600")):"text-slate-600";
  const c2=ok?(invert?(n2<n1?"text-green-600":n2>n1?"text-red-500":"text-slate-600"):(n2>n1?"text-green-600":n2<n1?"text-red-500":"text-slate-600")):"text-slate-600";
  return (
    <div className="flex items-center py-2 border-b border-slate-50 text-sm">
      <span className={`w-20 text-right font-bold ${c1}`}>{v1??""}</span>
      <span className="flex-1 text-center text-xs text-slate-400 px-1">{label}</span>
      <span className={`w-20 font-bold ${c2}`}>{v2??""}</span>
    </div>
  );
};

// ── Badge de rating ───────────────────────────────────────────────────────────
const RatingBadge = ({v}) => {
  if (v==null) return <span className="text-slate-300 text-xs">—</span>;
  const bg=v>=70?"bg-green-500":v>=50?"bg-yellow-500":"bg-red-400";
  return <span className={`inline-flex items-center justify-center w-10 h-10 rounded-full text-white text-sm font-black ${bg}`}>{v}</span>;
};

// ── Fitxa completa del jugador (pestanya dedicada, com el modal del Shiny) ────
function FitxaJugador({ nom, onTancar }) {
  const { categoria, grup } = useGrup();
  const [fitxa, setFitxa] = useState(null);
  const [loading, setLoading] = useState(true);

  useEffect(() => {
    setLoading(true); setFitxa(null);
    fetch(`${API}/jugador/${categoria}/${grup}/${encodeURIComponent(nom)}/complet`)
      .then(r => r.json()).then(d => { setFitxa(d); setLoading(false); })
      .catch(() => setLoading(false));
  }, [nom, categoria, grup]);

  if (loading) return (
    <div className="flex items-center justify-center py-24 text-slate-400">
      <div className="animate-spin text-2xl mr-3">⟳</div>Carregant fitxa...
    </div>
  );
  if (!fitxa) return <div className="text-red-500 p-4">Error carregant la fitxa</div>;

  const s = fitxa.stats || {};
  const hist = fitxa.historial || [];
  const ev   = fitxa.events   || [];
  const timing = fitxa.timing_gols || [];
  const radar = fitxa.radar || {};

  const ratingCls = (s.rating>=70)?"from-green-700 to-green-500":(s.rating>=50)?"from-yellow-600 to-yellow-400":"from-red-700 to-red-500";

  return (
    <div className="space-y-4">
      {/* Botó tornar */}
      <button onClick={onTancar} className="flex items-center gap-2 text-sm text-slate-500 hover:text-slate-700 transition-colors">
        <span>←</span> Tornar al buscador
      </button>

      {/* Capçalera — equivalent al modal-header del Shiny */}
      <div className="bg-white rounded-2xl border border-slate-200 overflow-hidden">
        <div className={`bg-gradient-to-br ${ratingCls} text-white p-6`}>
          <div className="flex items-start justify-between gap-4">
            <div>
              <h2 className="text-2xl font-bold leading-tight">{nom}</h2>
              <p className="text-sm opacity-80 mt-1">{s.team}</p>
            </div>
            <div className="text-center shrink-0 bg-white/10 rounded-2xl px-6 py-3">
              <p className="text-6xl font-black leading-none">{s.rating ?? "N/A"}</p>
              <p className="text-xs opacity-75 uppercase tracking-widest mt-1">Rating del Jugador</p>
            </div>
          </div>
        </div>

        {/* Layout 2 columnes com al Shiny: col-4 esquerra, col-8 dreta */}
        <div className="flex gap-0 divide-x divide-slate-100">
          {/* Columna esquerra: radar + últims partits */}
          <div className="w-72 shrink-0 p-4 space-y-4">
            <div>
              <p className="text-xs font-semibold text-slate-500 uppercase tracking-wide mb-2">⬡ Radar</p>
              <RadarJugador radar={radar} color="#1a6b3a"/>
            </div>

            {/* Últims partits — igual que modal_ultims_partits del Shiny */}
            {hist.length > 0 && (
              <div>
                <p className="text-xs font-semibold text-slate-500 uppercase tracking-wide mb-2">Últims partits</p>
                <div className="bg-slate-50 rounded-xl overflow-hidden">
                  <table className="w-full text-xs">
                    <thead>
                      <tr className="text-slate-400 border-b border-slate-200">
                        <th className="px-2 py-1.5 text-left font-semibold">Partit</th>
                        <th className="px-2 py-1.5 text-center font-semibold">Min</th>
                        <th className="px-2 py-1.5 text-center font-semibold">Evts</th>
                      </tr>
                    </thead>
                    <tbody>
                      {[...hist].reverse().slice(0,8).map((h,i) => {
                        const gols=ev.filter(e=>e.jornada===h.jornada&&e.event_type==="Gol");
                        const grog=ev.filter(e=>e.jornada===h.jornada&&e.event_type==="Targeta Groga");
                        const verm=ev.filter(e=>e.jornada===h.jornada&&e.event_type==="Targeta Vermella");
                        const icons=[...gols.map(()=>"⚽"),...grog.map(()=>"🟨"),...verm.map(()=>"🟥"),
                          ...(h.starter===0&&h.minutes_played>0?["⬆️"]:h.starter===1&&h.minutes_played<88?["⬇️"]:[])];
                        const jugat = h.goals_team!=null;
                        const res = jugat ? ` (${h.goals_team}-${h.goals_opp})` : "";
                        const lbl = `J${h.jornada} ${h.home_away==="Casa"?"C":"F"} vs ${(h.rival||"—").substring(0,15)}${res}`;
                        return (
                          <tr key={i} className="border-b border-slate-100 last:border-0">
                            <td className="px-2 py-1.5 text-slate-600">{lbl}</td>
                            <td className="px-2 py-1.5 text-center font-bold text-blue-600">{h.minutes_played}'</td>
                            <td className="px-2 py-1.5 text-center">{icons.join("")||"—"}</td>
                          </tr>
                        );
                      })}
                    </tbody>
                  </table>
                </div>
              </div>
            )}
          </div>

          {/* Columna dreta: vboxes + gràfics */}
          <div className="flex-1 p-4 space-y-4">
            {/* 8 vboxes: equip, gols, minuts, partits, titularitats, impacte, grogues, vermelles */}
            <div className="grid grid-cols-4 gap-2">
              {[
                ["👥","Equip",       s.team,          "#2980b9"],
                ["⚽","Gols",        s.goals,          "#27ae60"],
                ["⏱","Minuts",      (s.total_minutes??0)+"'", "#2980b9"],
                ["📅","Partits",     s.matches_played, "#e67e22"],
                ["⭐","Titularitats",s.starts,         "#8e44ad"],
                ["⚡","Impacte",     s.impacte!=null?(s.impacte>0?"+"+s.impacte:s.impacte)+" pts":"N/A",
                  s.impacte>0?"#27ae60":s.impacte<0?"#e74c3c":"#7f8c8d"],
                ["🟨","Grogues",    s.yellow_cards??0, "#f39c12"],
                ["🟥","Vermelles",  s.red_cards??0,    "#e74c3c"],
              ].map(([ico,label,val,col])=>(
                <div key={label} className="bg-slate-50 rounded-xl p-2.5 flex items-center gap-2">
                  <span className="text-xl">{ico}</span>
                  <div>
                    <p className="font-bold text-sm" style={{color:col}}>{val ?? "—"}</p>
                    <p className="text-xs text-slate-400 leading-tight">{label}</p>
                  </div>
                </div>
              ))}
            </div>

            <hr className="border-slate-100"/>

            {/* Gols acumulats */}
            {hist.length > 1 && (
              <div>
                <p className="text-xs font-semibold text-slate-500 uppercase tracking-wide mb-2">⚽ Gols Acumulats</p>
                <GraficLinies dades={hist}
                  series={[{key:"gols_acumulats",color:"#27ae60",label:"Gols acum.",dotKey:"goals",dotColor:"#e74c3c"}]}
                  height={160}/>
              </div>
            )}

            {/* Minuts per jornada */}
            {hist.length > 0 && (
              <div>
                <p className="text-xs font-semibold text-slate-500 uppercase tracking-wide mb-2">⏱ Minuts per Jornada</p>
                <BarresV
                  dades={hist.map(h=>({label:`J${h.jornada}`,tit:h.starter?h.minutes_played:0,sup:!h.starter&&h.minutes_played>0?h.minutes_played:0}))}
                  keys={["tit","sup"]} colors={["#2980b9","#e67e22"]} labels={["Titular","Suplent"]}/>
              </div>
            )}

            {/* Impacte — rendiment equip amb/sense */}
            {s.avg_pts_jugant!=null && s.avg_pts_sense!=null && (
              <div>
                <p className="text-xs font-semibold text-slate-500 uppercase tracking-wide mb-2">🏟 Rendiment de l'Equip amb/sense el Jugador</p>
                <BarresV
                  dades={[
                    {label:"Quan juga",    pts:s.avg_pts_jugant},
                    {label:"Quan NO juga", pts:s.avg_pts_sense},
                  ]}
                  keys={["pts"]}
                  colors={[s.avg_pts_jugant>=s.avg_pts_sense?"#27ae60":"#e74c3c","#e74c3c"]}
                  labels={["Pts promig"]} height={140}/>
                <p className="text-xs text-slate-400 text-center mt-1">
                  {s.games_played} partits jugant · {s.games_not_played} sense jugar
                </p>
              </div>
            )}

            {/* Timing de gols */}
            {timing.some(t=>t.gols>0) && (
              <div>
                <p className="text-xs font-semibold text-slate-500 uppercase tracking-wide mb-2">🎯 Distribució de Gols per Minut</p>
                <BarresV dades={timing.map(t=>({label:t.bloc,gols:t.gols}))} keys={["gols"]} colors={["#f59e0b"]} labels={["Gols"]} height={140}/>
              </div>
            )}

            {/* Stats per 90 */}
            <div>
              <p className="text-xs font-semibold text-slate-500 uppercase tracking-wide mb-2">📊 Estadístiques per 90'</p>
              <div className="grid grid-cols-2 gap-2">
                {[
                  ["G/90",   fmt2(s.goals_per_90), "#27ae60"],
                  ["T/90",   fmt2(s.cards_per_90), "#f39c12"],
                ].map(([l,v,c])=>(
                  <div key={l} className="bg-slate-50 rounded-xl p-2 text-center">
                    <p className="font-bold" style={{color:c}}>{v}</p>
                    <p className="text-xs text-slate-400">{l}</p>
                  </div>
                ))}
              </div>
            </div>
          </div>
        </div>
      </div>
    </div>
  );
}

// ── Comparador de jugadors ────────────────────────────────────────────────────
function ComparadorJugadors({ tots }) {
  const { categoria, grup } = useGrup();
  const [j1,setJ1]=useState(""), [j2,setJ2]=useState("");
  const [dades,setDades]=useState(null), [loading,setLoading]=useState(false);

  const comparar = () => {
    if (!j1||!j2||j1===j2) return;
    setLoading(true); setDades(null);
    Promise.all([
      fetch(`${API}/jugador/${categoria}/${grup}/${encodeURIComponent(j1)}/complet`).then(r=>r.json()),
      fetch(`${API}/jugador/${categoria}/${grup}/${encodeURIComponent(j2)}/complet`).then(r=>r.json()),
    ]).then(([d1,d2])=>{setDades({d1,d2});setLoading(false);}).catch(()=>setLoading(false));
  };

  const sorted=[...tots].sort((a,b)=>a.player.localeCompare(b.player));

  return (
    <div className="space-y-4">
      {/* Selectors amb cerca per escriptura */}
      <div className="bg-white rounded-2xl border border-slate-200 p-4">
        <p className="text-sm font-semibold text-slate-700 mb-3">Selecciona dos jugadors per comparar</p>
        <datalist id="dl-j1">{sorted.map(j=><option key={`${j.player}-${j.team}`} value={j.player}>{j.player} — {j.team.substring(0,18)}</option>)}</datalist>
        <datalist id="dl-j2">{sorted.filter(j=>j.player!==j1).map(j=><option key={`${j.player}-${j.team}`} value={j.player}>{j.player} — {j.team.substring(0,18)}</option>)}</datalist>
        <div className="flex gap-3 flex-wrap">
          <input list="dl-j1" value={j1} onChange={e=>setJ1(e.target.value)} placeholder="🔍 Jugador 1 (escriu per cercar)..."
            className="flex-1 min-w-[220px] px-3 py-2 text-sm border border-slate-200 rounded-lg focus:outline-none focus:border-green-400"/>
          <input list="dl-j2" value={j2} onChange={e=>setJ2(e.target.value)} placeholder="🔍 Jugador 2 (escriu per cercar)..."
            className="flex-1 min-w-[220px] px-3 py-2 text-sm border border-slate-200 rounded-lg focus:outline-none focus:border-green-400"/>
          <button onClick={comparar} disabled={!j1||!j2||j1===j2||loading}
            className="px-5 py-2 bg-green-600 text-white text-sm font-semibold rounded-lg hover:bg-green-700 disabled:opacity-40">
            {loading?"Carregant...":"Comparar"}
          </button>
        </div>
        <p className="text-xs text-slate-400 mt-2">Escriu el nom o el cognom per filtrar la llista</p>
      </div>

      {dades&&(()=>{
        const {d1,d2}=dades;
        const s1=d1.stats||{}, s2=d2.stats||{};
        const h1=d1.historial||[], h2=d2.historial||[];
        const t1=d1.timing_gols||[], t2=d2.timing_gols||[];

        return (
          <div className="space-y-4">
            {/* Capçalera resum — com compjug_header del Shiny */}
            <div className="bg-white rounded-2xl border border-slate-200 p-5">
              <div className="grid grid-cols-3 text-center gap-4">
                <div>
                  <p className="font-bold text-slate-800 truncate">{j1}</p>
                  <p className="text-xs text-slate-400">{s1.team?.substring(0,22)}</p>
                  <div className="mt-2 flex justify-center"><RatingBadge v={s1.rating}/></div>
                  <div className="mt-1 text-xs bg-slate-100 text-slate-600 rounded-full px-3 py-0.5 inline-block">
                    Impacte: {s1.impacte!=null?(s1.impacte>0?"+"+s1.impacte:s1.impacte):"N/A"}
                  </div>
                </div>
                <div className="flex items-center justify-center text-slate-300 text-2xl font-light">VS</div>
                <div>
                  <p className="font-bold text-slate-800 truncate">{j2}</p>
                  <p className="text-xs text-slate-400">{s2.team?.substring(0,22)}</p>
                  <div className="mt-2 flex justify-center"><RatingBadge v={s2.rating}/></div>
                  <div className="mt-1 text-xs bg-slate-100 text-slate-600 rounded-full px-3 py-0.5 inline-block">
                    Impacte: {s2.impacte!=null?(s2.impacte>0?"+"+s2.impacte:s2.impacte):"N/A"}
                  </div>
                </div>
              </div>
            </div>

            {/* Radars comparats */}
            <div className="bg-white rounded-2xl border border-slate-200 p-4">
              <p className="text-sm font-semibold text-slate-700 mb-2">⬡ Radars comparats</p>
              <div className="flex justify-center gap-6 text-xs mb-2">
                <span className="flex items-center gap-1.5"><span className="w-3 h-3 rounded-full bg-[#1a6b8a] inline-block"/>{j1.substring(0,20)}</span>
                <span className="flex items-center gap-1.5"><span className="w-3 h-3 rounded-full bg-[#c0392b] inline-block"/>{j2.substring(0,20)}</span>
              </div>
              <RadarDual r1={d1.radar} r2={d2.radar} name1={j1} name2={j2}/>
            </div>

            {/* Estadístiques detallades — compjug_stats_taula */}
            <div className="bg-white rounded-2xl border border-slate-200 p-4">
              <p className="text-sm font-semibold text-slate-700 mb-3">📊 Estadístiques detallades</p>
              <div className="flex text-xs text-slate-400 justify-between mb-1 px-1">
                <span className="truncate max-w-[130px]">{j1.split(",")[0].substring(0,16)}</span>
                <span className="truncate max-w-[130px] text-right">{j2.split(",")[0].substring(0,16)}</span>
              </div>
              <CompRow label="Equip"         v1={s1.team?.substring(0,12)??""} v2={s2.team?.substring(0,12)??""}/>
              <CompRow label="Partits"       v1={s1.matches_played}  v2={s2.matches_played}/>
              <CompRow label="Titularitats"  v1={s1.starts}          v2={s2.starts}/>
              <CompRow label="Minuts totals" v1={s1.total_minutes}   v2={s2.total_minutes}/>
              <CompRow label="Gols"          v1={s1.goals}           v2={s2.goals}/>
              <CompRow label="Gols/90'"      v1={fmt2(s1.goals_per_90)} v2={fmt2(s2.goals_per_90)}/>
              <CompRow label="Targetes/90'"  v1={fmt2(s1.cards_per_90)} v2={fmt2(s2.cards_per_90)} invert/>
              <CompRow label="Impacte"       v1={s1.impacte!=null?(s1.impacte>0?"+"+s1.impacte:s1.impacte):"N/A"} v2={s2.impacte!=null?(s2.impacte>0?"+"+s2.impacte:s2.impacte):"N/A"}/>
            </div>

            {/* Gols acumulats — compjug_gols_evolucio */}
            {h1.length>1&&h2.length>1&&(
              <div className="bg-white rounded-2xl border border-slate-200 p-4">
                <p className="text-sm font-semibold text-slate-700 mb-3">⚽ Gols Acumulats</p>
                {(()=>{
                  const n=Math.min(h1.length,h2.length);
                  const merged=h1.slice(0,n).map((h,i)=>({jornada:h.jornada,j1:h.gols_acumulats,j2:h2[i]?.gols_acumulats??null}));
                  return <GraficLinies dades={merged} series={[{key:"j1",color:"#1a6b8a",label:j1.split(",")[0].substring(0,13)},{key:"j2",color:"#c0392b",label:j2.split(",")[0].substring(0,13)}]}/>;
                })()}
              </div>
            )}

            {/* Minuts per jornada — compjug_minuts */}
            {h1.length>0&&h2.length>0&&(
              <div className="bg-white rounded-2xl border border-slate-200 p-4">
                <p className="text-sm font-semibold text-slate-700 mb-3">⏱ Minuts per Jornada</p>
                {(()=>{
                  const n=Math.min(h1.length,h2.length);
                  const merged=h1.slice(0,n).map((h,i)=>({label:`J${h.jornada}`,j1:h.minutes_played,j2:h2[i]?.minutes_played??0}));
                  return <BarresV dades={merged} keys={["j1","j2"]} colors={["#1a6b8a","#c0392b"]} labels={[j1.split(",")[0].substring(0,13),j2.split(",")[0].substring(0,13)]}/>;
                })()}
              </div>
            )}

            {/* Timing de gols — compjug_timing */}
            <div className="bg-white rounded-2xl border border-slate-200 p-4">
              <p className="text-sm font-semibold text-slate-700 mb-3">🎯 Timing de Gols</p>
              <BarresV
                dades={t1.map((t,i)=>({label:t.bloc,j1:t.gols,j2:t2[i]?.gols??0}))}
                keys={["j1","j2"]} colors={["#1a6b8a","#c0392b"]}
                labels={[j1.split(",")[0].substring(0,13),j2.split(",")[0].substring(0,13)]}/>
            </div>

            {/* Rendiment amb/sense — compjug_impacte */}
            {(s1.avg_pts_jugant!=null||s2.avg_pts_jugant!=null)&&(
              <div className="bg-white rounded-2xl border border-slate-200 p-4">
                <p className="text-sm font-semibold text-slate-700 mb-3">🏟 Rendiment de l'Equip amb/sense cada Jugador</p>
                <BarresV
                  dades={[
                    {label:"Quan juga",    j1:s1.avg_pts_jugant??0, j2:s2.avg_pts_jugant??0},
                    {label:"Quan NO juga", j1:s1.avg_pts_sense??0,  j2:s2.avg_pts_sense??0},
                  ]}
                  keys={["j1","j2"]} colors={["#1a6b8a","#c0392b"]}
                  labels={[j1.split(",")[0].substring(0,13),j2.split(",")[0].substring(0,13)]}
                  height={130}/>
              </div>
            )}
          </div>
        );
      })()}
    </div>
  );
}

// ── Component principal ───────────────────────────────────────────────────────
const COLS=[
  {key:"player",        label:"Jugador",     sortable:false},
  {key:"team",          label:"Equip",       sortable:false, hide:true},
  {key:"rating",        label:"Rating",      sortable:true,  center:true},
  {key:"matches_played",label:"PJ",          sortable:true,  center:true},
  {key:"starts",        label:"Tit.",        sortable:true,  center:true},
  {key:"total_minutes", label:"Min.",        sortable:true,  center:true},
  {key:"goals",         label:"⚽",          sortable:true,  center:true},
  {key:"goals_per_90",  label:"G/90",        sortable:true,  center:true},
  {key:"cards_per_90",  label:"T/90",        sortable:true,  center:true, invert:true},
  {key:"impacte",       label:"Impacte",     sortable:true,  center:true},
];

export default function JugadorsPage() {
  const { categoria, grup } = useGrup();
  const [tab,       setTab]      = useState("buscador");
  const [tots,      setTots]     = useState([]);
  const [loading,   setLoading]  = useState(true);
  const [error,     setError]    = useState(null);
  const [cerca,     setCerca]    = useState("");
  const [minMin,    setMinMin]   = useState(0);
  const [sortKey,   setSortKey]  = useState("rating");
  const [sortDir,   setSortDir]  = useState(-1);
  const [fitxaObert,setFitxaObert] = useState(null);  // nom del jugador, o null

  useEffect(()=>{
    setLoading(true); setError(null); setTots([]); setFitxaObert(null);
    fetch(`${API}/jugadors/${categoria}/${grup}/complets`)
      .then(r=>{ if(!r.ok) throw new Error(`HTTP ${r.status}`); return r.json(); })
      .then(d=>{ setTots(d.jugadors||[]); setLoading(false); })
      .catch(e=>{ setError(e.message); setLoading(false); });
  },[categoria,grup]);

  const toggleSort=(key)=>{ if(sortKey===key) setSortDir(d=>-d); else{setSortKey(key);setSortDir(-1);} };

  const filtrats=tots
    .filter(j=>j.total_minutes>=minMin)
    .filter(j=>!cerca||j.player.toLowerCase().includes(cerca.toLowerCase())||j.team.toLowerCase().includes(cerca.toLowerCase()))
    .sort((a,b)=>sortDir*((b[sortKey]??-Infinity)-(a[sortKey]??-Infinity)));

  // Si hi ha fitxa oberta, mostrem la fitxa en lloc del buscador
  if (fitxaObert) {
    return (
      <div>
        <h1 className="text-2xl font-bold text-slate-900 mb-4">Jugadors</h1>
        <FitxaJugador nom={fitxaObert} onTancar={()=>setFitxaObert(null)}/>
      </div>
    );
  }

  return (
    <div>
      <h1 className="text-2xl font-bold text-slate-900 mb-6">Jugadors</h1>

      {/* Tabs */}
      <div className="flex gap-1 bg-slate-100 p-1 rounded-xl mb-5 w-fit">
        {[{id:"buscador",label:"🔍 Buscador"},{id:"comparador",label:"⚖️ Comparador"}].map(t=>(
          <button key={t.id} onClick={()=>setTab(t.id)}
            className={`px-4 py-2 rounded-lg text-sm font-medium transition-all ${tab===t.id?"bg-white text-slate-800 shadow-sm":"text-slate-500 hover:text-slate-700"}`}>
            {t.label}
          </button>
        ))}
      </div>

      {loading&&<div className="flex items-center justify-center py-24 text-slate-400"><div className="animate-spin text-2xl mr-3">⟳</div>Calculant ratings i impacte...</div>}
      {error  &&<div className="bg-red-50 border border-red-200 rounded-xl p-4 text-red-600 text-sm">Error: {error}</div>}

      {/* ── BUSCADOR ── */}
      {!loading&&!error&&tab==="buscador"&&(
        <div>
          {/* Filtres */}
          <div className="flex gap-3 mb-3 flex-wrap items-center">
            <input value={cerca} onChange={e=>setCerca(e.target.value)} placeholder="🔍 Cerca jugador o equip..."
              className="flex-1 min-w-[200px] px-3 py-2 text-sm border border-slate-200 rounded-lg focus:outline-none focus:border-green-400"/>
            <select value={minMin} onChange={e=>setMinMin(Number(e.target.value))}
              className="px-3 py-2 text-sm border border-slate-200 rounded-lg bg-white focus:outline-none focus:border-green-400">
              <option value={0}>Tots els jugadors</option>
              <option value={45}>Mín. 45'</option>
              <option value={180}>Mín. 180'</option>
              <option value={450}>Mín. 450'</option>
              <option value={900}>Mín. 900'</option>
            </select>
            <span className="text-xs text-slate-400 shrink-0">{filtrats.length} jugadors</span>
          </div>
          <p className="text-xs text-slate-400 mb-2">Clica qualsevol jugador per veure la seva fitxa completa</p>

          <div className="bg-white rounded-2xl border border-slate-200 overflow-hidden">
            <div className="overflow-x-auto">
              <table className="w-full text-sm">
                <thead>
                  <tr className="bg-slate-50 border-b border-slate-200 text-xs font-semibold text-slate-500 uppercase tracking-wide">
                    {COLS.map(c=>(
                      <th key={c.key} onClick={c.sortable?()=>toggleSort(c.key):null}
                        className={`px-3 py-3 ${c.center?"text-center":"text-left"} ${c.hide?"hidden md:table-cell":""} ${c.sortable?"cursor-pointer hover:bg-slate-100 select-none":""}`}>
                        {c.label}{c.sortable&&sortKey===c.key?(sortDir===-1?" ↓":" ↑"):""}
                      </th>
                    ))}
                  </tr>
                </thead>
                <tbody>
                  {filtrats.map(j=>(
                    <tr key={`${j.player}-${j.team}`}
                      onClick={()=>setFitxaObert(j.player)}
                      className="border-b border-slate-100 cursor-pointer hover:bg-green-50 transition-colors">
                      <td className="px-3 py-2.5 font-medium text-green-700 hover:underline">{j.player}</td>
                      <td className="px-3 py-2.5 text-slate-500 text-xs hidden md:table-cell truncate max-w-[150px]">{j.team}</td>
                      <td className="px-3 py-2.5 text-center"><RatingBadge v={j.rating}/></td>
                      <td className="px-3 py-2.5 text-center text-slate-600">{j.matches_played}</td>
                      <td className="px-3 py-2.5 text-center text-slate-600">{j.starts}</td>
                      <td className="px-3 py-2.5 text-center text-slate-600">{j.total_minutes}'</td>
                      <td className="px-3 py-2.5 text-center font-semibold text-green-600">{j.goals>0?j.goals:"—"}</td>
                      <td className="px-3 py-2.5 text-center text-slate-500">{fmt2(j.goals_per_90)}</td>
                      <td className="px-3 py-2.5 text-center text-slate-500">{fmt2(j.cards_per_90)}</td>
                      <td className="px-3 py-2.5 text-center">
                        {j.impacte!=null&&j.impacte!==0?(
                          <span className={`text-xs font-semibold px-1.5 py-0.5 rounded ${j.impacte>0.3?"bg-green-50 text-green-600":j.impacte<-0.3?"bg-red-50 text-red-500":"bg-slate-100 text-slate-500"}`}>
                            {j.impacte>0?"+":""}{j.impacte}
                          </span>
                        ):<span className="text-slate-300 text-xs">—</span>}
                      </td>
                    </tr>
                  ))}
                </tbody>
              </table>
            </div>
          </div>
        </div>
      )}

      {/* ── COMPARADOR ── */}
      {!loading&&!error&&tab==="comparador"&&(
        <ComparadorJugadors tots={tots}/>
      )}
    </div>
  );
}