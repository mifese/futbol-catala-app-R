"use client";
import { useEffect, useState } from "react";
import { useGrup } from "../components/GrupContext";

const API = process.env.NEXT_PUBLIC_API_URL || "http://localhost:8000";

const KpiBox = ({ icon, label, value, color = "text-slate-800" }) => (
  <div className="bg-white rounded-2xl border border-slate-200 p-4 text-center">
    <div className="text-3xl mb-1">{icon}</div>
    <p className={`text-2xl font-black ${color}`}>{value}</p>
    <p className="text-xs text-slate-500 mt-0.5">{label}</p>
  </div>
);

function BarresV({ dades, keys, colors, labels, height = 220, showAvg = null }) {
  if (!dades?.length) return null;
  const W = 580, pL = 38, pR = 90, pT = 12, pB = 28;
  const gH = height - pT - pB, gW = W - pL - pR;
  const bpg = keys.length, grpW = gW / dades.length, bW = Math.min(18, (grpW - 4) / bpg);
  // Per colors per barra (quan colors és array de strings per barra)
  const getColor = (ki, gi) => {
    if (Array.isArray(colors) && typeof colors[0] === 'string' && colors.length === dades.length)
      return colors[gi];
    return Array.isArray(colors) ? colors[ki] : colors;
  };
  const maxV = Math.max(...dades.flatMap(d => keys.map(k => Math.abs(d[k] || 0))), 0.01);
  const yS = (v) => pT + gH - (Math.abs(v) / maxV) * gH;
  const step = Math.ceil(dades.length / 12);
  return (
    <svg viewBox={`0 0 ${W} ${height}`} className="w-full">
      {[0, maxV / 2, maxV].map(v => (
        <g key={v}>
          <line x1={pL} x2={W - pR} y1={yS(v)} y2={yS(v)} stroke="#f1f5f9" strokeWidth={1} />
          <text x={pL - 4} y={yS(v) + 4} textAnchor="end" fontSize={9} fill="#94a3b8">{Math.round(v * 10) / 10}</text>
        </g>
      ))}
      {showAvg != null && (
        <line x1={pL} x2={W - pR} y1={yS(showAvg)} y2={yS(showAvg)} stroke="#e74c3c" strokeWidth={1.5} strokeDasharray="4,3" />
      )}
      {dades.map((d, gi) => {
        const gx = pL + gi * grpW + (grpW - bpg * bW - (bpg - 1) * 2) / 2;
        return (
          <g key={gi}>
            {keys.map((k, ki) => {
              const v = d[k] || 0, bx = gx + ki * (bW + 2), h = gH - (yS(v) - pT);
              return h > 0 ? <rect key={k} x={bx} y={yS(v)} width={bW} height={h} fill={getColor(ki, gi)} rx={2} /> : null;
            })}
            {gi % step === 0 && (
              <text x={pL + gi * grpW + grpW / 2} y={height - 5} textAnchor="middle" fontSize={9} fill="#94a3b8">
                {d.label ?? (d.jornada != null ? `J${d.jornada}` : d.resultat ?? "")}
              </text>
            )}
          </g>
        );
      })}
      {/* Llegenda: si colors no és per-barra */}
      {!(Array.isArray(colors) && colors.length === dades.length) && labels.map((l, i) => (
        <g key={l}>
          <rect x={W - pR + 4} y={pT + i * 16} width={10} height={10} fill={Array.isArray(colors) ? colors[i] : colors} rx={2} />
          <text x={W - pR + 17} y={pT + i * 16 + 9} fontSize={9} fill="#64748b">{l}</text>
        </g>
      ))}
    </svg>
  );
}

function HistogramGols({ dades, height = 220 }) {
  if (!dades?.length) return null;
  const W = 580, pL = 38, pR = 20, pT = 12, pB = 28;
  const gH = height - pT - pB, gW = W - pL - pR;
  const maxV = Math.max(...dades.map(d => d.gols), 1);
  const bW = gW / dades.length - 1;
  const yS = (v) => pT + gH - (v / maxV) * gH;
  const xOfMin = (m) => {
    const idx = dades.findIndex(d => d.minut >= m);
    return idx >= 0 ? pL + idx * (bW + 1) : null;
  };
  return (
    <svg viewBox={`0 0 ${W} ${height}`} className="w-full">
      {[0, Math.round(maxV / 2), maxV].map(v => (
        <g key={v}>
          <line x1={pL} x2={W - pR} y1={yS(v)} y2={yS(v)} stroke="#f1f5f9" strokeWidth={1} />
          <text x={pL - 4} y={yS(v) + 4} textAnchor="end" fontSize={9} fill="#94a3b8">{v}</text>
        </g>
      ))}
      {xOfMin(45) && <line x1={xOfMin(45)} x2={xOfMin(45)} y1={pT} y2={pT + gH} stroke="#e74c3c" strokeWidth={1.5} strokeDasharray="4,3" />}
      {dades.map((d, i) => {
        const x = pL + i * (bW + 1);
        const h = gH - (yS(d.gols) - pT);
        return h > 0 ? <rect key={i} x={x} y={yS(d.gols)} width={bW} height={h} fill="#3498db" opacity={0.8} rx={1} /> : null;
      })}
      {[0, 15, 30, 45, 60, 75, 90].map(m => {
        const x = xOfMin(m);
        return x ? <text key={m} x={x} y={height - 4} fontSize={9} fill="#94a3b8">{m}'</text> : null;
      })}
    </svg>
  );
}

function EquipBiBarres({ dades, key1, key2, col1, col2, l1, l2, height }) {
  if (!dades?.length) return null;
  const H = height || dades.length * 28 + 30;
  const W = 580, pL = 160, pR = 55, pT = 8;
  const gW = W - pL - pR, rowH = (H - pT) / dades.length;
  const maxV = Math.max(...dades.flatMap(d => [d[key1] || 0, d[key2] || 0]), 0.01);
  const bW = (v) => (v / maxV) * gW;
  return (
    <div>
      <svg viewBox={`0 0 ${W} ${H}`} className="w-full">
        {dades.map((d, i) => {
          const y = pT + i * rowH, bh = Math.min(rowH / 2 - 2, 10);
          return (
            <g key={d.team}>
              <text x={pL - 5} y={y + rowH / 2 + 3} textAnchor="end" fontSize={9} fill="#475569">{(d.team || "").substring(0, 22)}</text>
              <rect x={pL} y={y + 2} width={bW(d[key1] || 0)} height={bh} fill={col1} rx={2} opacity={0.85} />
              <rect x={pL} y={y + bh + 4} width={bW(d[key2] || 0)} height={bh} fill={col2} rx={2} opacity={0.85} />
              <text x={pL + bW(d[key1] || 0) + 3} y={y + bh / 2 + 4} fontSize={8} fill={col1}>{d[key1]}</text>
              <text x={pL + bW(d[key2] || 0) + 3} y={y + bh + bh / 2 + 6} fontSize={8} fill={col2}>{d[key2]}</text>
            </g>
          );
        })}
      </svg>
      <div className="flex gap-4 mt-1 text-xs">
        <span className="flex items-center gap-1"><span className="w-3 h-2 rounded inline-block" style={{background:col1}}/>{l1}</span>
        <span className="flex items-center gap-1"><span className="w-3 h-2 rounded inline-block" style={{background:col2}}/>{l2}</span>
      </div>
    </div>
  );
}

function TargetesEquip({ dades }) {
  if (!dades?.length) return null;
  const H = dades.length * 26 + 20;
  const W = 580, pL = 160, rowH = (H - 10) / dades.length;
  const maxV = Math.max(...dades.map(d => d.grogues + d.vermelles), 1);
  return (
    <div>
      <svg viewBox={`0 0 ${W} ${H}`} className="w-full">
        {dades.map((d, i) => {
          const y = 8 + i * rowH, bh = Math.min(rowH - 4, 14);
          const wG = (d.grogues / maxV) * 360, wV = (d.vermelles / maxV) * 360;
          return (
            <g key={d.team}>
              <text x={pL - 5} y={y + bh / 2 + 5} textAnchor="end" fontSize={9} fill="#475569">{(d.team || "").substring(0, 22)}</text>
              <rect x={pL} y={y + 2} width={wG} height={bh} fill="#f39c12" rx={2} />
              <rect x={pL + wG} y={y + 2} width={wV} height={bh} fill="#e74c3c" rx={2} />
              <text x={pL + wG + wV + 4} y={y + bh / 2 + 5} fontSize={8} fill="#64748b">{d.grogues}{d.vermelles > 0 ? `+${d.vermelles}🟥` : ""}</text>
            </g>
          );
        })}
      </svg>
      <div className="flex gap-4 mt-1 text-xs">
        <span className="flex items-center gap-1"><span className="w-3 h-2 bg-yellow-500 rounded inline-block"/>Grogues</span>
        <span className="flex items-center gap-1"><span className="w-3 h-2 bg-red-500 rounded inline-block"/>Vermelles</span>
      </div>
    </div>
  );
}

function BarresH({ dades, keyX, keyY, colors, height, etiqueta = "" }) {
  if (!dades?.length) return null;
  const H = height || dades.length * 26 + 20;
  const W = 580, pL = 160, pT = 8;
  const rowH = (H - pT) / dades.length;
  const maxV = Math.max(...dades.map(d => d[keyX] || 0), 0.01);
  const bW = (v) => (v / maxV) * (W - pL - 60);
  return (
    <svg viewBox={`0 0 ${W} ${H}`} className="w-full">
      {dades.map((d, i) => {
        const y = pT + i * rowH, bh = Math.min(rowH - 4, 16);
        const c = Array.isArray(colors) ? colors[i] : colors;
        const w = bW(d[keyX] || 0);
        return (
          <g key={i}>
            <text x={pL - 5} y={y + bh / 2 + 5} textAnchor="end" fontSize={9} fill="#475569">{(d[keyY] || "").substring(0, 22)}</text>
            <rect x={pL} y={y + 2} width={w} height={bh} fill={c} rx={2} opacity={0.85} />
            <text x={pL + w + 4} y={y + bh / 2 + 5} fontSize={9} fill="#64748b">{d[keyX]}</text>
          </g>
        );
      })}
    </svg>
  );
}

function ScatterPlot({ dades, keyX, keyY, keyLabel, xLabel, yLabel, height = 340, color, regLine = null }) {
  const [hover, setHover] = useState(null);
  if (!dades?.length) return null;
  const W = 560, pL = 45, pR = 20, pT = 15, pB = 38;
  const gW = W - pL - pR, gH = height - pT - pB;
  const xs = dades.map(d => d[keyX]), ys = dades.map(d => d[keyY]);
  const mnX = Math.min(...xs), mxX = Math.max(...xs);
  const mnY = Math.min(...ys, ...(regLine ? [regLine[0].y, regLine[1].y] : []));
  const mxY = Math.max(...ys, ...(regLine ? [regLine[0].y, regLine[1].y] : []), 1);
  const xS = (v) => pL + ((v - mnX) / (mxX - mnX || 1)) * gW;
  const yS = (v) => pT + gH - ((v - mnY) / (mxY - mnY || 1)) * gH;
  return (
    <svg viewBox={`0 0 ${W} ${height}`} className="w-full" style={{ overflow: "visible" }}>
      {[mnY, (mnY + mxY) / 2, mxY].map(v => (
        <g key={v}><line x1={pL} x2={W - pR} y1={yS(v)} y2={yS(v)} stroke="#f1f5f9" strokeWidth={1} />
          <text x={pL - 4} y={yS(v) + 4} textAnchor="end" fontSize={9} fill="#94a3b8">{Math.round(v)}</text></g>
      ))}
      {[mnX, (mnX + mxX) / 2, mxX].map(v => (
        <text key={v} x={xS(v)} y={height - 5} textAnchor="middle" fontSize={9} fill="#94a3b8">{Math.round(v)}</text>
      ))}
      <text x={W / 2} y={height - 1} textAnchor="middle" fontSize={9} fill="#64748b">{xLabel}</text>
      <text x={8} y={height / 2} textAnchor="middle" fontSize={9} fill="#64748b" transform={`rotate(-90,8,${height / 2})`}>{yLabel}</text>
      {regLine?.length === 2 && (
        <line x1={xS(regLine[0].x)} y1={yS(regLine[0].y)} x2={xS(regLine[1].x)} y2={yS(regLine[1].y)}
          stroke="#e74c3c" strokeWidth={1.5} strokeDasharray="5,3" />
      )}
      {dades.map((d, i) => (
        <circle key={i} cx={xS(d[keyX])} cy={yS(d[keyY])} r={hover === i ? 8 : 6}
          fill={typeof color === "function" ? color(d, i) : color} fillOpacity={0.82}
          stroke={hover === i ? "#0f172a" : "white"} strokeWidth={1.5} style={{ cursor: "pointer" }}
          onMouseEnter={() => setHover(i)} onMouseLeave={() => setHover(null)} />
      ))}
      {hover != null && (() => {
        const d = dades[hover];
        const bx = Math.min(xS(d[keyX]) - 5, W - 185), by = Math.max(yS(d[keyY]) - 62, 5);
        return (
          <g>
            <rect x={bx} y={by} width={182} height={56} rx={6} fill="white" stroke="#e2e8f0" strokeWidth={1} />
            <text x={bx + 8} y={by + 16} fontSize={10} fontWeight="700" fill="#0f172a">{(d[keyLabel] || "").substring(0, 24)}</text>
            <text x={bx + 8} y={by + 30} fontSize={9} fill="#64748b">{xLabel}: {d[keyX]}</text>
            <text x={bx + 8} y={by + 44} fontSize={9} fill="#64748b">{yLabel}: {d[keyY]}</text>
          </g>
        );
      })()}
    </svg>
  );
}

function ScatterEquips({ dades, tiltData, height = 360 }) {
  const [hover, setHover] = useState(null);
  if (!dades?.length) return null;
  const tiltMap = Object.fromEntries((tiltData || []).map(t => [t.team, t.tilt]));
  const enrich = dades.map(d => ({ ...d, tilt: tiltMap[d.team] ?? 0 }));
  const W = 560, pL = 50, pR = 20, pT = 20, pB = 40;
  const gW = W - pL - pR, gH = height - pT - pB;
  const xs = enrich.map(d => d.tilt), ys = enrich.map(d => d.rating);
  const mnX = Math.min(...xs) - 0.18, mxX = Math.max(...xs) + 0.18;
  const mnY = Math.min(...ys) - 5, mxY = Math.max(...ys) + 5;
  const xS = (v) => pL + ((v - mnX) / (mxX - mnX || 1)) * gW;
  const yS = (v) => pT + gH - ((v - mnY) / (mxY - mnY || 1)) * gH;
  const colorQ = (d) => d.rating >= 50 && d.tilt >= 0 ? "#27ae60" : d.rating >= 50 ? "#e67e22" : d.tilt >= 0 ? "#3498db" : "#e74c3c";
  return (
    <svg viewBox={`0 0 ${W} ${height}`} className="w-full" style={{ overflow: "visible" }}>
      <line x1={xS(0)} x2={xS(0)} y1={pT} y2={pT + gH} stroke="#ccc" strokeWidth={1} strokeDasharray="4,3" />
      <line x1={pL} x2={W - pR} y1={yS(50)} y2={yS(50)} stroke="#ccc" strokeWidth={1} strokeDasharray="4,3" />
      {[["Favorits en forma", mxX, mxY, "end", "#27ae60"],
        ["Favorits en crisi", mnX, mxY, "start", "#e67e22"],
        ["Underdog en forma", mxX, mnY, "end", "#3498db"],
        ["Cua en crisi", mnX, mnY, "start", "#e74c3c"]].map(([txt, x, y, a, c]) => (
        <text key={txt} x={xS(x)} y={yS(y)} textAnchor={a} fontSize={9} fill={c} fontWeight="600" opacity={0.7}>{txt}</text>
      ))}
      {[-0.5, 0, 0.5].map(v => <text key={v} x={xS(v)} y={height - 5} textAnchor="middle" fontSize={9} fill="#94a3b8">{v > 0 ? `+${v}` : v}</text>)}
      {[0, 25, 50, 75, 100].map(v => <text key={v} x={pL - 4} y={yS(v) + 4} textAnchor="end" fontSize={9} fill="#94a3b8">{v}</text>)}
      <text x={W / 2} y={height - 1} textAnchor="middle" fontSize={9} fill="#64748b">Tilt (momentum recent)</text>
      <text x={10} y={height / 2} textAnchor="middle" fontSize={9} fill="#64748b" transform={`rotate(-90,10,${height / 2})`}>Rating</text>
      {enrich.map((d, i) => (
        <g key={i}>
          <circle cx={xS(d.tilt)} cy={yS(d.rating)} r={hover === i ? 10 : 8}
            fill={colorQ(d)} fillOpacity={0.85} stroke="white" strokeWidth={1.5} style={{ cursor: "pointer" }}
            onMouseEnter={() => setHover(i)} onMouseLeave={() => setHover(null)} />
          <text x={xS(d.tilt)} y={yS(d.rating) - 12} textAnchor="middle" fontSize={8} fill="#333" style={{ pointerEvents: "none" }}>
            {d.team.split(",")[0].substring(0, 12)}
          </text>
        </g>
      ))}
      {hover != null && (() => {
        const d = enrich[hover];
        const bx = Math.min(xS(d.tilt), W - 185), by = Math.max(yS(d.rating) - 65, 5);
        return (
          <g>
            <rect x={bx} y={by} width={180} height={58} rx={6} fill="white" stroke="#e2e8f0" strokeWidth={1} />
            <text x={bx + 8} y={by + 15} fontSize={10} fontWeight="700" fill="#0f172a">{d.team.substring(0, 24)}</text>
            <text x={bx + 8} y={by + 29} fontSize={9} fill="#64748b">Rating: {d.rating}</text>
            <text x={bx + 8} y={by + 43} fontSize={9} fill="#64748b">Tilt: {d.tilt > 0 ? "+" : ""}{d.tilt}</text>
          </g>
        );
      })()}
    </svg>
  );
}

function ScatterLatents({ dades, height = 360 }) {
  const [hover, setHover] = useState(null);
  if (!dades?.length) return null;
  const W = 560, pL = 55, pR = 20, pT = 20, pB = 40;
  const gW = W - pL - pR, gH = height - pT - pB;
  const xs = dades.map(d => d.attack), ys = dades.map(d => d.defense);
  const pad = 0.08;
  const mnX = Math.min(...xs) - pad, mxX = Math.max(...xs) + pad;
  const mnY = Math.min(...ys) - pad, mxY = Math.max(...ys) + pad;
  const xS = (v) => pL + ((v - mnX) / (mxX - mnX || 1)) * gW;
  const yS = (v) => pT + gH - ((v - mnY) / (mxY - mnY || 1)) * gH;
  return (
    <svg viewBox={`0 0 ${W} ${height}`} className="w-full" style={{ overflow: "visible" }}>
      <line x1={xS(0)} x2={xS(0)} y1={pT} y2={pT + gH} stroke="#ccc" strokeWidth={1} strokeDasharray="4,3" />
      <line x1={pL} x2={W - pR} y1={yS(0)} y2={yS(0)} stroke="#ccc" strokeWidth={1} strokeDasharray="4,3" />
      {[["Millor de tot", mxX, mxY, "end", "#27ae60"],
        ["Bon atac / mala def.", mxX, mnY, "end", "#e67e22"],
        ["Mal atac / bona def.", mnX, mxY, "start", "#3498db"],
        ["Pitjor de tot", mnX, mnY, "start", "#e74c3c"]].map(([txt, x, y, a, c]) => (
        <text key={txt} x={xS(x)} y={yS(y)} textAnchor={a} fontSize={9} fill={c} fontWeight="600" opacity={0.7}>{txt}</text>
      ))}
      <text x={W / 2} y={height - 1} textAnchor="middle" fontSize={9} fill="#64748b">Força d'Atac (+millor)</text>
      <text x={10} y={height / 2} textAnchor="middle" fontSize={9} fill="#64748b" transform={`rotate(-90,10,${height / 2})`}>Força de Defensa (+millor)</text>
      {dades.map((d, i) => (
        <g key={i}>
          <circle cx={xS(d.attack)} cy={yS(d.defense)} r={hover === i ? 10 : 8}
            fill={d.color} fillOpacity={0.85} stroke="white" strokeWidth={1.5} style={{ cursor: "pointer" }}
            onMouseEnter={() => setHover(i)} onMouseLeave={() => setHover(null)} />
          <text x={xS(d.attack)} y={yS(d.defense) - 12} textAnchor="middle" fontSize={8} fill="#333" style={{ pointerEvents: "none" }}>
            {d.team.split(",")[0].substring(0, 12)}
          </text>
        </g>
      ))}
      {hover != null && (() => {
        const d = dades[hover];
        const bx = Math.min(xS(d.attack), W - 185), by = Math.max(yS(d.defense) - 70, 5);
        return (
          <g>
            <rect x={bx} y={by} width={180} height={64} rx={6} fill="white" stroke="#e2e8f0" strokeWidth={1} />
            <text x={bx + 8} y={by + 15} fontSize={10} fontWeight="700" fill="#0f172a">{d.team.substring(0, 24)}</text>
            <text x={bx + 8} y={by + 29} fontSize={9} fill="#64748b">Atac: {d.attack > 0 ? "+" : ""}{d.attack}</text>
            <text x={bx + 8} y={by + 43} fontSize={9} fill="#64748b">Defensa: {d.defense > 0 ? "+" : ""}{d.defense}</text>
            <text x={bx + 8} y={by + 57} fontSize={9} fill="#64748b">{d.n_matches} partits jugats</text>
          </g>
        );
      })()}
    </svg>
  );
}

function GraficLiniesMulti({ dades, height = 300 }) {
  if (!dades?.length) return null;
  const COLORS = ["#1a6b8a","#27ae60","#e74c3c","#f39c12","#8e44ad","#2980b9","#e67e22","#16a085","#c0392b","#2c3e50"];
  const W = 560, pL = 35, pR = 20, pT = 15, pB = 25;
  const gW = W - pL - pR, gH = height - pT - pB;

  // Jugadors i jornades únics, ben ordenats numèricament
  const players = [...new Set(dades.map(d => d.player))];
  const jornades = [...new Set(dades.map(d => Number(d.jornada)))].sort((a, b) => a - b);
  const jMin = jornades[0], jMax = jornades[jornades.length - 1];
  const maxV = Math.max(...dades.map(d => d.gols_acum), 1);
  const xS = (j) => pL + ((Number(j) - jMin) / (jMax - jMin || 1)) * gW;
  const yS = (v) => pT + gH - (v / maxV) * gH;

  // Agrupar per jugador i ordenar per jornada
  const byPlayer = {};
  players.forEach(p => {
    byPlayer[p] = dades
      .filter(d => d.player === p)
      .sort((a, b) => Number(a.jornada) - Number(b.jornada));
  });

  // Nom curt per mostrar: cognom principal (sense comes ni truncament agressiu)
  const nomCurt = (p) => {
    const parts = p.split(",");
    // Format "COGNOM, NOM" → mostrar "COGNOM"
    const cognom = (parts[0] || p).trim();
    return cognom.length > 16 ? cognom.substring(0, 15) + "…" : cognom;
  };

  return (
    <div>
      <svg viewBox={`0 0 ${W} ${height}`} className="w-full">
        {/* Graella */}
        {[0, Math.round(maxV / 3), Math.round(maxV * 2 / 3), maxV].map(v => (
          <g key={v}>
            <line x1={pL} x2={W - pR} y1={yS(v)} y2={yS(v)} stroke="#f1f5f9" strokeWidth={1} />
            <text x={pL - 4} y={yS(v) + 4} textAnchor="end" fontSize={9} fill="#94a3b8">{v}</text>
          </g>
        ))}
        {/* Eix X: jornades */}
        {jornades.filter((_, i) => i === 0 || i === jornades.length - 1 || i % Math.ceil(jornades.length / 8) === 0).map(j => (
          <text key={j} x={xS(j)} y={height - 4} textAnchor="middle" fontSize={9} fill="#94a3b8">J{j}</text>
        ))}
        {/* Línies per jugador */}
        {players.map((p, i) => {
          const pts = byPlayer[p];
          if (!pts || pts.length < 1) return null;
          const color = COLORS[i % COLORS.length];
          const path = pts.map((d, j) =>
            `${j === 0 ? "M" : "L"} ${xS(d.jornada)} ${yS(d.gols_acum)}`
          ).join(" ");
          // Punt final (valor màxim) per la etiqueta inline
          const last = pts[pts.length - 1];
          return (
            <g key={p}>
              <path d={path} fill="none" stroke={color} strokeWidth={2} strokeLinejoin="round" />
              {/* Cercle petit a cada punt */}
              {pts.filter(d => d.gols_acum > (pts[pts.indexOf(d) - 1]?.gols_acum ?? -1)).map((d, ci) => (
                <circle key={ci} cx={xS(d.jornada)} cy={yS(d.gols_acum)} r={3} fill={color} />
              ))}
              {/* Valor final a la dreta de la línia */}
              <circle cx={xS(last.jornada)} cy={yS(last.gols_acum)} r={4} fill={color} />
              <text x={xS(last.jornada) + 6} y={yS(last.gols_acum) + 4}
                fontSize={8} fill={color} fontWeight="700">{last.gols_acum}</text>
            </g>
          );
        })}
      </svg>
      {/* Llegenda sota el gràfic — noms complets, en 2 columnes */}
      <div className="grid grid-cols-2 gap-x-4 gap-y-1 mt-3 px-1">
        {players.map((p, i) => {
          const last = byPlayer[p]?.[byPlayer[p].length - 1];
          return (
            <div key={p} className="flex items-center gap-1.5 text-xs">
              <span className="w-3 h-3 rounded-full shrink-0 inline-block"
                style={{ background: COLORS[i % COLORS.length] }} />
              <span className="text-slate-700 truncate" title={p}>{nomCurt(p)}</span>
              <span className="text-slate-400 shrink-0 ml-auto">{last?.gols_acum ?? 0}⚽</span>
            </div>
          );
        })}
      </div>
    </div>
  );
}

const Card = ({ title, subtitle, children }) => (
  <div className="bg-white rounded-2xl border border-slate-200 p-4">
    <p className="text-sm font-semibold text-slate-700 mb-1">{title}</p>
    {subtitle && <p className="text-xs text-slate-400 mb-3">{subtitle}</p>}
    {!subtitle && <div className="mb-3" />}
    {children}
  </div>
);

export default function EstadistiquesPage() {
  const { categoria, grup } = useGrup();
  const [tab, setTab]       = useState("lliga");
  const [data, setData]     = useState(null);
  const [loading, setLoading] = useState(true);
  const [error, setError]   = useState(null);

  useEffect(() => {
    setLoading(true); setError(null); setData(null);
    fetch(`${API}/estadistiques/${categoria}/${grup}`)
      .then(r => { if (!r.ok) throw new Error(`HTTP ${r.status}`); return r.json(); })
      .then(d => { setData(d); setLoading(false); })
      .catch(e => { setError(e.message); setLoading(false); });
  }, [categoria, grup]);

  if (loading) return (
    <div>
      <h1 className="text-2xl font-bold text-slate-900 mb-6">📊 Estadístiques</h1>
      <div className="flex items-center justify-center py-24 text-slate-400">
        <div className="animate-spin text-2xl mr-3">⟳</div>Carregant...
      </div>
    </div>
  );
  if (error) return <div className="bg-red-50 border border-red-200 rounded-xl p-4 text-red-600 text-sm mt-6">Error: {error}</div>;
  if (!data) return null;

  const k = data.kpis;

  // Gradient colors per ranking
  const gradVerd = (n) => Array.from({length: n}, (_, i) => `hsl(${140 - i*6},60%,${48 + i*2}%)`);
  const gradBlau = (n) => Array.from({length: n}, (_, i) => `hsl(${210 - i*4},65%,${58 - i*2}%)`);
  const gradSemafor = (n) => Array.from({length: n}, (_, i) => {
    const t = i / Math.max(n - 1, 1);
    return `hsl(${120 - t * 120},70%,50%)`;
  });

  return (
    <div>
      <h1 className="text-2xl font-bold text-slate-900 mb-5">📊 Estadístiques de la Competició</h1>

      {/* KPIs */}
      <div className="grid grid-cols-2 md:grid-cols-4 gap-3 mb-5">
        <KpiBox icon="⚽" label="Gols Totals"           value={k.total_gols}     color="text-green-600" />
        <KpiBox icon="📊" label="Gols per Partit"       value={k.avg_gols}       color="text-blue-600" />
        <KpiBox icon="🌟" label="Màx. Gols en 1 Partit" value={k.max_gols}       color="text-orange-500" />
        <KpiBox icon="🟨" label="Targetes Totals"       value={k.total_targetes} color="text-yellow-600" />
      </div>

      {/* Tabs */}
      <div className="flex gap-1 bg-slate-100 p-1 rounded-xl mb-5 w-fit">
        {[{id:"lliga",label:"🏟️ Lliga"},{id:"equips",label:"🛡️ Equips"},{id:"jugadors",label:"👤 Jugadors"}].map(t => (
          <button key={t.id} onClick={() => setTab(t.id)}
            className={`px-4 py-2 rounded-lg text-sm font-medium transition-all ${tab === t.id ? "bg-white text-slate-800 shadow-sm" : "text-slate-500 hover:text-slate-700"}`}>
            {t.label}
          </button>
        ))}
      </div>

      {/* ─── TAB LLIGA ─── */}
      {tab === "lliga" && (
        <div className="space-y-4">
          <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
            <Card title="⏱ Distribució de Gols per Minut" subtitle="La línia vermella marca el minut 45 (descans)">
              <HistogramGols dades={data.gols_minut} />
            </Card>
            <Card title="⚽ Gols per Jornada" subtitle={`Línia discontínua: mitjana ${data.avg_gols_jornada} gols/jornada`}>
              <BarresV dades={data.gols_jornada} keys={["gols"]} colors={["#3498db"]} labels={["Gols"]} showAvg={data.avg_gols_jornada} />
            </Card>
          </div>
          <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
            <Card title="🟨 Targetes per Jornada">
              <BarresV dades={data.targetes_jornada} keys={["grogues","vermelles"]} colors={["#f39c12","#e74c3c"]} labels={["Grogues","Vermelles"]} />
            </Card>
            <Card title="📋 Resultats més Comuns (top 12)">
              <BarresV
                dades={data.resultats_comuns.map(r => ({ ...r, label: r.resultat }))}
                keys={["freq"]}
                colors={data.resultats_comuns.map(r => r.color)}
                labels={[]}
              />
            </Card>
          </div>
        </div>
      )}

      {/* ─── TAB EQUIPS ─── */}
      {tab === "equips" && (
        <div className="space-y-4">
          <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
            <Card title="🥅 Gols a Favor vs en Contra per Equip">
              <EquipBiBarres dades={data.gols_equip} key1="GF" key2="GC" col1="#27ae60" col2="#e74c3c" l1="GF" l2="GC"
                height={data.gols_equip.length * 28 + 20} />
            </Card>
            <Card title="🏠 Punts promig Casa vs Fora per Equip">
              <EquipBiBarres dades={data.casa_fora} key1="casa" key2="fora" col1="#27ae60" col2="#3498db" l1="Casa" l2="Fora"
                height={data.casa_fora.length * 28 + 20} />
            </Card>
          </div>
          <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
            <Card title="🟨 Targetes per Equip">
              <TargetesEquip dades={data.targetes_equip} />
            </Card>
            <Card title="🎯 Targetes vs Punts" subtitle="Cada punt = un equip. Grogues + 3×Vermelles">
              <ScatterPlot dades={data.targetes_punts} keyX="targetes" keyY="punts" keyLabel="team"
                xLabel="Targetes (ponderació)" yLabel="Punts"
                color={(d, i) => { const t = i / Math.max(data.targetes_punts.length - 1, 1); const r = Math.round(39 + t * 192); const g = Math.round(174 - t * 98); return `rgb(${r},${g},96)`; }} />
            </Card>
          </div>
          <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
            <Card title="🥇 Ranking Millors Atacs (gols marcats)">
              <BarresH dades={data.ranking_atac.map(d => ({ ...d, team: d.team }))} keyX="avg_gf" keyY="team"
                colors={gradVerd(data.ranking_atac.length)} height={data.ranking_atac.length * 26 + 20} />
            </Card>
            <Card title="🛡️ Ranking Millors Defenses (gols encaixats)">
              <BarresH dades={[...data.ranking_defensa].reverse().map(d => ({ ...d, avg_gc: d.avg_gc }))}
                keyX="avg_gc" keyY="team"
                colors={gradBlau(data.ranking_defensa.length)} height={data.ranking_defensa.length * 26 + 20} />
            </Card>
          </div>
          <Card title="⚡ Rating vs Tilt — Posicionament d'Equips" subtitle="Eix X: Tilt (momentum recent) · Eix Y: Rating global de l'equip">
            <ScatterEquips dades={data.latents} tiltData={data.tilt_data} />
          </Card>
          <Card title="⚔️ Latents d'Atac i Defensa" subtitle="Model Poisson. Dreta = millor atac · Dalt = millor defensa">
            <ScatterLatents dades={data.latents} />
          </Card>
        </div>
      )}

      {/* ─── TAB JUGADORS ─── */}
      {tab === "jugadors" && (
        <div className="space-y-4">
          <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
            <Card title="⚽ Evolució Top 10 Golejadors">
              <GraficLiniesMulti dades={data.top10_evolucio} />
            </Card>
            <Card title="🟨 Top 10 Jugadors amb més Targetes">
              <svg viewBox={`0 0 580 ${data.top_targetes.length * 28 + 20}`} className="w-full">
                {data.top_targetes.map((d, i) => {
                  const maxV = Math.max(...data.top_targetes.map(x => x.total), 1);
                  const rowH = 28, y = 8 + i * rowH, bh = 14;
                  const wG = (d.grogues / maxV) * 360, wV = (d.vermelles / maxV) * 360;
                  return (
                    <g key={d.player}>
                      <text x={155} y={y + bh / 2 + 5} textAnchor="end" fontSize={9} fill="#475569">{d.player.split(",")[0].substring(0, 20)}</text>
                      <rect x={160} y={y + 2} width={wG} height={bh} fill="#f39c12" rx={2} />
                      <rect x={160 + wG} y={y + 2} width={wV} height={bh} fill="#e74c3c" rx={2} />
                      <text x={163 + wG + wV} y={y + bh / 2 + 5} fontSize={8} fill="#64748b">{d.grogues}🟨{d.vermelles > 0 ? ` ${d.vermelles}🟥` : ""}</text>
                    </g>
                  );
                })}
              </svg>
            </Card>
          </div>
          <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
            <Card title="⚽ Gols vs Minuts Jugats (mín. 2 gols)" subtitle="Cada punt = un jugador. La línia mostra la tendència esperada.">
              <ScatterPlot dades={data.scatter_gols} keyX="total_minutes" keyY="goals" keyLabel="player"
                xLabel="Minuts jugats" yLabel="Gols" regLine={data.reg_line}
                color={(d, i) => { const n = data.scatter_gols.length; const t = i / Math.max(n - 1, 1); return `hsl(${200 + t * 20},70%,${55 - t * 15}%)`; }} />
            </Card>
            <Card title="🏃 Minuts per Gol — Eficiència Golejadora" subtitle="Jugadors amb mín. 2 gols, ordenats per minuts necessaris per marcar">
              <svg viewBox={`0 0 580 ${data.eficiencia.length * 28 + 20}`} className="w-full">
                {data.eficiencia.map((d, i) => {
                  const maxV = Math.max(...data.eficiencia.map(x => x.min_per_gol), 1);
                  const n = data.eficiencia.length;
                  const t = i / Math.max(n - 1, 1);
                  const col = `hsl(${120 - t * 120},70%,50%)`;
                  const rowH = 28, y = 8 + i * rowH, bh = 14;
                  const w = (d.min_per_gol / maxV) * 310;
                  return (
                    <g key={d.player}>
                      <text x={155} y={y + bh / 2 + 5} textAnchor="end" fontSize={9} fill="#475569">{d.player.split(",")[0].substring(0, 20)}</text>
                      <rect x={160} y={y + 2} width={w} height={bh} fill={col} rx={2} opacity={0.85} />
                      <text x={163 + w} y={y + bh / 2 + 5} fontSize={8} fill="#64748b">{d.min_per_gol}' / gol ({d.goals} gols)</text>
                    </g>
                  );
                })}
              </svg>
            </Card>
          </div>
        </div>
      )}
    </div>
  );
}