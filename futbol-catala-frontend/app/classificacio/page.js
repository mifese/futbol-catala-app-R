"use client";
import { useEffect, useState, useRef } from "react";
import { useGrup } from "../components/GrupContext";

const API = process.env.NEXT_PUBLIC_API_URL || "http://localhost:8000";

const posCls = (pos, total) => {
  if (pos === 1)        return "bg-yellow-100 text-yellow-800 font-bold";
  if (pos <= 3)         return "bg-green-100 text-green-700 font-semibold";
  if (pos >= total - 1) return "bg-red-100 text-red-600";
  return "text-slate-600";
};

// ── Taula de classificació reutilitzable ──
function TaulaClassificacio({ dades, titol }) {
  const total = dades.length;
  return (
    <div className="bg-white rounded-2xl border border-slate-200 overflow-hidden shadow-sm">
      {titol && <div className="px-4 py-3 border-b border-slate-100 font-semibold text-slate-700 text-sm">{titol}</div>}
      <div className="overflow-x-auto">
        <table className="w-full text-sm">
          <thead>
            <tr className="bg-slate-50 border-b border-slate-200 text-xs font-semibold text-slate-500 uppercase tracking-wide">
              <th className="px-4 py-3 text-left w-10">#</th>
              <th className="px-4 py-3 text-left">Equip</th>
              <th className="px-3 py-3 text-center">PJ</th>
              <th className="px-3 py-3 text-center">G</th>
              <th className="px-3 py-3 text-center">E</th>
              <th className="px-3 py-3 text-center">P</th>
              <th className="px-3 py-3 text-center">GF</th>
              <th className="px-3 py-3 text-center">GC</th>
              <th className="px-3 py-3 text-center">DG</th>
              <th className="px-3 py-3 text-center">Pts</th>
            </tr>
          </thead>
          <tbody>
            {dades.map(eq => (
              <tr key={eq.team} className="border-b border-slate-100 hover:bg-slate-50 transition-colors">
                <td className="px-4 py-3">
                  <span className={`inline-flex items-center justify-center w-6 h-6 rounded-full text-xs ${posCls(eq.position, total)}`}>
                    {eq.position}
                  </span>
                </td>
                <td className="px-4 py-3 font-medium text-slate-800">{eq.team}</td>
                <td className="px-3 py-3 text-center text-slate-600">{eq.played}</td>
                <td className="px-3 py-3 text-center text-slate-600">{eq.wins}</td>
                <td className="px-3 py-3 text-center text-slate-600">{eq.draws}</td>
                <td className="px-3 py-3 text-center text-slate-600">{eq.losses}</td>
                <td className="px-3 py-3 text-center text-slate-600">{eq.goals_for}</td>
                <td className="px-3 py-3 text-center text-slate-600">{eq.goals_against}</td>
                <td className={`px-3 py-3 text-center font-medium ${eq.goal_diff > 0 ? "text-green-600" : eq.goal_diff < 0 ? "text-red-500" : "text-slate-500"}`}>
                  {eq.goal_diff > 0 ? `+${eq.goal_diff}` : eq.goal_diff}
                </td>
                <td className="px-3 py-3 text-center font-bold text-slate-900">{eq.points}</td>
              </tr>
            ))}
          </tbody>
        </table>
      </div>
      <div className="px-4 py-3 border-t border-slate-100 flex flex-wrap gap-4 text-xs text-slate-500">
        <span className="flex items-center gap-1.5"><span className="w-3 h-3 rounded-full bg-yellow-200 inline-block"></span>1r</span>
        <span className="flex items-center gap-1.5"><span className="w-3 h-3 rounded-full bg-green-200 inline-block"></span>Zona ascens</span>
        <span className="flex items-center gap-1.5"><span className="w-3 h-3 rounded-full bg-red-200 inline-block"></span>Zona descens</span>
      </div>
    </div>
  );
}

// ── Gràfic evolució de posicions (SVG pur) ──
function GraficEvolucio({ dades }) {
  const equips = Object.keys(dades);
  if (!equips.length) return null;
  const jornades = [...new Set(Object.values(dades).flatMap(e => e.map(x => x.jornada)))].sort((a, b) => a - b);
  const nEquips = equips.length;
  const W = 900, H = 500, padL = 160, padR = 20, padT = 30, padB = 30;
  const gW = W - padL - padR, gH = H - padT - padB;
  const xScale = (j) => padL + (jornades.indexOf(j) / (jornades.length - 1)) * gW;
  const yScale = (pos) => padT + ((pos - 1) / (nEquips - 1)) * gH;
  const COLORS = ["#16a34a","#2563eb","#dc2626","#d97706","#7c3aed","#0891b2","#be185d","#65a30d","#ea580c","#0284c7","#9333ea","#b45309","#4f46e5","#15803d","#b91c1c","#0369a1","#7e22ce","#a16207"];

  return (
    <div className="bg-white rounded-2xl border border-slate-200 p-4 overflow-x-auto">
      <h3 className="font-semibold text-slate-800 mb-3 text-sm">📈 Evolució de Posicions</h3>
      <svg viewBox={`0 0 ${W} ${H}`} className="w-full" style={{ minWidth: 600 }}>
        {/* Línies de fons */}
        {equips.map((_, i) => (
          <line key={i} x1={padL} x2={W - padR} y1={yScale(i + 1)} y2={yScale(i + 1)}
            stroke="#f1f5f9" strokeWidth={1} />
        ))}
        {/* Etiquetes eix X */}
        {jornades.map(j => (
          <text key={j} x={xScale(j)} y={H - 5} textAnchor="middle" fontSize={10} fill="#94a3b8">J{j}</text>
        ))}
        {/* Etiquetes eix Y */}
        {equips.map((_, i) => (
          <text key={i} x={padL - 6} y={yScale(i + 1) + 4} textAnchor="end" fontSize={9} fill="#64748b">{i + 1}</text>
        ))}
        {/* Línies de cada equip */}
        {equips.map((equip, idx) => {
          const punts = dades[equip];
          const color = COLORS[idx % COLORS.length];
          const path = punts.map((p, i) => `${i === 0 ? "M" : "L"} ${xScale(p.jornada)} ${yScale(p.position)}`).join(" ");
          const ultima = punts[punts.length - 1];
          return (
            <g key={equip}>
              <path d={path} fill="none" stroke={color} strokeWidth={2} strokeLinejoin="round" opacity={0.85} />
              {ultima && (
                <text x={xScale(ultima.jornada) + 5} y={yScale(ultima.position) + 4}
                  fontSize={9} fill={color} fontWeight="600">
                  {equip.split(",")[0].substring(0, 18)}
                </text>
              )}
            </g>
          );
        })}
      </svg>
    </div>
  );
}

// ── Heatmap Head2Head ──
function HeatmapH2H({ equips, matriu }) {
  if (!equips.length) return null;
  const getColor = (key) => {
    const r = matriu[key];
    if (!r) return "#f8fafc";
    if (r.result === "W") return "#bbf7d0";
    if (r.result === "L") return "#fecaca";
    return "#fef9c3";
  };
  const cellSize = Math.min(48, Math.floor(820 / (equips.length + 1)));

  return (
    <div className="bg-white rounded-2xl border border-slate-200 p-4 overflow-x-auto">
      <h3 className="font-semibold text-slate-800 mb-1 text-sm">⚔️ Matriu de Confrontacions Directes</h3>
      <p className="text-xs text-slate-400 mb-3">Files = local · Columnes = visitant · 🟩 Victòria · 🟥 Derrota · 🟨 Empat</p>
      <div style={{ overflowX: "auto" }}>
        <table className="border-collapse text-xs" style={{ minWidth: equips.length * cellSize + 160 }}>
          <thead>
            <tr>
              <th style={{ width: 160, padding: "4px 6px", textAlign: "right", fontSize: 10, color: "#94a3b8" }}>Local ↓ / Visitant →</th>
              {equips.map(e => (
                <th key={e} style={{ width: cellSize, padding: 2, textAlign: "center" }}>
                  <div style={{ transform: "rotate(-45deg)", transformOrigin: "center", fontSize: 9, color: "#64748b", whiteSpace: "nowrap", width: cellSize, overflow: "hidden", textOverflow: "ellipsis" }}>
                    {e.split(",")[0].substring(0, 12)}
                  </div>
                </th>
              ))}
            </tr>
          </thead>
          <tbody>
            {equips.map(home => (
              <tr key={home}>
                <td style={{ padding: "4px 6px", textAlign: "right", fontSize: 10, color: "#475569", maxWidth: 160, overflow: "hidden", textOverflow: "ellipsis", whiteSpace: "nowrap" }}>
                  {home.split(",")[0].substring(0, 20)}
                </td>
                {equips.map(away => {
                  if (home === away) return (
                    <td key={away} style={{ background: "#e2e8f0", textAlign: "center", fontSize: 10, border: "1px solid #fff", width: cellSize, height: cellSize }}>—</td>
                  );
                  const key = `${home}||${away}`;
                  const r = matriu[key];
                  return (
                    <td key={away} style={{ background: getColor(key), textAlign: "center", fontSize: 10, fontWeight: "600", border: "1px solid #fff", width: cellSize, height: cellSize, color: "#1e293b" }}>
                      {r ? r.label : ""}
                    </td>
                  );
                })}
              </tr>
            ))}
          </tbody>
        </table>
      </div>
    </div>
  );
}


// ── Taula de prediccions ──
function TaulaPrediccions({ dades }) {
  if (!dades || dades.length === 0) return (
    <div className="bg-white rounded-2xl border border-slate-200 p-8 text-center text-slate-400">
      <div className="text-4xl mb-2">🔮</div>
      <p className="text-sm">Les prediccions es generen automàticament cada dilluns.</p>
      <p className="text-xs mt-1 text-slate-300">Si acabes de configurar el sistema, executa el scraper manualment per generar-les.</p>
    </div>
  );

  const BarraPct = ({ value, color }) => (
    <div className="flex items-center gap-2">
      <div className="flex-1 bg-slate-100 rounded-full h-1.5 overflow-hidden">
        <div className={`h-full rounded-full ${color}`} style={{ width: `${Math.round(value * 100)}%` }} />
      </div>
      <span className="text-xs font-semibold w-10 text-right">{(value * 100).toFixed(1)}%</span>
    </div>
  );

  return (
    <div className="space-y-4">
      <div className="bg-amber-50 border border-amber-200 rounded-xl px-4 py-3 text-xs text-amber-700">
        🔮 Basada en <strong>10.000 simulacions</strong> dels partits pendents. Model Poisson amb latents d'atac i defensa per equip.
      </div>
      <div className="bg-white rounded-2xl border border-slate-200 overflow-hidden shadow-sm">
        <div className="overflow-x-auto">
          <table className="w-full text-sm">
            <thead>
              <tr className="bg-slate-50 border-b border-slate-200 text-xs font-semibold text-slate-500 uppercase tracking-wide">
                <th className="px-4 py-3 text-left">Equip</th>
                <th className="px-3 py-3 text-center">Pts actuals</th>
                <th className="px-3 py-3 text-center">Pts esperats</th>
                <th className="px-3 py-3 text-center">Pos. esp.</th>
                <th className="px-4 py-3 text-left min-w-[120px]">🏆 Campió</th>
                <th className="px-4 py-3 text-left min-w-[120px]">⬆️ Top 3</th>
                <th className="px-4 py-3 text-left min-w-[120px]">⬇️ Descens</th>
              </tr>
            </thead>
            <tbody>
              {dades.map((eq, i) => (
                <tr key={eq.team} className="border-b border-slate-100 hover:bg-slate-50 transition-colors">
                  <td className="px-4 py-3 font-medium text-slate-800">{eq.team}</td>
                  <td className="px-3 py-3 text-center font-bold text-slate-900">{eq.punts_actuals}</td>
                  <td className="px-3 py-3 text-center text-green-600 font-semibold">{eq.punts_esperats}</td>
                  <td className="px-3 py-3 text-center">
                    <span className="inline-flex items-center justify-center w-8 h-8 rounded-full bg-slate-100 text-slate-700 text-xs font-bold">
                      {eq.pos_esperada?.toFixed(1)}
                    </span>
                  </td>
                  <td className="px-4 py-3"><BarraPct value={eq["prob_campió"] || 0} color="bg-yellow-400" /></td>
                  <td className="px-4 py-3"><BarraPct value={eq.prob_top3     || 0} color="bg-green-500"  /></td>
                  <td className="px-4 py-3"><BarraPct value={eq.prob_descens  || 0} color="bg-red-400"   /></td>
                </tr>
              ))}
            </tbody>
          </table>
        </div>
      </div>
    </div>
  );
}

// ── Component principal ──
const TABS = [
  { id: "total",      label: "📊 Total" },
  { id: "casa",       label: "🏠 Casa" },
  { id: "fora",       label: "✈️ Fora" },
  { id: "evolucio",   label: "📈 Evolució" },
  { id: "h2h",        label: "⚔️ H2H" },
  { id: "prediccions",label: "🔮 Predicció" },
];

export default function ClassificacioPage() {
  const { categoria, grup } = useGrup();
  const [tab,        setTab]        = useState("total");
  const [classfData, setClassfData] = useState(null);
  const [evolData,   setEvolData]   = useState(null);
  const [h2hData,    setH2hData]    = useState(null);
  const [predData,   setPredData]   = useState(null);
  const [carregant,  setCarregant]  = useState(true);
  const [error,      setError]      = useState(null);

  useEffect(() => {
    setCarregant(true); setError(null);
    setClassfData(null); setEvolData(null); setH2hData(null); setPredData(null);

    Promise.all([
      fetch(`${API}/classificacio/${categoria}/${grup}/casa-fora`).then(r => r.json()),
      fetch(`${API}/classificacio/${categoria}/${grup}/evolucio`).then(r => r.json()),
      fetch(`${API}/classificacio/${categoria}/${grup}/head2head`).then(r => r.json()),
      fetch(`${API}/classificacio/${categoria}/${grup}/prediccions`).then(r => r.json()).catch(() => ({prediccions:[]})),
    ])
      .then(([cf, ev, h2h, pred]) => {
        setClassfData(cf);
        setEvolData(ev);
        setH2hData(h2h);
        setPredData(pred);
        setCarregant(false);
      })
      .catch(e => { setError(e.message); setCarregant(false); });
  }, [categoria, grup]);

  const jornada_max = classfData?.total?.length
    ? Math.max(...(classfData.total.map(e => e.played)))
    : null;

  return (
    <div>
      <div className="mb-5">
        <h1 className="text-2xl font-bold text-slate-900">Classificació</h1>
        {classfData && <p className="text-sm text-slate-500 mt-1">{classfData.total.length} equips · {classfData.total[0]?.played ?? 0} jornades jugades</p>}
      </div>

      {/* Tabs */}
      <div className="flex gap-1 bg-slate-100 p-1 rounded-xl mb-5 w-fit flex-wrap">
        {TABS.map(t => (
          <button key={t.id} onClick={() => setTab(t.id)}
            className={`px-4 py-2 rounded-lg text-sm font-medium transition-all ${
              tab === t.id ? "bg-white text-slate-800 shadow-sm" : "text-slate-500 hover:text-slate-700"
            }`}>
            {t.label}
          </button>
        ))}
      </div>

      {carregant && <div className="flex items-center justify-center py-20 text-slate-400"><div className="animate-spin text-2xl mr-3">⟳</div>Carregant...</div>}
      {error     && <div className="bg-red-50 border border-red-200 rounded-xl p-4 text-red-600 text-sm">Error: {error}</div>}

      {!carregant && !error && (
        <div className="space-y-5">
          {tab === "total"    && classfData && <TaulaClassificacio dades={classfData.total} />}
          {tab === "casa"     && classfData && <TaulaClassificacio dades={classfData.casa}  titol="🏠 Classificació a Casa" />}
          {tab === "fora"     && classfData && <TaulaClassificacio dades={classfData.fora}  titol="✈️ Classificació a Fora" />}
          {tab === "evolucio" && evolData   && <GraficEvolucio dades={evolData.equips} />}
          {tab === "h2h"      && h2hData    && <HeatmapH2H equips={h2hData.equips} matriu={h2hData.matriu} />}
          {tab === "prediccions"             && <TaulaPrediccions dades={predData?.prediccions} />}
        </div>
      )}
    </div>
  );
}