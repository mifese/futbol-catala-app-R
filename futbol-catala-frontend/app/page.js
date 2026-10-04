"use client";
import { useEffect, useState } from "react";
import { useGrup } from "./components/GrupContext";

const API = process.env.NEXT_PUBLIC_API_URL || "http://localhost:8000";
const CAT_LABEL = { TERCERA: "Tercera Catalana", SEGONA: "Segona Catalana", PRIMERA: "Primera Catalana" };

const KpiCard = ({ icon, label, value, sub, color = "text-slate-800" }) => (
  <div className="bg-white rounded-2xl border border-slate-200 p-4 flex items-center gap-4">
    <div className="text-3xl shrink-0">{icon}</div>
    <div>
      <p className={`text-2xl font-bold ${color}`}>{value}</p>
      <p className="text-xs font-semibold text-slate-700">{label}</p>
      {sub && <p className="text-xs text-slate-400">{sub}</p>}
    </div>
  </div>
);

const posCls = (pos, total) => {
  if (pos === 1)        return "bg-yellow-100 text-yellow-800 font-bold";
  if (pos <= 3)         return "bg-green-100 text-green-700 font-semibold";
  if (pos >= total - 1) return "bg-red-100 text-red-600";
  return "text-slate-600";
};

export default function IniciPage() {
  const { categoria, grup } = useGrup();
  const [dades,     setDades]     = useState(null);
  const [carregant, setCarregant] = useState(true);
  const [error,     setError]     = useState(null);

  useEffect(() => {
    setCarregant(true); setError(null); setDades(null);
    fetch(`${API}/inici/${categoria}/${grup}`)
      .then(r => { if (!r.ok) throw new Error(`Error ${r.status}`); return r.json(); })
      .then(d => { setDades(d); setCarregant(false); })
      .catch(e => { setError(e.message); setCarregant(false); });
  }, [categoria, grup]);

  if (carregant) return <div className="flex items-center justify-center py-20 text-slate-400"><div className="animate-spin text-2xl mr-3">⟳</div>Carregant...</div>;
  if (error)     return <div className="bg-red-50 border border-red-200 rounded-xl p-4 text-red-600 text-sm">Error: {error}</div>;
  if (!dades)    return null;

  const { kpis, classificacio, golejadors, ultima_jornada } = dades;
  const total = classificacio.length;

  return (
    <div className="space-y-6">
      {/* Títol */}
      <div>
        <h1 className="text-2xl font-bold text-slate-900">Inici</h1>
        <p className="text-sm text-slate-500 mt-1">{CAT_LABEL[categoria]} · Grup {grup}</p>
      </div>

      {/* KPIs fila 1 */}
      <div className="grid grid-cols-2 sm:grid-cols-4 gap-3">
        <KpiCard icon="🛡️" label="Equips"    value={kpis.equips} />
        <KpiCard icon="👤" label="Jugadors"  value={kpis.jugadors} />
        <KpiCard icon="📅" label="Jornades"  value={kpis.jornades} />
        <KpiCard icon="⚽" label="Gols totals" value={kpis.total_gols} sub={`${kpis.avg_gols_partit} per partit`} color="text-green-600" />
      </div>

      {/* KPIs fila 2 */}
      <div className="grid grid-cols-2 sm:grid-cols-4 gap-3">
        <KpiCard icon="🏠" label="Gols locals"     value={kpis.gols_local}    sub="marcats a casa" />
        <KpiCard icon="✈️" label="Gols visitants"  value={kpis.gols_visitant} sub="marcats a fora" />
        <KpiCard icon="🏆" label="Victòries local" value={`${kpis.pct_vic_local}%`} sub="dels partits" color="text-green-600" />
        <KpiCard icon="🤝" label="Empats"          value={`${kpis.pct_empat}%`}     sub="dels partits" />
      </div>

      {/* 3 columnes: classificació + golejadors + última jornada */}
      <div className="grid grid-cols-1 lg:grid-cols-12 gap-5">

        {/* Classificació */}
        <div className="lg:col-span-5 bg-white rounded-2xl border border-slate-200 overflow-hidden">
          <div className="px-4 py-3 border-b border-slate-100 flex items-center gap-2">
            <span className="text-base">🏆</span>
            <h2 className="font-semibold text-slate-800 text-sm">Classificació Actual</h2>
          </div>
          <div className="overflow-x-auto">
            <table className="w-full text-xs">
              <thead>
                <tr className="bg-slate-50 border-b border-slate-100 text-slate-400 uppercase tracking-wide">
                  <th className="px-3 py-2 text-left w-8">#</th>
                  <th className="px-3 py-2 text-left">Equip</th>
                  <th className="px-2 py-2 text-center">PJ</th>
                  <th className="px-2 py-2 text-center">DG</th>
                  <th className="px-2 py-2 text-center font-bold text-slate-600">Pts</th>
                </tr>
              </thead>
              <tbody>
                {classificacio.map(eq => (
                  <tr key={eq.team} className="border-b border-slate-50 hover:bg-slate-50">
                    <td className="px-3 py-2">
                      <span className={`inline-flex items-center justify-center w-5 h-5 rounded-full text-xs ${posCls(eq.position, total)}`}>
                        {eq.position}
                      </span>
                    </td>
                    <td className="px-3 py-2 font-medium text-slate-800 max-w-[140px] truncate">{eq.team}</td>
                    <td className="px-2 py-2 text-center text-slate-500">{eq.played}</td>
                    <td className={`px-2 py-2 text-center font-medium ${eq.goal_diff > 0 ? "text-green-600" : eq.goal_diff < 0 ? "text-red-500" : "text-slate-400"}`}>
                      {eq.goal_diff > 0 ? `+${eq.goal_diff}` : eq.goal_diff}
                    </td>
                    <td className="px-2 py-2 text-center font-bold text-slate-900">{eq.points}</td>
                  </tr>
                ))}
              </tbody>
            </table>
          </div>
        </div>

        {/* Top Golejadors */}
        <div className="lg:col-span-3 bg-white rounded-2xl border border-slate-200 overflow-hidden">
          <div className="px-4 py-3 border-b border-slate-100 flex items-center gap-2">
            <span className="text-base">🥇</span>
            <h2 className="font-semibold text-slate-800 text-sm">Top Golejadors</h2>
          </div>
          <div className="divide-y divide-slate-50">
            {golejadors.map((j, i) => (
              <div key={j.player} className="flex items-center gap-2 px-4 py-2.5 hover:bg-slate-50">
                <span className={`w-5 text-center text-xs font-bold shrink-0 ${
                  i === 0 ? "text-yellow-500" : i === 1 ? "text-slate-400" : i === 2 ? "text-amber-600" : "text-slate-300"
                }`}>{i + 1}</span>
                <div className="flex-1 min-w-0">
                  <p className="text-xs font-medium text-slate-800 truncate">{j.player}</p>
                  <p className="text-xs text-slate-400 truncate">{j.team}</p>
                </div>
                <div className="text-right shrink-0">
                  <span className="text-sm font-bold text-green-600">{j.goals} ⚽</span>
                </div>
              </div>
            ))}
          </div>
        </div>

        {/* Última Jornada */}
        <div className="lg:col-span-4 bg-white rounded-2xl border border-slate-200 overflow-hidden">
          <div className="px-4 py-3 border-b border-slate-100 flex items-center gap-2">
            <span className="text-base">📅</span>
            <h2 className="font-semibold text-slate-800 text-sm">Última Jornada (J{kpis.jornades})</h2>
          </div>
          <div className="divide-y divide-slate-50">
            {ultima_jornada.map((p, i) => {
              const gh = p.goals_home, ga = p.goals_away;
              return (
                <div key={i} className="flex items-center gap-2 px-4 py-2.5 text-xs">
                  <span className={`flex-1 text-right truncate font-medium ${gh > ga ? "text-green-700" : gh < ga ? "text-slate-500" : "text-yellow-600"}`}>
                    {p.local_team}
                  </span>
                  <span className="font-bold text-slate-800 shrink-0 bg-slate-100 px-2 py-0.5 rounded">
                    {gh} - {ga}
                  </span>
                  <span className={`flex-1 truncate font-medium ${ga > gh ? "text-green-700" : ga < gh ? "text-slate-500" : "text-yellow-600"}`}>
                    {p.away_team}
                  </span>
                </div>
              );
            })}
          </div>
        </div>
      </div>
    </div>
  );
}