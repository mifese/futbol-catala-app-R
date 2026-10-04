"use client";
import { useState } from "react";
import Link from "next/link";
import { usePathname } from "next/navigation";
import { useGrup } from "./GrupContext";

const GRUPS     = { TERCERA: 18, SEGONA: 6, PRIMERA: 3 };
const CAT_LABEL = { TERCERA: "Tercera", SEGONA: "Segona", PRIMERA: "Primera" };

const SECCIONS = [
  { href: "/",              label: "Inici",          icon: "🏠" },
  { href: "/partits",       label: "Partits",        icon: "📅" },
  { href: "/classificacio", label: "Classificació",  icon: "🏆" },
  { href: "/equips",        label: "Equips",         icon: "🛡️" },
  { href: "/jugadors",      label: "Jugadors",       icon: "👤" },
  { href: "/estadistiques", label: "Estadístiques",  icon: "📊" },
];

export default function Navbar() {
  const pathname = usePathname();
  const { categoria, grup, setCategoria, setGrup } = useGrup();
  const [menuObert, setMenuObert] = useState(false);
  const [grupObert, setGrupObert] = useState(false);

  const handleCategoria = (cat) => { setCategoria(cat); setGrup(1); setGrupObert(false); };
  const handleGrup      = (g)   => { setGrup(g); setGrupObert(false); };
  const isActive = (href) => href === "/" ? pathname === "/" : pathname.startsWith(href);

  return (
    <>
      <nav className="bg-white border-b border-slate-200 sticky top-0 z-50 shadow-sm">
        <div className="max-w-7xl mx-auto px-4">
          <div className="flex items-center justify-between h-14 gap-4">

            <Link href="/" className="flex items-center gap-2 font-bold text-green-600 text-lg shrink-0">
              ⚽ Futbol Català
            </Link>

            <div className="flex items-center gap-2 flex-1 justify-center">
              <div className="flex bg-slate-100 rounded-lg p-0.5 gap-0.5">
                {Object.keys(GRUPS).map((cat) => (
                  <button key={cat} onClick={() => handleCategoria(cat)}
                    className={`px-3 py-1.5 rounded-md text-xs font-semibold transition-all ${
                      categoria === cat ? "bg-green-600 text-white shadow-sm" : "text-slate-500 hover:text-slate-800"
                    }`}>
                    {CAT_LABEL[cat]}
                  </button>
                ))}
              </div>

              <div className="relative">
                <button onClick={() => setGrupObert(!grupObert)}
                  className="flex items-center gap-1.5 px-3 py-1.5 bg-slate-100 hover:bg-slate-200 rounded-lg text-xs font-semibold text-slate-700 transition-colors">
                  Grup {grup} <span className="text-slate-400">{grupObert ? "▲" : "▼"}</span>
                </button>
                {grupObert && (
                  <div className="absolute top-full mt-1 left-0 bg-white border border-slate-200 rounded-xl shadow-lg p-2 z-50">
                    <div className="grid grid-cols-6 gap-1">
                      {Array.from({ length: GRUPS[categoria] }, (_, i) => i + 1).map((g) => (
                        <button key={g} onClick={() => handleGrup(g)}
                          className={`w-8 h-8 rounded-lg text-xs font-medium transition-colors ${
                            grup === g ? "bg-green-600 text-white" : "hover:bg-slate-100 text-slate-700"
                          }`}>
                          {g}
                        </button>
                      ))}
                    </div>
                  </div>
                )}
              </div>
            </div>

            <button className="md:hidden p-2 rounded-lg text-slate-500 hover:bg-slate-100 shrink-0"
              onClick={() => setMenuObert(!menuObert)}>
              {menuObert ? "✕" : "☰"}
            </button>

            <div className="hidden md:flex items-center gap-0.5 shrink-0">
              {SECCIONS.filter(s => s.href !== "/").map(({ href, label }) => (
                <Link key={href} href={href}
                  className={`px-3 py-1.5 rounded-lg text-xs font-medium transition-colors ${
                    isActive(href) ? "bg-green-100 text-green-700" : "text-slate-600 hover:bg-slate-100"
                  }`}>
                  {label}
                </Link>
              ))}
            </div>
          </div>
        </div>

        {menuObert && (
          <div className="md:hidden border-t border-slate-100 bg-white px-4 py-2 flex flex-col gap-1">
            {SECCIONS.map(({ href, label, icon }) => (
              <Link key={href} href={href} onClick={() => setMenuObert(false)}
                className={`flex items-center gap-2 px-3 py-2 rounded-lg text-sm font-medium transition-colors ${
                  isActive(href) ? "bg-green-100 text-green-700" : "text-slate-600 hover:bg-slate-100"
                }`}>
                <span>{icon}</span> {label}
              </Link>
            ))}
          </div>
        )}
      </nav>
      {grupObert && <div className="fixed inset-0 z-40" onClick={() => setGrupObert(false)} />}
    </>
  );
}