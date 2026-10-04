"use client";
import { createContext, useContext, useState } from "react";

const GrupContext = createContext(null);

export function GrupProvider({ children }) {
  const [categoria, setCategoria] = useState("TERCERA");
  const [grup,      setGrup]      = useState(1);

  return (
    <GrupContext.Provider value={{ categoria, grup, setCategoria, setGrup }}>
      {children}
    </GrupContext.Provider>
  );
}

export function useGrup() {
  const ctx = useContext(GrupContext);
  if (!ctx) throw new Error("useGrup ha d'estar dins de GrupProvider");
  return ctx;
}