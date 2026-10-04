import "./globals.css";
import { GrupProvider } from "./components/GrupContext";
import Navbar from "./components/Navbar";

export const metadata = {
  title: "Futbol Català",
  description: "Estadístiques i anàlisi del futbol amateur català",
};

export default function RootLayout({ children }) {
  return (
    <html lang="ca">
      <body>
        <GrupProvider>
          <Navbar />
          <main className="max-w-7xl mx-auto px-4 py-8">
            {children}
          </main>
        </GrupProvider>
      </body>
    </html>
  );
}