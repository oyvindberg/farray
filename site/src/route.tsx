import { useEffect, useState } from "react";

export type Route = "home" | "fusion" | "json";

function read(): Route {
  const h = location.hash.replace(/^#\/?/, "");
  return h === "fusion" || h === "json" ? h : "home";
}

export function useRoute(): Route {
  const [route, setRoute] = useState<Route>(read);
  useEffect(() => {
    const on = () => { setRoute(read()); window.scrollTo(0, 0); };
    window.addEventListener("hashchange", on);
    return () => window.removeEventListener("hashchange", on);
  }, []);
  return route;
}

const TABS: { id: Route; href: string; label: string }[] = [
  { id: "home", href: "#/", label: "FArray" },
  { id: "fusion", href: "#/fusion", label: "Fusion" },
  { id: "json", href: "#/json", label: "Fused JSON" },
];

export function Nav({ route }: { route: Route }) {
  return (
    <nav className="nav">
      {TABS.map((t) => (
        <a key={t.id} href={t.href} className={t.id === route ? "nav__tab nav__tab--on" : "nav__tab"}>
          {t.label}
        </a>
      ))}
    </nav>
  );
}
