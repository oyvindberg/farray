import { useEffect, useState } from "react";

// Single source of truth for the site's pages. Adding one is one line here plus a branch in
// App's Router and its page component.
const TABS = [
  { id: "home", href: "#/", label: "FArray" },
  { id: "fusion", href: "#/fusion", label: "Fusion" },
  { id: "json", href: "#/json", label: "Fused JSON" },
  { id: "fset", href: "#/fset", label: "FSet" },
  { id: "reference", href: "#/reference", label: "FArray benchmarks" },
  { id: "setbench", href: "#/setbench", label: "FSet benchmarks" },
] as const;

export type Route = (typeof TABS)[number]["id"];
const IDS = TABS.map((t) => t.id) as string[];

function read(): Route {
  const h = location.hash.replace(/^#\/?/, "");
  return (IDS.includes(h) ? h : "home") as Route;
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
