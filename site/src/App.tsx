import { DataProvider } from "./data/store";
import { ThemeProvider } from "./theme";
import { useRoute, Nav } from "./route";
import ThemeToggle from "./components/ThemeToggle";
import MainStory from "./MainStory";
import FusionPage from "./pages/FusionPage";
import JsonPage from "./pages/JsonPage";

function Router() {
  const route = useRoute();
  return (
    <div className="wrap">
      <Nav route={route} />
      {route === "fusion" ? <FusionPage /> : route === "json" ? <JsonPage /> : <MainStory />}
    </div>
  );
}

export default function App() {
  return (
    <ThemeProvider>
      <DataProvider>
        <ThemeToggle />
        <Router />
      </DataProvider>
    </ThemeProvider>
  );
}
