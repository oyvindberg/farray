import { DataProvider } from "./data/store";
import { ThemeProvider } from "./theme";
import { useRoute, Nav } from "./route";
import ThemeToggle from "./components/ThemeToggle";
import MainStory from "./MainStory";
import FusionPage from "./pages/FusionPage";
import JsonPage from "./pages/JsonPage";
import ReferencePage from "./pages/ReferencePage";
import FSetPage from "./pages/FSetPage";
import SetReferencePage from "./pages/SetReferencePage";

function Router() {
  const route = useRoute();
  return (
    <div className="wrap">
      <Nav route={route} />
      {route === "fusion" ? <FusionPage />
        : route === "json" ? <JsonPage />
        : route === "fset" ? <FSetPage />
        : route === "reference" ? <ReferencePage />
        : route === "setbench" ? <SetReferencePage />
        : <MainStory />}
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
