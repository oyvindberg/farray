import { useColorMode } from "@docusaurus/theme-common";
import { ratioBand } from "../data/bench";

// The site's one color scale, made explicit: a gradient strip from "fastest" (green) through
// green-yellow (~1.2x behind), amber (~1.5x), faint red (2x) to blood red (5x+). Rendered from the
// SAME ratioBand function the charts and scorecards use, so the legend can't drift from the data.
const STOPS = [1.0, 1.05, 1.1, 1.2, 1.3, 1.5, 1.75, 2.0, 2.5, 3.0];
const LABELS: [number, string][] = [[1.0, "fastest"], [1.2, "1.2×"], [1.3, "1.3×"], [2.0, "2×"], [3.0, "3×+ behind"]];

export default function RatioLegend() {
  const dark = useColorMode().colorMode === "dark";
  const grad = STOPS.map((b, i) => `${ratioBand(1 / b, dark)} ${((i / (STOPS.length - 1)) * 100).toFixed(0)}%`).join(", ");
  return (
    <div className="ratiolegend" aria-label="color scale: distance behind the fastest structure">
      <div className="ratiolegend__bar" style={{ background: `linear-gradient(90deg, ${grad})` }} />
      <div className="ratiolegend__labels">
        {LABELS.map(([b, txt]) => (
          <span key={txt} style={{ left: `${((STOPS.findIndex((s) => s >= b) / (STOPS.length - 1)) * 100).toFixed(0)}%` }}>
            {txt}
          </span>
        ))}
      </div>
    </div>
  );
}
