import { useStore } from "../data/store";
import { useColorMode } from "@docusaurus/theme-common";
import { ratioBand } from "../data/bench";

export default function Scorecard({ suite = "farray" }: { suite?: "farray" | "fset" }) {
  const { scorecard: fa, setScorecard, ready } = useStore();
  const scorecard = suite === "fset" ? setScorecard : fa;
  const dark = useColorMode().colorMode === "dark";
  if (!ready || !scorecard) return <div className="snippet snippet--loading">tallying…</div>;

  return (
    <div className="scorewrap">
      <table className="score">
        <thead>
          <tr>
            <th className="snm">structure</th>
            {scorecard.cols.map((c) => (
              <th key={c} className={c === "TOTAL" ? "sc-total" : undefined}>{c}</th>
            ))}
          </tr>
        </thead>
        <tbody>
          {scorecard.rows.map((row) => (
            <tr key={row.v} className={row.ours ? "srow srow--me" : "srow"}>
              <td className="snm"><i style={{ background: row.color }} />{row.label}</td>
              {row.vals.map((x, i) => (
                <td key={i} className={`sc-cell${scorecard.cols[i] === "TOTAL" ? " sc-total" : ""}`}
                    style={x == null ? undefined : { background: ratioBand(1 / x, dark) }}>
                  {x == null ? "·" : x.toFixed(2)}
                </td>
              ))}
            </tr>
          ))}
        </tbody>
      </table>
      <p className="scorenote">
        <b>How to read this.</b> For every benchmark at every size, the fastest structure in that cell scores{" "}
        <b>1.00</b>; everyone else scores how many times slower they ran. Each number here is the{" "}
        <b>geometric mean</b> of those per-cell ratios across the group; geometric, so a handful of
        thousand-× structural wins can't paper over ordinary losses. <b>Lower is better</b>: 1.00 = fastest
        everywhere, 3.0 = typically 3× off the pace. TOTAL is everything at once. Cell color is the
        site's one scale: green at the front, amber by 1.2×, red past 1.3×, blood-red at 3× behind.
      </p>
    </div>
  );
}
