import { useStore } from "../data/store";

// near-best green · >=3x slow red (mirrors python sc_cls)
function cell(x: number | null): string {
  if (x == null) return "sc-na";
  return x <= 1.3 ? "sc-hi" : x > 3.0 ? "sc-lo" : "sc-mid";
}

export default function Scorecard({ suite = "farray" }: { suite?: "farray" | "fset" }) {
  const { scorecard: fa, setScorecard, ready } = useStore();
  const scorecard = suite === "fset" ? setScorecard : fa;
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
                <td key={i} className={`${cell(x)}${scorecard.cols[i] === "TOTAL" ? " sc-total" : ""}`}>
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
        <b>geometric mean</b> of those per-cell ratios across the group — geometric, so a handful of
        thousand-× structural wins can't paper over ordinary losses. <b>Lower is better</b>: 1.00 = fastest
        everywhere, 3.0 = typically 3× off the pace. TOTAL is everything at once.
      </p>
    </div>
  );
}
