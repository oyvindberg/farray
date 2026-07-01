// Big-O of each operation under the lazy-tree model. Structural ops build an O(1) node; the
// element-touching ops are O(n) and leave you holding a flat array afterwards.
const ROWS: { op: string; cost: string; builds: string; o1: boolean }[] = [
  { op: "+: prepend  ·  :+ append", cost: "O(1)", builds: "a Prepend / Append node", o1: true },
  { op: "++ concat", cost: "O(1)", builds: "a Concat node", o1: true },
  { op: "take · drop · slice", cost: "O(1)", builds: "a SliceNode", o1: true },
  { op: "reverse", cost: "O(1)", builds: "a ReverseNode", o1: true },
  { op: "updated", cost: "O(1)", builds: "an Updated node", o1: true },
  { op: "padTo", cost: "O(1)", builds: "a Pad node", o1: true },
  { op: "FArray.range", cost: "O(1)", builds: "a RangeNode — elements never allocated", o1: true },
  { op: "apply(i) · head · last", cost: "O(1) on a leaf · O(depth) on a node chain", builds: "—", o1: false },
  { op: "map · filter · flatMap · fold", cost: "O(n)", builds: "a flat array — you hold a leaf after", o1: false },
];

export default function Complexity() {
  return (
    <figure className="figure">
      <table className="cx">
        <thead>
          <tr><th className="cx-op">operation</th><th className="cx-cost">cost</th><th className="cx-b">what it builds</th></tr>
        </thead>
        <tbody>
          {ROWS.map((r) => (
            <tr key={r.op}>
              <td className="cx-op">{r.op}</td>
              <td className={`cx-cost${r.o1 ? " cx-cost--free" : ""}`}>{r.cost}</td>
              <td className="cx-b">{r.builds}</td>
            </tr>
          ))}
        </tbody>
      </table>
    </figure>
  );
}
