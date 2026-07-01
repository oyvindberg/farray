// Hand-drawn SVG diagrams for the three spatial ideas the prose leans on: the hybrid representation,
// what fusion removes, and how an inline map resolves. All theme-aware via CSS classes (see styles.css).

function Cells({ x, y, nums }: { x: number; y: number; nums: number[] }) {
  const w = 30;
  return (
    <g>
      {nums.map((n, i) => (
        <g key={i}>
          <rect className="dgm-cell" x={x + i * w} y={y} width={w} height={30} rx={2} />
          <text className="dgm-t dgm-num" x={x + i * w + w / 2} y={y + 20} textAnchor="middle">{n}</text>
        </g>
      ))}
    </g>
  );
}

/** A concrete FArray value — (1,2,3) ++ (4,5) then take(4) — drawn as its node tree. */
export function RepresentationDiagram() {
  return (
    <figure className="dgm">
      <svg viewBox="0 0 560 300" role="img" aria-label="An FArray node tree: a SliceNode over a Concat over two leaf arrays">
        {/* edges first, under the nodes */}
        <line className="dgm-edge" x1={280} y1={72} x2={280} y2={108} />
        <line className="dgm-edge" x1={252} y1={150} x2={150} y2={210} />
        <line className="dgm-edge" x1={308} y1={150} x2={395} y2={210} />

        {/* SliceNode (lazy) */}
        <rect className="dgm-lazy" x={192} y={26} width={176} height={46} rx={9} />
        <text className="dgm-t dgm-node" x={280} y={47} textAnchor="middle">SliceNode</text>
        <text className="dgm-op" x={280} y={63} textAnchor="middle">take(4)</text>

        {/* Concat (lazy) */}
        <rect className="dgm-lazy" x={205} y={112} width={150} height={46} rx={9} />
        <text className="dgm-t dgm-node" x={280} y={133} textAnchor="middle">Concat</text>
        <text className="dgm-op" x={280} y={149} textAnchor="middle">++</text>

        {/* leaves */}
        <Cells x={105} y={212} nums={[1, 2, 3]} />
        <Cells x={350} y={212} nums={[4, 5]} />
        <text className="dgm-dim dgm-leaflbl" x={150} y={262} textAnchor="middle">IntArr — a real int[]</text>
        <text className="dgm-dim dgm-leaflbl" x={380} y={262} textAnchor="middle">IntArr</text>
      </svg>
      <figcaption className="dgm-cap">
        <code>(1,2,3) ++ (4,5)</code> then <code>take(4)</code>. Both structural ops just add a{" "}
        <span className="dgm-key dgm-key--lazy">lazy node</span> — O(1), copies nothing. The data stays in
        the <span className="dgm-key dgm-key--leaf">flat leaves</span>. Reading walks the tree; <code>take(4)</code>{" "}
        stops after four elements, so the fifth is never touched.
      </figcaption>
    </figure>
  );
}

/** What fusion removes: the intermediate arrays between eager stages. */
export function FusionDiagram() {
  const arrow = (x1: number, x2: number, y: number, label: string) => (
    <g>
      <line className="dgm-edge dgm-arrow" x1={x1} y1={y} x2={x2 - 7} y2={y} markerEnd="url(#dgm-ah)" />
      <text className="dgm-op dgm-flow" x={(x1 + x2) / 2} y={y - 8} textAnchor="middle">{label}</text>
    </g>
  );
  const box = (x: number, y: number, label: string, cls: string) => (
    <g>
      <rect className={cls} x={x} y={y} width={62} height={34} rx={4} />
      <text className="dgm-t dgm-num" x={x + 31} y={y + 22} textAnchor="middle">{label}</text>
    </g>
  );
  return (
    <figure className="dgm">
      <svg viewBox="0 0 640 210" role="img" aria-label="Eager vs fused: eager allocates an array between every stage; fused runs one pass">
        <defs>
          <marker id="dgm-ah" markerWidth="7" markerHeight="7" refX="5" refY="3" orient="auto">
            <path className="dgm-ahp" d="M0,0 L6,3 L0,6 Z" />
          </marker>
        </defs>

        <text className="dgm-dim dgm-rowlbl" x={0} y={22} textAnchor="start">eager</text>
        {box(6, 34, "src", "dgm-box")}
        {arrow(68, 150, 51, "map")}
        {box(150, 34, "tmp", "dgm-box dgm-box--waste")}
        {arrow(212, 300, 51, "filter")}
        {box(300, 34, "tmp", "dgm-box dgm-box--waste")}
        {arrow(362, 452, 51, "map")}
        {box(452, 34, "out", "dgm-box")}
        <text className="dgm-dim dgm-note" x={528} y={55} textAnchor="start">3 arrays,</text>
        <text className="dgm-dim dgm-note" x={528} y={69} textAnchor="start">2 thrown away</text>

        <text className="dgm-dim dgm-rowlbl" x={0} y={150} textAnchor="start">.fuse</text>
        {box(6, 162, "src", "dgm-box")}
        {arrow(68, 300, 179, "map · filter · map — one while-loop")}
        {box(300, 162, "out", "dgm-box")}
        <text className="dgm-op dgm-note" x={376} y={183} textAnchor="start">0 intermediates</text>
      </svg>
      <figcaption className="dgm-cap">
        Every eager stage allocates an array the next stage immediately consumes and discards.{" "}
        <code>.fuse</code> rewrites the chain, at compile time, into one pass over the source — the middle
        arrays, the <code>Function1</code>s, the boxing, gone.
      </figcaption>
    </figure>
  );
}

/** How xs.map(_ + 1) resolves when xs: FArray[Int]. */
export function DispatchDiagram() {
  const step = (tag: string, body: React.ReactNode) => (
    <div className="dgm-step">
      <span className="dgm-step__tag">{tag}</span>
      <div className="dgm-step__body">{body}</div>
    </div>
  );
  return (
    <figure className="dgm dgm--flow">
      <div className="dgm-flow-col">
        {step("you write", <code>xs.map(_ + 1)</code>)}
        <div className="dgm-down"><span>inline — body + lambda spliced at the call site</span></div>
        {step("compile time", <><code>summonFrom</code>: <code>A = Int</code> → keep the <code>IntRepr</code> branch, delete the other 80</>)}
        <div className="dgm-down"><span>one resolved, monomorphic call</span></div>
        {step("runs", <><code>mapLeafIntInt(xs, (v: Int) =&gt; v + 1)</code> — one shared <code>int[]</code> loop, unboxed <code>IntToIntFn</code></>)}
      </div>
      <figcaption className="dgm-cap">
        The dispatch happens in the compiler, not the JIT. By the time the program runs there is no branch
        left — just a specialized loop and an unboxed function value.
      </figcaption>
    </figure>
  );
}
