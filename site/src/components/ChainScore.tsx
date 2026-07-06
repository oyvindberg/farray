import { useStore } from "../data/store";

// Live W/T/L strip over every CHAINED-operation benchmark class in the scorecard — computed from
// the same JSON as the reference page, so it can't drift from the data. Chained = the benchmark
// runs two or more collection operations per invocation (the class list below is curated by hand;
// single-op classes deliberately excluded — those are the "floor" story, not the chain story).
const CHAINED_PREFIXES = [
  "LongMixedPipeline", "StructuralShowcase", "MapFilterFold", "FilterMapReverse", "DropConcatFold",
  "TakeDropFold", "DropTakeMap", "MapFlatMapFold", "ConcatDropTake", "PrependAppendMap",
  "FlatMapChain", "AppendChain", "PrependChain", "UpdateChain", "MixedTree", "DeepConcat",
  "FlatMapFilterTake", "ReverseMap", "AppendConcatReverse",
];

export default function ChainScore() {
  const { charts, ready } = useStore();
  if (!ready) return <div className="snippet snippet--loading">loading benchmark data…</div>;
  let w = 0, t = 0, l = 0, classes = 0;
  const seen = new Set<string>();
  for (const c of charts) {
    if (!CHAINED_PREFIXES.some((p) => c.cls.startsWith(p))) continue;
    if (!seen.has(c.cls)) { seen.add(c.cls); classes++; }
    w += c.w; t += c.t; l += c.l;
  }
  if (w + t + l === 0) return null;
  return (
    <div className="chainscore">
      <span className="chainscore__num chainscore__num--w">{w} wins</span>
      <span className="chainscore__num chainscore__num--t">{t} ties</span>
      <span className="chainscore__num chainscore__num--l">{l} losses</span>
      <span className="chainscore__note">
        across {classes} benchmark classes, computed from the checked-in scorecard.
      </span>
    </div>
  );
}
