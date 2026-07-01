import Snippet from "./Snippet";
import BenchChart from "./BenchChart";

interface Props {
  snippet: string;
  cls: string;
  title?: string;
  caption?: React.ReactNode;
}

// The "boom": the fused pipeline source on the left, what that one word does on the right.
export default function SideBySide({ snippet, cls, title, caption }: Props) {
  return (
    <figure className="figure figure--split">
      <div className="split">
        <div className="split__code">
          <Snippet name={snippet} hideFull />
        </div>
        <div className="split__chart">
          <BenchChart cls={cls} title={title} bare />
        </div>
      </div>
      {caption && <figcaption className="figure__cap">{caption}</figcaption>}
    </figure>
  );
}
