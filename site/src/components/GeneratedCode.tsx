import { useState } from "react";
import { useStore } from "../data/store";

interface Props {
  name: string;
  summary: string;
}

// Collapsed-by-default panel showing a golden-file lowering — what the `.fuse` macro actually emits.
export default function GeneratedCode({ name, summary }: Props) {
  const { snippets, ready } = useStore();
  const [open, setOpen] = useState(false);
  if (!ready) return null;
  const data = snippets[name];
  if (!data) return <div className="snippet snippet--error">generated snippet <code>{name}</code> not found</div>;

  const lines = data.code.split("\n").length;
  return (
    <div className={`gen${open ? " gen--open" : ""}`}>
      <button className="gen__toggle" onClick={() => setOpen((o) => !o)} aria-expanded={open}>
        <span className="gen__chevron" aria-hidden>{open ? "▾" : "▸"}</span>
        {summary}
        <span className="gen__meta">{data.file.split("/").pop()} · {lines} lines</span>
      </button>
      {open && (
        <div className="gen__body">
          <div className="snippet__code" dangerouslySetInnerHTML={{ __html: data.html }} />
        </div>
      )}
    </div>
  );
}
