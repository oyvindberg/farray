import { useState } from "react";
import { useStore } from "../data/store";

interface Props {
  /** snippet NAME from a //start:NAME … //stop:NAME region in real source */
  name: string;
  /** hide the "entire file" toggle even when the file is small enough to embed */
  hideFull?: boolean;
}

export default function Snippet({ name, hideFull }: Props) {
  const { snippets, ready } = useStore();
  const [showFull, setShowFull] = useState(false);
  const [copied, setCopied] = useState(false);

  if (!ready) return <div className="snippet snippet--loading">loading source…</div>;
  const data = snippets[name];
  if (!data) {
    return (
      <div className="snippet snippet--error">
        snippet <code>{name}</code> not found — known: {Object.keys(snippets).join(", ")}
      </div>
    );
  }

  const html = showFull && data.fullHtml ? data.fullHtml : data.html;
  const raw = showFull && data.full ? data.full : data.code;
  const canFull = !hideFull && !!data.full;

  const copy = () => {
    navigator.clipboard?.writeText(raw).then(() => {
      setCopied(true);
      window.setTimeout(() => setCopied(false), 1200);
    });
  };

  return (
    <figure className="snippet">
      <figcaption className="snippet__bar">
        <span className="snippet__dot" aria-hidden />
        <span className="snippet__file">{data.file}</span>
        {canFull && (
          <label className="snippet__toggle">
            <input type="checkbox" checked={showFull} onChange={(e) => setShowFull(e.target.checked)} />
            entire file
          </label>
        )}
        <button className="snippet__copy" onClick={copy} type="button">
          {copied ? "copied" : "copy"}
        </button>
      </figcaption>
      <div className="snippet__code" dangerouslySetInnerHTML={{ __html: html }} />
    </figure>
  );
}
