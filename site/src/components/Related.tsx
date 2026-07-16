import Link from "@docusaurus/Link";

export interface RelatedLink {
  to: string;
  label: string;
  desc?: string;
}

// "Related pages" card row for the bottom of a doc — complements the automatic prev/next
// pagination with the cross-cutting links (a chapter's benchmark page, the fusion pages, …).
export default function Related({ links }: { links: RelatedLink[] }) {
  return (
    <nav className="related" aria-label="Related pages">
      <p className="related__title">related</p>
      <div className="related__grid">
        {links.map((l) => (
          <Link key={l.to} to={l.to} className="related__card">
            <span className="related__label">{l.label} →</span>
            {l.desc && <span className="related__desc">{l.desc}</span>}
          </Link>
        ))}
      </div>
    </nav>
  );
}
