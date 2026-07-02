import type { SidebarsConfig } from "@docusaurus/plugin-content-docs";

// The whole site hierarchy in one place. Ordering matters: Docusaurus derives the
// previous/next pagination links on every page from this list. Benchmarks live inside
// the section they measure (FArray / FSet) rather than in a section of their own.
const sidebars: SidebarsConfig = {
  docs: [
    {
      type: "category",
      label: "The story",
      collapsed: false,
      items: ["index"],
    },
    {
      type: "category",
      label: "FArray",
      collapsed: false,
      items: [
        "design/java-core",
        "design/scala-surface",
        "design/combinators",
        "design/list-parity",
        {
          type: "category",
          label: "Inside the ops",
          collapsed: false,
          items: ["operations/map", "operations/flatmap", "operations/filter"],
        },
        "scoreboard",
        "benchmarks/farray",
      ],
    },
    {
      type: "category",
      label: "Fusion",
      collapsed: false,
      items: ["fusion/index", "fusion/optimizer", "fusion/json"],
    },
    {
      type: "category",
      label: "FSet",
      collapsed: false,
      items: ["fset", "benchmarks/fset"],
    },
  ],
};

export default sidebars;
