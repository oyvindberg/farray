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
        "chains",
        "design/java-core",
        "design/scala-surface",
        "design/combinators",
        {
          type: "category",
          label: "Inside the ops",
          collapsed: false,
          items: [
            "operations/building",
            "operations/map",
            "operations/flatmap",
            "operations/filter",
            "operations/collect",
            "operations/folds",
            "operations/indexed",
            "operations/searching",
            "operations/builder",
            "operations/getting-out",
            "operations/list-parity",
          ],
        },
        "scoreboard",
        "benchmarks/farray",
      ],
    },
    {
      type: "category",
      label: "Fusion",
      collapsed: false,
      items: [
        "fusion/index",
        "fusion/sources",
        "fusion/stages",
        "fusion/terminals",
        "fusion/optimizer",
        "fusion/farray",
        "fusion/json",
        "fusion/benchmarks",
      ],
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
