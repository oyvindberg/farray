import type { Config } from "@docusaurus/types";
import type * as Preset from "@docusaurus/preset-classic";

// Docs-only Docusaurus site: the docs plugin serves at the site root, and every page is MDX that
// imports the interactive components (bench charts, extracted snippets, scorecards). The data those
// components consume is produced by scripts/build-data.mjs into static/data/ before every build.
const config: Config = {
  title: "FArray",
  tagline: "Immutable, unboxed collections for Scala 3 — at the speed of a raw array",
  favicon: "img/favicon.svg",

  url: "https://oyvindberg.github.io",
  baseUrl: "/farray/",
  organizationName: "oyvindberg",
  projectName: "farray",
  trailingSlash: false,

  onBrokenLinks: "throw",
  markdown: {
    hooks: { onBrokenMarkdownLinks: "throw" },
  },

  i18n: { defaultLocale: "en", locales: ["en"] },

  presets: [
    [
      "classic",
      {
        docs: {
          routeBasePath: "/", // docs-only mode: the docs ARE the site
          sidebarPath: "./sidebars.ts",
          editUrl: "https://github.com/oyvindberg/farray/tree/main/site/",
          showLastUpdateTime: false,
        },
        blog: false,
        pages: false,
        theme: { customCss: "./src/css/custom.css" },
      } satisfies Preset.Options,
    ],
  ],

  themeConfig: {
    // Prism's default bundle has no Scala: every ```scala fence rendered unhighlighted until this.
    prism: {
      additionalLanguages: ["scala", "java"],
    },
    metadata: [
      {
        name: "description",
        content:
          "How an immutable, Array-backed Scala sequence ended up beating List, Vector, fs2.Chunk and zio.Chunk — told through benchmarks and the source that produced them.",
      },
    ],
    colorMode: {
      defaultMode: "light",
      respectPrefersColorScheme: true,
    },
    docs: {
      sidebar: { hideable: false, autoCollapseCategories: false },
    },
    navbar: {
      title: "FArray",
      hideOnScroll: false,
      items: [
        { to: "/", label: "The story", position: "left", activeBaseRegex: "^/$" },
        { to: "/design/java-core", label: "FArray", position: "left", activeBaseRegex: "^/(design|operations|scoreboard|benchmarks/farray)" },
        { to: "/fusion", label: "Fusion", position: "left", activeBaseRegex: "^/fusion" },
        { to: "/fset", label: "FSet", position: "left", activeBaseRegex: "^/(fset|benchmarks/fset)" },
        { href: "https://github.com/oyvindberg/farray", label: "GitHub", position: "right" },
      ],
    },
    footer: {
      style: "light",
      copyright:
        "Charts are the checked-in JMH results; snippets are extracted verbatim from compiled source; lowerings are golden tests, regenerated on every build. Nothing on this site is a mock-up.",
    },
    tableOfContents: { minHeadingLevel: 2, maxHeadingLevel: 3 },
  } satisfies Preset.ThemeConfig,
};

export default config;
