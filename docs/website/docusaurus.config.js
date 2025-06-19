// @ts-check
// Note: type annotations allow type checking and IDEs autocompletion

const lightCodeTheme = require("prism-react-renderer").themes.github;
const darkCodeTheme = require("prism-react-renderer").themes.dracula;

import remarkMath from "remark-math";
import rehypeKatex from "rehype-katex";

/** @type {import('@docusaurus/types').Config} */
const config = {
  title: "Mithril. A complete guide.",
  tagline: "Explore the user manual, key concepts, and API reference",
  url: "https://mithril.network",
  baseUrl: "/doc/",
  onBrokenLinks: "throw",
  onBrokenMarkdownLinks: "warn",
  favicon: "img/logo.svg",
  organizationName: "Input Output Global",
  projectName: "Mithril",

  scripts: [
    {
      src: "https://plausible.io/js/script.js",
      defer: true,
      "data-domain": "mithril.network",
    },
  ],

  presets: [
    [
      "classic",
      /** @type {import('@docusaurus/preset-classic').Options} */
      ({
        docs: {
          path: "root",
          routeBasePath: "/",
          sidebarPath: require.resolve("./sidebars.js"),
          editUrl: ({ docPath }) => {
            // We want users to submit doc updates to the upstream/next version!
            // Otherwise we risk losing the update on the next release.
            return `https://github.com/input-output-hk/mithril/edit/main/docs/website/root/${docPath}`;
          },
          lastVersion: "maintained",
          versions: {
            current: {
              label: "Next 🚧",
              banner: "unreleased",
            },
            maintained: {
              label: "Current",
            },
          },
          remarkPlugins: [remarkMath],
          rehypePlugins: [rehypeKatex],
        },
        blog: {
          path: "blog/",
          routeBasePath: "dev-blog",
          blogTitle: "Dev blog",
          blogSidebarTitle: "Dev blog",
          sortPosts: "descending",
          showReadingTime: true,
        },
        theme: {
          customCss: require.resolve("./src/css/custom.css"),
        },
        gtag: {
          trackingID: "G-4MB41WL2Z2",
          anonymizeIP: true,
        },
      }),
    ],
    [
      "redocusaurus",
      {
        specs: [
          {
            spec: "../../openapi.yaml",
            route: "/aggregator-api/",
          },
        ],
        theme: {
          primaryColor: "#1890ff",
        },
      },
    ],
  ],

  plugins: [
    async function myPlugin(context, options) {
      return {
        name: "docusaurus-tailwindcss",
        configurePostCss(postcssOptions) {
          // Appends TailwindCSS and AutoPrefixer.
          postcssOptions.plugins.push(require("tailwindcss"));
          postcssOptions.plugins.push(require("autoprefixer"));
          return postcssOptions;
        },
      };
    },
    [
      "@docusaurus/plugin-content-blog",
      {
        id: "adr_blog",
        path: "adr",
        routeBasePath: "adr/",
        blogTitle: "ADR",
        blogSidebarTitle: "ADR",
        sortPosts: "descending",
      },
    ],
    [
      "@docusaurus/plugin-client-redirects",
      {
        redirects: [
          {
            to: "/manual/develop/protocol-simulation",
            from: ["/mithril/mithril-protocol/simulation"],
          },
          {
            to: "/manual/operate/become-mithril-spo",
            from: ["/manual/getting-started/SPO-on-boarding-guide"],
          },
          {
            to: "/manual/operate/run-signer-node",
            from: ["/manual/getting-started/run-signer-node"],
          },
          {
            to: "/manual/develop/run-mithril-devnet",
            from: ["/manual/getting-started/run-mithril-devnet"],
          },
          {
            to: "/manual/develop/",
            from: ["/category/developer-docs"],
          },
          {
            to: "/manual/develop/nodes/mithril-aggregator",
            from: ["/manual/developer-docs/nodes/mithril-aggregator"],
          },
          {
            to: "/manual/develop/nodes/mithril-signer",
            from: ["/manual/developer-docs/nodes/mithril-signer"],
          },
          {
            to: "/manual/develop/nodes/mithril-client",
            from: ["/manual/developer-docs/nodes/mithril-client"],
          },
          {
            to: "/manual/develop/nodes/mithril-client-library",
            from: ["/manual/developer-docs/nodes/mithril-client-library"],
          },
          {
            to: "/manual/develop/nodes/mithril-client-library-wasm",
            from: ["/manual/developer-docs/nodes/mithril-client-library-wasm"],
          },
          {
            to: "/manual/develop/references",
            from: ["/manual/developer-docs/references"],
          },
          {
            to: "/mithril/advanced/mithril-protocol",
            from: ["/category/mithril-protocol"],
          },
          {
            to: "/mithril/advanced/mithril-protocol/protocol",
            from: ["/mithril/mithril-protocol/protocol"],
          },
          {
            to: "/mithril/advanced/mithril-protocol/certificates",
            from: ["/mithril/mithril-protocol/certificates"],
          },
          {
            to: "/mithril/advanced/mithril-protocol/security",
            from: ["/mithril/mithril-protocol/security"],
          },
          {
            to: "/mithril/advanced/mithril-network/",
            from: ["/category/mithril-network"],
          },
          {
            to: "/mithril/advanced/mithril-network/architecture",
            from: ["/mithril/mithril-network/architecture"],
          },
          {
            to: "/mithril/advanced/mithril-network/aggregator",
            from: ["/mithril/mithril-network/aggregator"],
          },
          {
            to: "/mithril/advanced/mithril-network/signer",
            from: ["/mithril/mithril-network/signer"],
          },
          {
            to: "/mithril/advanced/mithril-network/client",
            from: ["/mithril/mithril-network/client"],
          },
          {
            to: "/mithril/advanced/threat-model",
            from: ["/mithril/threat-model"],
          },
        ],
      },
    ],
  ],

  themeConfig:
    /** @type {import('@docusaurus/preset-classic').ThemeConfig} */
    ({
      metadata: [{ name: "og:image", content: "https://mithril.network/doc/img/logo.png" }],
      announcementBar: {
        id: "announcement",
        content:
          'Participate in Mithril Protocol’s Mainnet Beta! Follow our SPO on-boarding guide  <a rel="noopener noreferrer" href="https://mithril.network/doc/manual/operate/become-mithril-spo">here</a>!',
        backgroundColor: "#2A598E",
        textColor: "#f1f1f1",
        isCloseable: true,
      },
      colorMode: {
        defaultMode: "light",
        disableSwitch: true,
        respectPrefersColorScheme: false,
      },
      navbar: {
        logo: {
          alt: "Mithril. A complete guide.",

          src: "img/logo-text.svg",
        },
        items: [
          {
            type: "doc",
            docId: "manual/welcome",
            position: "left",
            label: "User manual",
          },
          {
            type: "doc",
            docId: "mithril/intro",
            label: "About Mithril",
            position: "left",
          },
          {
            type: "docsVersionDropdown",
            position: "right",
            dropdownActiveClassDisabled: true,
          },
          {
            label: "Explorer",
            to: "https://mithril.network/explorer",
            position: "right",
          },
          {
            to: "/glossary",
            label: "Glossary",
            position: "right",
          },
          { to: "/dev-blog", label: "Blog", position: "right" },
          { to: "/adr", label: "ADRs", position: "right" },
          {
            className: "header-github-link",
            href: "https://github.com/input-output-hk/mithril/",
            position: "right",
          },
        ],
      },
      footer: {
        logo: {
          alt: "Mithril. A complete guide.",
          src: "img/logo-dark.svg",
        },
        links: [
          {
            title: "Contributing",
            items: [
              {
                label: "Contributing guidelines",
                href: "https://github.com/input-output-hk/mithril/blob/main/CONTRIBUTING.md",
              },
              {
                label: "Architectural Decision Records",
                to: "/adr",
              },
            ],
          },
          {
            title: "Community",
            items: [
              {
                label: "Discord (#ask-mithril)",
                href: "https://discord.gg/5kaErDKDRq",
              },
              {
                label: "GitHub discussions",
                href: "https://github.com/input-output-hk/mithril/discussions",
              },
              {
                label: "Stack Exchange",
                href: "https://cardano.stackexchange.com/search?q=mithril",
              },
            ],
          },
          {
            title: "More",
            items: [
              {
                label: "Mithril networks status",
                href: "https://mithril.cronitorstatus.com/",
              },
              {
                label: "Mithril protocol insights",
                href: "https://lookerstudio.google.com/s/mbL23-8gibI",
              },
              {
                label: "Logbook",
                href: "https://github.com/input-output-hk/mithril/wiki/Logbook",
              },
              {
                label: "Input Output (Blog)",
                href: "https://iohk.io/en/blog",
              },
            ],
          },
          {
            title: "Legal",
            items: [
              {
                label: "Privacy policy",
                href: "https://static.iohk.io/terms/iog-privacy-policy.pdf",
              },
              {
                label: "Terms and conditions",
                href: "https://static.iohk.io/terms/iog-terms-and-conditions.pdf",
              },
            ],
          },
        ],
        copyright: `Copyright © ${new Date().getFullYear()} IOG, Inc.`,
      },
      prism: {
        theme: lightCodeTheme,
        darkTheme: darkCodeTheme,
        additionalLanguages: ["rust", "toml", "bash", "powershell"],
      },
      mermaid: {
        theme: { light: "base", dark: "base" },
      },
    }),
  markdown: {
    mermaid: true,
  },
  themes: ["@docusaurus/theme-mermaid"],
  stylesheets: [
    {
      href: "https://cdn.jsdelivr.net/npm/katex@0.13.24/dist/katex.min.css",
      type: "text/css",
      integrity: "sha384-odtC+0UGzzFL/6PNoE8rX/SPcQDXBJ+uRepguP4QkPCm2LBxH3FA3y+fKSiJ+AmM",
      crossorigin: "anonymous",
    },
    {
      href: "/css/custom.css",
      type: "text/css",
    },
  ],
};

module.exports = config;
