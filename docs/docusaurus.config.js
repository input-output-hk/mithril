// @ts-check
// Note: type annotations allow type checking and IDEs autocompletion

const lightCodeTheme = require('prism-react-renderer/themes/github');
const darkCodeTheme = require('prism-react-renderer/themes/dracula');

/** @type {import('@docusaurus/types').Config} */
const config = {
  title: 'Mithril',
  tagline: 'User Manual, Core Concepts and API Reference',
  url: 'https://input-output-hk.github.io',
  baseUrl: '/doc/',
  onBrokenLinks: 'throw',
  onBrokenMarkdownLinks: 'warn',
  favicon: 'img/favicon.ico',
  organizationName: 'Input Output',
  projectName: 'Mithril',

  presets: [
    [
      'classic',
      /** @type {import('@docusaurus/preset-classic').Options} */
      ({
        docs: {
          path: 'root',
          routeBasePath: '/',
          sidebarPath: require.resolve('./sidebars.js'),
          editUrl: 'https://github.com/input-output-hk/mithril/tree/main/docs/root/',
        },
        blog: {
          path: 'adr',
          routeBasePath: 'adr/',
          blogTitle: "ADR",
          blogSidebarTitle: "ADR",
          sortPosts: 'ascending',
          showReadingTime: true,
        },
        theme: {
          customCss: require.resolve('./src/css/custom.css'),
        },
      }),
    ],
    [
      'redocusaurus',
      {
        specs: [
          {
            spec: '../openapi.yaml',
            route: '/aggregator-api/',
          },
        ],
        theme: {
          primaryColor: '#1890ff',
        },
      },
    ],
  ],

  themeConfig:
  /** @type {import('@docusaurus/preset-classic').ThemeConfig} */
    ({
      navbar: {
        title: 'Mithril',
        logo: {
          alt: 'Mithril Logo',
          src: 'img/logo.png',
        },
        items: [
          {
            type: 'doc',
            docId: 'intro',
            position: 'left',
            label: 'Getting Started',
          },
          {
            type: 'doc',
            docId: 'core-concepts/index',
            label: 'Core Concepts',
            position: 'left',
          },
          {
            type: 'doc',
            docId: 'topologies/index',
            label: 'Topologies',
            position: 'left',
          },
          {
            type: 'doc',
            docId: 'test-lab/index',
            label: 'Test Lab',
            position: 'left',
          },
          {
            to: '/aggregator-api',
            label: 'Aggregator Server Api Reference',
            position: 'left',
          },
          {to: '/adr', label: 'ADRs', position: 'right'},
          {
            href: 'https://github.com/input-output-hk/mithril/',
            label: 'GitHub',
            position: 'right',
          },
        ],
      },
      footer: {
        style: 'dark',
        links: [
          // @todo
          // {
          //   title: 'Contributing',
          //   items: [],
          // },
          // {
          //   title: 'Community',
          //   items: [],
          // },
          {
            title: 'More',
            items: [
              {
                label: 'Input Output (Blog)',
                to: 'https://iohk.io/en/blog'
              },
            ],
          },
        ],
        copyright: `Copyright © ${new Date().getFullYear()} <strong>Input Output</strong> <br/> <small>Built with Docusaurus</small>`,
      },
      prism: {
        theme: lightCodeTheme,
        darkTheme: darkCodeTheme,
      },
    }),
};

module.exports = config;
