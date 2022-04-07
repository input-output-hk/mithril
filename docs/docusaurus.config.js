// @ts-check
// Note: type annotations allow type checking and IDEs autocompletion

const lightCodeTheme = require('prism-react-renderer/themes/github');
const darkCodeTheme = require('prism-react-renderer/themes/dracula');

/** @type {import('@docusaurus/types').Config} */
const config = {
  title: 'Mithril',
  tagline: 'User Manual, Core Concepts and API Reference',
  url: 'https://input-output-hk.github.io',
  baseUrl: '/',
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
          sidebarPath: require.resolve('./sidebars.js'),
          editUrl: 'https://github.com/input-output-hk/mithril/tree/main/docs/docs/',
        },
        blog: {
          path: 'milestone',
          routeBasePath: 'milestones/',
          blogTitle: "milestones",
          blogSidebarTitle: "Milestones",
          sortPosts: 'ascending',
          showReadingTime: true,
        },
        theme: {
          customCss: require.resolve('./src/css/custom.css'),
        },
      }),
    ],
  ],

  plugins: [
    [
      'content-docs',
      /** @type {import('@docusaurus/plugin-content-docs').Options} */
      ({
        id: 'core-concepts',
        path: 'core-concepts',
        routeBasePath: 'core-concepts',
        editUrl: 'https://github.com/input-output-hk/hydra-poc/tree/master/docs/core-concepts'
      })
    ],
    [
      'content-docs',
      /** @type {import('@docusaurus/plugin-content-docs').Options} */
      ({
        id: 'topologies',
        path: 'topologies',
        routeBasePath: 'topologies',
        editUrl: 'https://github.com/input-output-hk/hydra-poc/tree/master/docs/topologies'
      }),
    ],
    [
      'content-docs',
      /** @type {import('@docusaurus/plugin-content-docs').Options} */
      ({
        id: 'test-lab',
        path: 'test-lab',
        routeBasePath: 'test-lab',
        editUrl: 'https://github.com/input-output-hk/hydra-poc/tree/master/docs/test-lab'
      }),
    ],
  ],


  themeConfig:
    /** @type {import('@docusaurus/preset-classic').ThemeConfig} */
    ({
      navbar: {
        title: 'Mithril',
        logo: {
          alt: 'Mithril Logo',
          src: 'img/logo.svg',
        },
        items: [
          {
            type: 'doc',
            docId: 'intro',
            position: 'left',
            label: 'Tutorial',
          },
          {
            to: '/core-concepts',
            label: 'Core Concepts',
            position: 'left',
          },
          {
            to: '/topologies',
            label: 'Topologies',
            position: 'left',
          },
          {
            to: '/test-lab',
            label: 'Test Lab',
            position: 'left',
          },
          {
            to: '/topologies',
            label: 'Topologies',
            position: 'left',
          },
          {to: '/milestones', label: 'Milestones', position: 'right'},
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
          {
            title: 'Docs',
            items: [
              {
                label: 'Tutorial',
                to: '/docs/intro',
              },
            ],
          },
          // @todo
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
