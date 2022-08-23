// @ts-check
// Note: type annotations allow type checking and IDEs autocompletion

const lightCodeTheme = require('prism-react-renderer/themes/github');
const darkCodeTheme = require('prism-react-renderer/themes/dracula');

/** @type {import('@docusaurus/types').Config} */
const config = {
  title: 'Mithril. A complete guide.',
  tagline: 'Explore the user manual, key concepts, and API reference',
  url: 'https://mithril.network',
  baseUrl: '/doc/',
  onBrokenLinks: 'throw',
  onBrokenMarkdownLinks: 'warn',
  favicon: 'img/favicon.ico',
  organizationName: 'Input Output Global',
  projectName: 'Mithril',

  scripts: [
    {
      src: 'https://plausible.io/js/script.js',
      defer: true,
      'data-domain': 'mithril.network'
    }
  ],

  presets: [
    [
      'classic',
      /** @type {import('@docusaurus/preset-classic').Options} */
      ({
        docs: {
          path: 'root',
          routeBasePath: '/',
          sidebarPath: require.resolve('./sidebars.js'),
          editUrl: 'https://github.com/input-output-hk/mithril/edit/main/docs',
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
        gtag: {
          trackingID: 'G-4MB41WL2Z2',
          anonymizeIP: true,
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
      metadata: [{ name: 'og:image', content: 'https://mithril.network/doc/img/logo.png' }],
      announcementBar: {
        id: 'announcement',
        content:
          'We are actively looking for SPO candidates to test Mithril on the Cardano testnet. Contact us  <a rel="noopener noreferrer" href="https://contact.mithril.network/" target="_blank">here</a> if you are interested!',
        backgroundColor: '#2e8555',
        textColor: '#f1f1f1',
        isCloseable: true,
      },
      navbar: {
        title: 'Mithril',
        logo: {
          alt: 'Mithril. A complete guide.',
          src: 'img/logo.png',
        },
        items: [
          {
            type: 'doc',
            docId: 'manual/welcome',
            position: 'left',
            label: 'User Manual',
          },
          {
            type: 'doc',
            docId: 'mithril/intro',
            label: 'About Mithril',
            position: 'left',
          },
          {
            label: 'Explorer',
            to: 'https://mithril.network/explorer',
            position: 'right',
          },
          {
            to: '/glossary',
            label: 'Glossary',
            position: 'right',
          },
          { to: '/adr', label: 'ADRs', position: 'right' },
          {
            className: 'header-github-link',
            href: 'https://github.com/input-output-hk/mithril/',
            position: 'right',
          },
        ],
      },
      footer: {
        style: 'dark',
        links: [
          {
            title: 'Contributing',
            items: [
              {
                label: 'Project Charter',
                href: 'https://github.com/input-output-hk/mithril/wiki/Project-Charter',
              },
              {
                label: 'Architectural Decision Records',
                to: '/adr',
              }
            ],
          },
          {
            title: 'Community',
            items: [
              {
                label: 'Github Discussions',
                href: 'https://github.com/input-output-hk/mithril/discussions',
              },
              {
                label: 'Stack Exchange',
                href: 'https://cardano.stackexchange.com/questions/tagged/mithril',
              },
            ],
          },
          {
            title: 'More',
            items: [
              {
                label: 'Logbook',
                href: 'https://github.com/input-output-hk/mithril/wiki/Logbook'
              },
              {
                label: 'Input Output (Blog)',
                href: 'https://iohk.io/en/blog'
              },
            ],
          },
        ],
        copyright: `Copyright © ${new Date().getFullYear()} <strong>Input Output Global</strong> <br/> <a href="https://static.iohk.io/terms/iog-privacy-policy.pdf" target="_blank" class="footer__link-item">Privacy Policy</a> | <a href="https://static.iohk.io/terms/iohktermsandconditions.pdf" target="_blank" class="footer__link-item">Terms & Conditions</a> <br/> <small>Built with Docusaurus</small>`,
      },
      prism: {
        theme: lightCodeTheme,
        darkTheme: darkCodeTheme,
      },
    }),
};

module.exports = config;
