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
                        docId: 'manual/getting-started/welcome',
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
                        type: 'dropdown',
                        label: 'Developer Docs',
                        position: 'left',
                        items: [
                            {
                                to: '/aggregator-api',
                                label: 'Aggregator Node - API Reference'
                            },
                            {
                                href: 'https://mithril.network/mithril-aggregator/doc/mithril_aggregator/index.html',
                                label: 'Aggregator Node - Rust documentation'
                            },
                            {
                                href: 'https://mithril.network/mithril-signer/doc/mithril_signer/index.html',
                                label: 'Signer Node - Rust documentation'
                            },
                            {
                                href: 'https://mithril.network/mithril-client/doc/mithril_client/index.html',
                                label: 'Client Node - Rust documentation'
                            },
                            {
                                href: 'https://mithril.network/mithril-core/doc/mithril/index.html',
                                label: 'Cryptographic Core Library - Rust documentation'
                            },
                            {
                                href: 'https://mithril.network/mithril-common/doc/mithril_common/index.html',
                                label: 'Common Node Library - Rust documentation'
                            },
                        ]
                    },
                    {
                        to: '/glossary',
                        label: 'Glossary',
                        position: 'right',
                    },
                    { to: '/adr', label: 'ADRs', position: 'right' },
                    {
                        className: 'header-github-link',
                        html: ':before',
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
                copyright: `Copyright © ${new Date().getFullYear()} <strong>Input Output</strong> <br/> <small>Built with Docusaurus</small>`,
            },
            prism: {
                theme: lightCodeTheme,
                darkTheme: darkCodeTheme,
            },
        }),
};

module.exports = config;
