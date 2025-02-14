// @ts-check

/** @type {import('@docusaurus/plugin-content-docs').SidebarsConfig} */
const sidebars = {
  manualSideBar: [
    {
      type: "autogenerated",
      dirName: "manual",
    },
    {
      type: "link",
      label: "Release notes",
      href: "https://github.com/input-output-hk/mithril/releases",
    },
    {
      type: "link",
      label: "Support",
      href: "https://discord.gg/5kaErDKDRq",
    },
  ],
  mithrilSideBar: [
    {
      type: "autogenerated",
      dirName: "mithril",
    },
  ],
};

module.exports = sidebars;
