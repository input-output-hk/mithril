/** @type {import("fantasticon").RunnerOptions} */
module.exports = {
  inputDir: "./icons",
  outputDir: "./public/fonts",
  fontTypes: ["woff", "woff2"],
  assetTypes: ["css"],
  fontsUrl: "/explorer/fonts",
  formatOptions: {},
  name: "mithril-icons",
  prefix: "mi",
  templates: {
    css: "./font.css.hbs",
  },
  pathOptions: {
    css: "./src/app/mithril-icons.css",
  },
  codepoints: {
    logo: 57344, // 0xe000 = start of unicode private use area
  },
};
