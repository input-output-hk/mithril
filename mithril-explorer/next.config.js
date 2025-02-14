const webpack = require("webpack");

/** @type {import("next").NextConfig} */
const nextConfig = {
  output: "export",
  basePath: process.env.BASE_PATH ?? "/explorer",
  reactStrictMode: true,
  images: {
    unoptimized: true,
  },
  webpack: (config) => {
    config.experiments = { ...config.experiments, layers: true, asyncWebAssembly: true };
    config.plugins.push(
      new webpack.DefinePlugin({
        "process.env.UNSTABLE": process.env.UNSTABLE === "1",
      }),
    );
    return config;
  },
};

module.exports = nextConfig;
