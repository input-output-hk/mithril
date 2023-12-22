/** @type {import('next').NextConfig} */
const nextConfig = {
  output: "export",
  basePath: "/explorer",
  reactStrictMode: true,
  swcMinify: true,
  images: {
    unoptimized: true,
  },
  webpack: function (config, { isServer }) {
    config.experiments = { asyncWebAssembly: true };
    return config;
  }
};

module.exports = nextConfig;
