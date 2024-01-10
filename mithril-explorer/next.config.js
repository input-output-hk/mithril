/** @type {import('next').NextConfig} */
const nextConfig = {
  output: "export",
  basePath: "/explorer",
  reactStrictMode: true,
  swcMinify: true,
  images: {
    unoptimized: true,
  },
  webpack: (config) => {
    config.experiments = { layers: true, asyncWebAssembly: true };
    return config;
  },
};

module.exports = nextConfig;
