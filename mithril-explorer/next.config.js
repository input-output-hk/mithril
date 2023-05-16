/** @type {import('next').NextConfig} */
const nextConfig = {
  output: "export",
  basePath: "/explorer",
  reactStrictMode: true,
  swcMinify: true,
  images: {
    unoptimized: true
  }
}

module.exports = nextConfig
