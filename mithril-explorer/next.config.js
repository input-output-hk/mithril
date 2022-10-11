/** @type {import('next').NextConfig} */
const nextConfig = {
  basePath: "/explorer",
  reactStrictMode: true,
  swcMinify: true,
  images: {
    unoptimized: true
  }
}

module.exports = nextConfig
