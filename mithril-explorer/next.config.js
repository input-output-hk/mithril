/** @type {import('next').NextConfig} */
const nextConfig = {
  basePath: "/explorer",
  reactStrictMode: true,
  swcMinify: true,
  experimental: {
    images: {
      unoptimized: true
    }
  }
}

module.exports = nextConfig
