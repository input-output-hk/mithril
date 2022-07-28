/** @type {import('next').NextConfig} */
const nextConfig = {
  basePath: "/showcase",
  reactStrictMode: true,
  swcMinify: true,
  experimental: {
    images: {
      unoptimized: true
    }
  }
}

module.exports = nextConfig
