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
    config.experiments = { layers: true, asyncWebAssembly: true };
    
    const { join } = require('path');
    const { access, symlink } = require('fs/promises');
    config.plugins.push(
      new (class {
        apply(compiler) {
          compiler.hooks.afterEmit.tapPromise(
            'SymlinkWebpackPlugin',
            async (compiler) => {
              if (isServer) {
                const from = join(compiler.options.output.path, '../static');
                const to = join(compiler.options.output.path, 'static');
    
                try {
                  await access(from);
                  // console.log(`${from} already exists`);
                  return;
                } catch (error) {
                  if (error.code === 'ENOENT') {
                    // No link exists
                  } else {
                    throw error;
                  }
                }
    
                await symlink(to, from, 'junction');
                console.log(`created symlink ${from} -> ${to}`);
              }
            },
          );
        }
      })(),
    );

    return config;
  }
};

module.exports = nextConfig;
