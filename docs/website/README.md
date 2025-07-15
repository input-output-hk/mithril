# Website

This website is built using [Docusaurus 3](https://docusaurus.io/), a modern static website generator.

### Installation

```shell
$ make install
```

### Build

```shell
$ make build
```

This command generates static content into the `build` directory and can be served using any static contents hosting
service.

### Local Development

```shell
$ make dev
```

This command starts a local development server and opens up a browser window. Most changes are reflected live without
having to restart the server.

Alternatively, if you need to simulate GitHub pages hosting environment, you can run

```shell
$ make serve-static
```

This will create a production build and run it on a Python server as a static content.

### Content versioning

This website gives access to two versions of the documentation:

- `current`:
  - Hosts the documentation of the latest Mithril distribution
  - Shown by default
  - Source code can be found in `./root`
- `next`:
  - Hosts the documentation of the upcoming Mithril distribution under development
  - Accessible by selecting `Next` in the version dropdown in the navbar
  - Source code can be found in `./versioned_docs/version-maintained`

#### Versions rotation

When releasing a new distribution, the `current` documentation content can be rotated by using the `next` version with the command:

```shell
make update-current
```

### Upgrading swizzled components

To apply a custom theme to the website, some components from the `docusaurus-theme-classic` have been swizzled.
If there are changes from the docusaurus side, a makefile command is available to automatically swizzle them again and
try to apply our custom changes:

> [!WARNING]
> Conflicts have to be handled manually by the developer.

```shell
$ make swizzled-components-upgrade
```
