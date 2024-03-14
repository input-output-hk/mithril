# Mithril Explorer

**This is a work in progress** :hammer_and_wrench:

A website showcasing data from a Mithril Aggregator made using [Next.js](https://nextjs.org/).

---

## Pre-requisites

Install [node.js](https://nodejs.org/en/) && (yarn)[https://yarnpkg.com/getting-started/install].

## Getting Started

First, build the `mithril-client-wasm` package:

```bash
make -C ../mithril-client-wasm build
```

Then, run the development server:

```bash
make dev
```

Open [http://localhost:3000](http://localhost:3000/explorer) with your browser to see the result.

## Adding or updating an icon of the 'Mithril' font

In the `./icons` folder add or modify a svg.

If you add a new icon you need to reference and associate it with a codepoint in the `./fantasticonrc.js`
configuration file.

Then rebuild the font:

```bash
make icons-font
```

You can then use the icon in the js, ie if your icon name is `shield`:

```jsx
<i className={`bi mi mi-shield`}></i>
```
