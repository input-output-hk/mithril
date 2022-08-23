# Contributing to Mithril

Thanks for considering contributing and help us on creating the Mithril protocol!

The best way to contribute right now is to try things out and provide feedback,
but we also accept contributions to the documentation and the obviously to the
code itself.

This document contains guidelines to help you get started and how to make sure
your contribution gets accepted, making you our newest Mithril contributor!

## Communication channels

Should you have any questions or need some help in getting set up, you can use
these communication channels to reach the Mithril team and get answers in a way
where others can benefit from it as well:

- Github [Discussions](https://github.com/input-output-hk/mithril/discussions)
- Cardano [StackExchange](https://cardano.stackexchange.com/) using the `mithril` tag

## Your first contribution

Contributing to the documentation, its translation, reporting bugs or proposing features are awesome ways to get started.

### Documentation

We host our documentation / user manual as a website [here](https://mithril.network/doc).

Each page has an "Edit this page" button which should take you to the source
file containing the markup. Should you would want to extend the documentation or
find some errors, please file an issue pointing to the mistake or even better,
create a pull request with the changes directly!

### Bug reports

[Submit an issue](https://github.com/input-output-hk/mithril/issues/new/choose) using the "Bug report :bug:" template.

For bug reports, it's very important to explain

- what version you used,
- steps to reproduce (or steps you took),
- what behavior you saw (ideally supported by logs), and
- what behavior you expected.

### Feature ideas

Feature ideas are precursors to high-level features items, which will be
discussed and fleshed out to ideally become items on our feature roadmap.

We use the [Ideas discussions category](https://github.com/input-output-hk/mithril/discussions/categories/ideas)
to discuss and vote on feature ideas, but you can also [submit an
issue](https://github.com/input-output-hk/mithril/issues/new/choose) using the
"Feature idea :thought_balloon:" template and we convert that to a discussion.

We expect a description of

- why you (or the user) need/want something (e.g. problem, challenge, pain, benefit), and
- what this is roughly about (e.g. description of a new API endpoint or message format).

Note that we do NOT require a detailed technical description, but are much more
interested in *why* a feature is needed. This also helps in understanding the
relevance and ultimately the priority of such an item.

## Making changes

When contributing code, it helps to have discussed the rationale and (ideally)
how something is implemented in a feature idea or bug ticket beforehand.

### Building & Testing

With the latest [Rust](https://www.rust-lang.org/tools/install) build tools installed, at the rootof the repository, or in each `mithril-` folder, run: `make`. This will build the libraries, executables and run tests. More information is available in the `README.md` files of each folder (see list of them [here](https://www.rust-lang.org/tools/install)).

Also you will find tutorials and technical documentation on the [`mithril.network`](https://mithril.network/doc/manual/welcome) documentation website.

Besides these general build instructions, some components might document
additional steps and useful tools in their `README.md` files.

### Coding standards

Make sure to follow our [Coding Standards](https://github.com/input-output-hk/mithril/wiki/Project-Charter#coding-standards).
It includes guidelines on Rust code style, but also on Git commit messages
and some processes. To propose new standards or changes to the existing standards, file an issue.

### Creating a pull request

Thank you for contributing your changes by opening a pull requests! To get
something merged we usually require:

- Description of the changes - if your commit messages are great, this is less important
- Quality of changes is ensured - through new or updated automated tests in [Github Actions](https://github.com/input-output-hk/mithril/actions)
- Change is related to an issue, feature (idea) or bug report - ideally discussed beforehand
- Well-scoped - we prefer multiple PRs, rather than a big one

### Versioning & Changelog

During development

- Make sure `CHANGELOG.md` is kept up-to-date with high-level, technical, but user-focused list of changes according to [keepachangelog](https://keepachangelog.com/en/1.0.0/)
- Bump `UNRELEASED` version in `CHANGELOG.md` according to [semver](https://semver.org/)
- All `mithril-` packages are versioned the same, at latest on release their versions are aligned.
- Other packages are versioned independently of `mithril-` packages and keep a dedicated changelog.

### Releasing

To perform a release

- Replace `UNRELEASED` with a date in [ISO8601](https://en.wikipedia.org/wiki/ISO_8601)
- Create a signed, annotated git tag of the version: `git tag -as <version>`
- (ideally) Use the released changes as annotation
