---
slug: 14
title: |
  14. Choosing a TUI crate for setup command
authors:
  - name: Mithril Team
tags: [In progress]
date: 2026-10-12
---

## Status

In progress

## Context

With the goal of increasing Mithril adoption, we want to simplify the installation of the Mithril Signer for SPOs.
At present, following the Mithril signer node documentation and installing the signer takes around one hour.

We want to simplify this installation process by using a terminal user interface (TUI) to guide users through the signer installation flow.
We also need to choose a Rust TUI crate that will help us develop a smooth, one-command setup experience for users while remaining easy to maintain.

# How

Several candidates can help us build a terminal user interface. Here is the selection we made:
- [Ratatui](https://crates.io/crates/ratatui) with [crossterm](https://crates.io/crates/ratatui-crossterm/0.1.2)
- [inquire](https://crates.io/crates/inquire)
- [dialoguer](https://crates.io/crates/dialoguer)
- [cliclack](https://crates.io/crates/cliclack)

### What we will need in the TUI?

#### Inputs
- string, with a default value
- integer (prevent non-numeric characters or validate after input), with a default value
- multiple-choice selection
  - with other text input?

OR

table grid edit mode :
show a grid with all prefilled values with row selection to edit value (mouse selection + arrows)

#### Render
- progress bar for installation progress
- key-value summary

### Crate comparison
Dependencies count :
`grep -c '^\[\[package\]\]$' Cargo.lock`

| Name | Downloads | Size (KiB) | Dependencies | SSH compatible | Testable |
| --- | --- | --- | --- | --- | --- |
| Ratatui + crossterm | 59 106 231 | 70.7 + 18.4 | [Ratatui + crossterm + color-eyre](../../../ratatui-question-answer/cargo-tree-result.txt) | 207 | [UI output unit test](https://ratatui.rs/tutorials/counter-app/basic-app/#testing-the-ui-output) |
| inquire | 21 997 939 | 98.9 |  |  |  |
| dialoguer | 87 789 311 | 30.5 |  |  |  |
| cliclack | 3 863 258 | 37.2 |  |  |  |

## Decision

Todo

## Consequences

Todo