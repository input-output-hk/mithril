# ratatui-grid-edit-mode

This is a [Ratatui] terminal app for filling out a `SignerConfigDraft`.
The grid discovers draft fields recursively from their Serde representation;
Serde `rename` attributes define the display names shown in the grid.

## Usage

- Use the up/down arrow keys to select a row.
- Click a value cell to select and edit it.
- Press Enter to edit the selected value. Type the value and press Enter again
  to save it; press Esc to cancel editing.
- Network values must be `mainnet`, `preprod`, or `preview`. Numeric settings
  must be non-negative integers, and paths cannot be empty.
- Values supplied by the draft defaults are marked `default` in the provenance
  column. Values entered in the grid are marked `user`.
- Once every value is filled, press `f` to show the configuration summary.
- Press `q` or Esc to quit.

[Ratatui]: https://ratatui.rs