# Test immutable files

> [!IMPORTANT]
> The directory name matters, it must be named `immutable` in order to be understood as an
> immutable db directory by our db parser (see ([immutable_file.rs](./../../mithril-common/src/digesters/immutable_file.rs))).

> [!NOTE]
> The immutable files (`.chunk`, `.primary` and `.secondary`) files are data that are the result of
> the `mithril-end-to-end` test command execution.
