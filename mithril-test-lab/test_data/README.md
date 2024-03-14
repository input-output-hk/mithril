# Test immutable files

> [!IMPORTANT]
> The directory name matters, it must be named `immutable` in order to be understood as an
> immutable db directory by our db parser (see ([immutable_file.rs](./../../mithril-common/src/digesters/immutable_file.rs))).

> [!NOTE]
> The immutable files (`.chunk`, `.primary` and `.secondary`) files are data that are the result of
> the `mithril-end-to-end` test command execution.
> The `parsing_error/` directory contains the `04831` and `04832` immutable files with some unparsable blocks in the first one (`04831` have been produced by the Sanchonet network, and `04832` created manually).
> They are needed for testing of the Cardano transactions parser.
