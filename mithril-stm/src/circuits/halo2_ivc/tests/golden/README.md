# Golden Test Assets

## Purpose

`halo2_ivc` golden tests rely on a small committed asset set so normal test runs
can validate recursive behavior without regenerating the full proving flow.

The expensive proofs are generated manually through ignored tests in
`mithril-stm/src/circuits/halo2_ivc/tests/common/generators/asset_generation.rs`.
The positive golden tests load those stored outputs at compile time via
`include_bytes!`, while regeneration keeps using the file-based readers/writers.

## Asset Set

The committed asset files live under:

`mithril-stm/src/circuits/halo2_ivc/tests/assets`

Reader helpers live in:

`mithril-stm/src/circuits/halo2_ivc/tests/common/asset_readers.rs`

The positive golden tests assume these committed asset files are present in the
worktree at compile time. If they were removed locally, restore them from git
before rebuilding or regenerating:

```bash
git restore mithril-stm/src/circuits/halo2_ivc/tests/assets
```

The current asset set is:

- `verification_context.bin` — static verifier-side context (verifying key, global inputs, fixed bases, SRS)
- `recursive_chain_state.bin` — chain checkpoint after several recursive steps
- `genesis_step_output.bin` — output of the genesis base-case step
- `same_epoch_step_output.bin` — output of a same-epoch recursive step
- `recursive_step_output.bin` — output of a next-epoch recursive step

## Dependency Order

1. `verification_context.bin` — no dependencies
2. `genesis_step_output.bin` — no dependencies
3. `recursive_chain_state.bin` — no dependencies
4. `same_epoch_step_output.bin` — depends on `recursive_chain_state.bin`
5. `recursive_step_output.bin` — depends on `recursive_chain_state.bin`

## Generation Model

Asset generation uses:

- deterministic shared setup
- deterministic universal KZG parameters built with `ParamsKZG::unsafe_setup(...)`
- OS randomness for signatures and proof generation, aligned with the current
  generator flow

The public-state evolution is reproducible at the semantic level. Proof-bearing
assets are not expected to be byte-identical across regenerations.

## How To Regenerate Everything

Run these commands from the repository root in order:

```bash
cargo test -p mithril-stm --features future_snark --release generate_verification_context_only -- --ignored --nocapture
cargo test -p mithril-stm --features future_snark --release generate_genesis_step_output_only -- --ignored --nocapture
cargo test -p mithril-stm --features future_snark --release generate_recursive_chain_state_only -- --ignored --nocapture
cargo test -p mithril-stm --features future_snark --release generate_same_epoch_step_output_only -- --ignored --nocapture
cargo test -p mithril-stm --features future_snark --release generate_recursive_step_output_only -- --ignored --nocapture
```

These commands intentionally use `--release` because asset generation is a
manual workflow dominated by real proof generation.

## When Regeneration Is Needed

Regenerate the assets when one of these changes:

- recursive circuit logic
- recursive public input or state layout
- accumulator encoding
- recursive or certificate verifying key inputs
- verifier-side SRS data format
- generation setup or proving randomness model
- chained-flow replay contract for the recursive step assets

## Temporary Certificate Dependency

The current asset generation depends on the temporary local certificate relation
in
`mithril-stm/src/circuits/halo2_ivc/tests/test_certificate.rs`.

This is a temporary compatibility step. It keeps recursive asset generation
close to the original prototype flow while the STM non-recursive Halo2
certificate circuit continues to evolve.

The intended follow-up is to remove this temporary local relation and generate
the assets directly from the current STM Halo2 certificate circuit.
