# Golden Test Assets

## Purpose

`halo2_ivc` golden tests rely on a small committed asset set so normal test runs
can validate recursive behavior without regenerating the full proving flow.

The expensive proofs are generated manually through ignored tests in
`mithril-stm/src/circuits/halo2_ivc/tests/golden/generators/tests.rs`.
The positive golden tests load those stored outputs at compile time via
`include_bytes!`, while regeneration keeps using the file-based readers/writers.

## Asset Set

The committed asset files live under:

`mithril-stm/src/circuits/halo2_ivc/tests/golden/assets`

Reader helpers live in:

`mithril-stm/src/circuits/halo2_ivc/tests/golden/asset_readers.rs`

The positive golden tests assume these committed asset files are present in the
worktree at compile time. If they were removed locally, restore them from git
before rebuilding or regenerating:

```bash
git restore mithril-stm/src/circuits/halo2_ivc/tests/golden/assets
```

The current asset set is:

- `verification_context.bin`
  - static verifier-side context
  - contains the global public inputs, recursive verifying key, combined fixed
    bases, and shared verifier-side SRS data

- `recursive_chain_state.bin`
  - stored recursive chain checkpoint
  - contains the global public inputs, current recursive state, previous
    recursive proof, and current accumulator

- `recursive_step_output.bin`
  - output of extending the stored chain checkpoint by one more recursive step
  - contains the final recursive proof, next accumulator, next state, and the
    exact certificate proof artifact folded into that stored step

## Dependency Order

The assets have a simple dependency chain:

1. `verification_context.bin`
2. `recursive_chain_state.bin`
3. `recursive_step_output.bin`

The chained-flow golden test replays the stored recursive step exactly, and the
stored `next_accumulator` depends on the exact certificate proof bytes embedded
inside `recursive_step_output.bin`.

## Generation Model

Asset generation uses:

- deterministic shared setup
- deterministic universal KZG parameters built with `ParamsKZG::unsafe_setup(...)`
- OS randomness for signatures and proof generation, aligned with the current
  generator flow

The public-state evolution is reproducible at the semantic level. Proof-bearing
assets are not expected to be byte-identical across regenerations.

In practice:

- `verification_context.bin` should stay stable unless verifier-side setup changes
- `recursive_chain_state.bin` may change bytewise because it contains a proof
- `recursive_step_output.bin` may change bytewise because it contains both the
  final recursive proof and the exact certificate proof used for that step

## How To Regenerate Everything

Run these commands from the repository root:

```bash
cargo test -p mithril-stm --features future_snark --release generate_verification_context_only -- --ignored --nocapture
cargo test -p mithril-stm --features future_snark --release generate_recursive_chain_state_only -- --ignored --nocapture
cargo test -p mithril-stm --features future_snark --release generate_recursive_step_output_only -- --ignored --nocapture
```

These commands intentionally use `--release` because asset generation is a
manual workflow dominated by real proof generation.

These commands correspond to the ignored generator entrypoints in
`mithril-stm/src/circuits/halo2_ivc/tests/golden/generators/tests.rs`:

- `generate_verification_context_only`
- `generate_recursive_chain_state_only`
- `generate_recursive_step_output_only`

Recommended order:

1. regenerate `verification_context.bin`
2. regenerate `recursive_chain_state.bin`
3. regenerate `recursive_step_output.bin`

## Partial Regeneration

If only the representative recursive step output changed, it is sufficient to rerun:

```bash
cargo test -p mithril-stm --features future_snark --release generate_recursive_step_output_only -- --ignored --nocapture
```

That command regenerates:

- `recursive_step_output.bin`

It does not require regenerating `verification_context.bin` or
`recursive_chain_state.bin` unless their inputs changed.

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
