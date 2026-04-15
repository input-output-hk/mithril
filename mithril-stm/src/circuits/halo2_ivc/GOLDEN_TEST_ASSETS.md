# Golden Test Assets

## Purpose

This module uses small stored assets for the `halo2_ivc` golden tests.

The goal is to verify stable recursive proof behavior without regenerating the
full recursive flow in every test run. The expensive proof generation is done
manually through ignored tests, and the golden tests will load the stored
outputs.

## Stored assets

The current asset set is:

- `verification_context.bin`
  - static verifier-side data
  - contains the global public inputs, the recursive verifying key, the
    combined fixed bases, and the shared verifier-side SRS data

- `recursive_chain_state.bin`
  - stored recursive chain snapshot
  - contains the global public inputs, the current recursive state, the
    previous recursive proof, and the current accumulator

- `recursive_step_output.bin`
  - output of extending the stored recursive chain by one more step
  - contains the final recursive proof, the next accumulator, and the next
    state

## How assets are generated

Assets are generated from ignored tests in
`mithril-stm/src/circuits/halo2_ivc/tests/generators.rs`.

Generation uses:

- deterministic shared setup
- deterministic random seeds for proof generation
- deterministic universal KZG parameters built with `ParamsKZG::unsafe_setup(...)`

The three manual generation entrypoints are:

- `generate_verification_context_only`
- `generate_recursive_chain_state_only`
- `generate_recursive_step_output_only`

The recursive proving flow follows the same high-level split as the prototype:

- intermediate recursive proofs use the Poseidon transcript
- the final recursive proof uses the Blake2b transcript

## Where assets are stored

Stored asset files live under:

`mithril-stm/src/circuits/halo2_ivc/assets`

The reader helpers live in:

`mithril-stm/src/circuits/halo2_ivc/tests/asset_readers.rs`

## When to regenerate

Regenerate the assets when one of the following changes:

- the recursive circuit logic
- the recursive public input/state layout
- the accumulator encoding
- the recursive or certificate verifying key inputs
- the verifier-side SRS data format
- the deterministic generation setup or seeds

Regenerate them with:

```bash
cargo test --features future_snark generate_verification_context_only -- --ignored --nocapture
cargo test --features future_snark generate_recursive_chain_state_only -- --ignored --nocapture
cargo test --features future_snark generate_recursive_step_output_only -- --ignored --nocapture
```

Run these commands from the `mithril-stm` crate directory.

To validate that the stored assets still deserialize correctly, also run:

```bash
cargo test --features future_snark load_generated_assets_only -- --ignored --nocapture
```

This is a manual smoke test for the asset readers.

## Temporary certificate relation dependency

The current asset generation depends on the temporary local certificate
relation in
`mithril-stm/src/circuits/halo2_ivc/tests/test_certificate.rs`.

This is a temporary compatibility step. It keeps the recursive asset generation
path close to the original prototype flow while the STM non-recursive Halo2
certificate circuit continues to evolve.

The intended follow-up is to remove this temporary local relation and generate
the assets directly from the current STM Halo2 certificate circuit.
