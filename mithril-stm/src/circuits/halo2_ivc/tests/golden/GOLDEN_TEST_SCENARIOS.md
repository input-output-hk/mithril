# Golden Test Scenarios

## Purpose

The `halo2_ivc` positive golden tests provide a small safety net for refactoring.

For asset layout and regeneration commands, see `GOLDEN_TEST_ASSETS.md` in the
same directory.

They are not meant to cover every recursive case. They anchor five main
scenarios:

1. the recursive base case
2. one normal recursive step
3. the stored previous checkpoint
4. the stored next checkpoint
5. the continuity between those two checkpoints

## Why The Suite Uses Different Test Styles

The suite mixes two styles on purpose:

- `MockProver`
  - checks that a circuit instance is satisfiable
  - useful when we want circuit coverage without generating a full new proof

- stored assets
  - needed when the scenario depends on real proof artifacts being reused as inputs
  - useful for validating stored checkpoints and replaying a stored step exactly

In short:

- use `MockProver` when the question is "does the circuit accept this case?"
- use stored assets when the question is "is this proof-bearing checkpoint or step still valid?"

## Positive Scenarios

### `genesis_base_case_mock_prover`

Checks the recursive base case.

This is the first valid recursive step starting from genesis, where there is no
previous recursive proof yet. It protects the explicit base-case branch in the
recursive circuit and makes sure the circuit accepts the first valid recursive
transition.

### `normal_recursive_step_mock_prover`

Checks one normal non-genesis recursive step.

This protects the main recursive-step path in the circuit. In this scenario,
the circuit expects real proof-bearing inputs from the previous recursive step
and from the certificate side. We still use `MockProver` as the final checker,
so the test focuses on circuit correctness without paying the cost of a fresh
final recursive proof.

### `recursive_chain_state_asset_valid`

Checks that the stored recursive chain checkpoint is valid on its own.

This protects the stored previous recursive proof, state, and accumulator as one
consistent checkpoint. In other words, it checks that the saved "previous step"
artifact is still structurally valid and still verifies as expected.

### `recursive_step_output_asset_valid`

Checks that the stored next recursive step output is valid on its own.

This protects the stored final recursive proof and the stored next accumulator
as one consistent "next step" checkpoint. It tells us that the stored output
artifact is still valid in isolation.

### `recursive_step_output_chain_flow_asset_valid`

Checks the continuity between the two stored recursive checkpoints.

This test recomputes the expected next step from the stored previous chain
state, then replays the exact next accumulator from the stored step-output
asset. It protects the link between:

- the stored previous checkpoint
- the stored next step output

This is different from the asset-validity tests above. Those checks tell us that
each stored artifact is valid on its own. This one tells us that the stored
next output is really the continuation of the stored previous checkpoint, so the
two artifacts fit together as one recursive flow.

## How To Read The Suite

A good mental model is:

1. base case works
2. normal recursive step works
3. stored previous checkpoint is valid
4. stored next checkpoint is valid
5. stored previous and next checkpoints are connected correctly
