# Update the circuit keys after a change in one of the circuits

## When to use this guide

> [!IMPORTANT]
> This guide is intended to be used when one of the golden tests for the circuit verification key fails, either for the non-recursive or recursive certificate, and there is a modification done to one of the circuit that is intended. If one of the tests fail without any modification done to one of the circuits or the modification was not intended, do not use this guide and investigate further the reasons why this is happening.

## Role and responsabilities

Updating any circuit verification key is a costly process so we need to make sure it is done correctly and that it is justified. In order to do so, several persons need to be involve in the process.

The author of the change:

- Prepares the PR that includes the change
- Needs to justify the change in the circuit
- Takes care of Step 1 to 3

Reviewers:

- Perform a thorough review of the change
- Analyse the justification of the change to make sure it is valid and cannot be avoided
- Reviews Step 1 to 3

Release manager:

- Prepare the release of this update
- Updating any verification key is a breaking change that requires a re-genesis
- Takes care of Step 4 and 5

## Step 1: Update of the golden value

The author needs to update the golden value of the verification keys in the golden test in `mithril-stm/src/circuits/mod.rs`.
To do so, run:

- `golden_value_non_recursive_circuit()` or `golden_value_recursive_circuit()`
- Save the output of the function to the corresponding constant, `NON_RECURSIVE_GOLDEN_CIRCUIT_VERIFICATION_KEY` or `RECURSIVE_GOLDEN_CIRCUIT_VERIFICATION_KEY`

## Step 2: Update of the testing circuit verification key

## Step 3: Update of the production circuit verification key

## Step 4: Scheduling of the re-genesis

## Step 5: Checklist verification

## Rollback procedure in case of regression
