# Cardano transactions prover benchmark

Run locally, against the `pre-release-preview` aggregator deployed with the distribution, then reported as a comment on the release issue, as in the previous release issues.

## When

The benchmark loads the aggregator VM, so it runs only after the user's `run`, given when the aggregator is not busy with other actions (e.g. a Cardano database snapshot). Before asking, show as information the age of the latest `CardanoDatabase` certificate (`/aggregator/artifact/cardano-database`). The aggregator must serve `$COMMIT` (`$PROBE aggregator-commit pre-release-preview $COMMIT`).

## Run

Printed for the user (run by the skill only with `--allow-run-commands` after `run`), about 2 minutes, work folder in the scratchpad:

```bash
bash scripts/prover-benchmark.sh run --aggregator https://aggregator.pre-release-preview.api.mithril.network/aggregator --benchmark-dir $REPOSITORY_DIR/mithril-test-lab/benchmark/aggregator-prover --transactions-file $REPOSITORY_DIR/mithril-test-lab/benchmark/aggregator-prover/transactions-preview.txt --work-dir <work folder>
```

It writes `benchmark.csv` and `benchmark.log` (paths made relative) in the work folder. A `>>>> Failure` line (exit `1`) stops the step.

## Previous results and comment

```bash
bash scripts/prover-benchmark.sh previous --issue $PREVIOUS_ISSUE_NUMBER --out <work folder>/previous.csv
bash scripts/prover-benchmark.sh comment --work-dir <work folder> --previous <work folder>/previous.csv --machine-type <type> --vcpus <n> --ram-gb <n> --issue $ISSUE_NUMBER
```

The machine type is the `google_machine_type` of `pre-release-preview` in `.github/workflows/pre-release.yml` (vCPUs and RAM of the GCP type, e.g. `e2-highmem-4` = 4 vCPUs, 32 GB). `comment` writes `benchmark-comment.md`, prints the delta table and the `gh issue comment` command (exit `1` when a row is flagged).

## Regression

Print a before/after table with the delta per row. A row more than 20 % below the previous result is flagged (the tolerance is wide because several Cardano nodes run on the aggregator VM), and the "no performance regression" child is ticked only on the user's verdict.
