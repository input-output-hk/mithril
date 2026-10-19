# `/mithril-distribution` help

## Usage

```
/mithril-distribution <distribution> <phase> [--dry-run | --plan | --check] [--no-issue-tick] [--allow-run-commands]
/mithril-distribution help [<phase>]
```

| Argument               | Values                                                                                                                                                                           |
| ---------------------- | -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `<distribution>`       | Distribution number, e.g. `2642`. Used to find the release issue (`Release \`2642\` distribution`), the tags, the rollout log and the announcements file                         |
| `<phase>`              | `prepare` (How "Phase 1"), `pre-release` (How "Phase 2"), `release` (How "Phase 3")                                                                                              |
| `--dry-run`            | Rehearse the phase: real read-only checks, no write at all                                                                                                                       |
| `--plan`               | Print every command of the phase from its first step, in order, then stop, also written to `<distribution>-<phase>-PLAN.md` in the log directory                                 |
| `--check`              | Report each step of the phase: 🟢 done and checked, 🟡 done not checked, 🔴 checked not done, ⚪ not done not checked, ⚫ checked with no probe, then stop (no answer, no write) |
| `--no-issue-tick`      | No tick command: you check the boxes on the issue, the skill waits for your check before moving on                                                                               |
| `--allow-run-commands` | Let the skill run the write commands after your `run` (default: it only displays them, you run them and answer `done`)                                                           |

Examples: `/mithril-distribution 2642 prepare --dry-run`, `/mithril-distribution 2642 pre-release`, `/mithril-distribution help release`.

## Phases

| Phase         | What it does                                                                                             | Deploys                                                                      |
| ------------- | -------------------------------------------------------------------------------------------------------- | ---------------------------------------------------------------------------- |
| `prepare`     | CI tests, dependencies PR, caches, changelog, draft PRs, announcements preparation                       | Nothing                                                                      |
| `pre-release` | Environment configuration, signed tag push, release notes, checks, announcements, 2-epoch wait           | `pre-release-preview` (tag push), `pre-release-2-preview` out of the process |
| `release`     | KMS rotation, environment configuration, release publication, checks, packages, PR merges, announcements | `release-preprod` and `release-mainnet` (release publication)                |

## Steps

The skill reads the unchecked How checkboxes of the phase from the release issue and handles them in order. Each step has a kind:

| Kind     | What the skill does                                                                                                                          |
| -------- | -------------------------------------------------------------------------------------------------------------------------------------------- |
| `check`  | Runs a read-only check and shows the result                                                                                                  |
| `action` | Shows the exact write command (or batch) and its expected result, then detects the result                                                    |
| `impact` | Same as `action`, for a command that changes an environment (variables, secrets, tag push, release publication): shown only after your `ack` |
| `manual` | Gives the instructions and polls the checkbox of the step on the issue                                                                       |
| `wait`   | Records the target epoch and pauses the phase                                                                                                |

## Answers by option

What you can answer depends on the options of the run. A cell with "—" means the answer is refused.

| Situation                          | Default                                         | `--allow-run-commands`                                    | `--dry-run`                  |
| ---------------------------------- | ----------------------------------------------- | --------------------------------------------------------- | ---------------------------- |
| Context summary shown              | `go`                                            | `go`                                                      | `go`                         |
| `action` command shown             | `done` (you ran it), `skip <reason>`            | `run` (the skill runs it), `I'll run it`, `skip <reason>` | `rehearsed`, `issue: <text>` |
| `impact` command announced         | `ack`, then as `action`                         | `ack`, then as `action`                                   | `ack`, then as `action`      |
| Tag blocks (step 1, then the push) | `done` after each block (`ack` before the push) | same as default (always run by you)                       | `rehearsed`, `issue: <text>` |
| `manual` instructions              | check the box on the issue, or `skip <reason>`  | same as default                                           | `rehearsed`, `issue: <text>` |
| Tick command (`gh issue edit`)     | `done` (you ran it), `skip <reason>`            | `run`, `I'll run it`, `skip <reason>`                     | — (diff shown only)          |
| Failed check or probe              | `retry`, `investigate`, `skip <reason>`         | same as default                                           | same as default              |
| Poll timeout (30 min)              | `wait more`, `stop`                             | same as default                                           | same as default              |
| Any time                           | `stop`                                          | `stop`                                                    | `stop`                       |

With `--no-issue-tick`, no tick command is shown in any mode: you check the box on the issue, and the skill polls it before moving on (every minute, up to 8 minutes, then `wait more` or `stop`). The What items of the distribution being built are ticked with the same edit as their last How step, in default and `--allow-run-commands` modes only.

| Answer                        | Effect                                                                             |
| ----------------------------- | ---------------------------------------------------------------------------------- |
| `go`                          | Start the phase                                                                    |
| `ack`                         | Record that the team agreed to the announced `impact` command, which is then shown |
| `done`                        | You ran the shown command, the skill detects its effect (every 30 s, up to 30 min) |
| `run`                         | The skill runs the shown command (`--allow-run-commands` only)                     |
| `I'll run it`                 | You run it instead, same as `done` (`--allow-run-commands` only)                   |
| `skip <reason>`               | Skip the step, logged with the reason                                              |
| `retry` / `investigate`       | After a failure: re-run the check, or start debugging                              |
| `wait more`                   | Keep polling after the timeout                                                     |
| `rehearsed` / `issue: <text>` | Dry run: the step would work, or what would block it                               |
| `stop`                        | Stop the phase, the next run resumes from the issue checkboxes and the log         |

## Dry run

Same flow on the same issue and networks. Checks run for real. Commands are rendered and validated but never run, `ack` is still asked, announcements are printed but not saved, ticks are shown but not applied, the wait computes the target epoch without pausing. It ends with a report saved in the log.

## Files

| File                                                                                               | Content                                                                      |
| -------------------------------------------------------------------------------------------------- | ---------------------------------------------------------------------------- |
| `$MITHRIL_DISTRIBUTION_LOG_DIR/<distribution>-ROLLOUT.md` (default `~/.mithril-distribution/logs`) | Rollout log: runs, steps, commands, `ack`, baselines, waits, dry run reports |
| `$MITHRIL_DISTRIBUTION_LOG_DIR/<distribution>-<phase>-PLAN.md`                                     | Plan of a phase written by `--plan`: every step and its commands             |
| `$MITHRIL_DISTRIBUTION_ANNOUNCEMENTS_DIR/mithril-<distribution>.md` (default: the logs folder)     | Discord and Slack announcement drafts                                        |

## Script help

Each script prints its purpose, rights, requirements, exit codes and usage with `--help`, from `~/.claude/skills/mithril-distribution`:

```bash
bash scripts/resolve-context.sh --help
bash scripts/detect-step.sh --help
bash scripts/wait-step.sh --help
bash scripts/network-check.sh --help
bash scripts/env-commands.sh --help
bash scripts/tag-commands.sh --help
bash scripts/release-notes.sh --help
bash scripts/release-publish.sh --help
bash scripts/release-compatibility.sh --help
bash scripts/client-test-command.sh --help
bash scripts/prover-benchmark.sh --help
bash scripts/secret-date-check.sh --help
bash scripts/kms-rotation-gcloud.sh --help
bash scripts/announcement.sh --help
python3 -I scripts/issue-steps.py --help
```

## Rules

- By default the skill runs no write command, you run what it displays. With `--allow-run-commands`, nothing is published without your `run` on the exact command shown, and one `run` covers one command or one batch (the commands of a step grouped by default, e.g. all the variables of an environment).
- Environment variables and secrets of a phase are changed before its tag push or release publication.
- The skill never connects to GCP, SSH hosts or hosting platforms, and never posts to Discord or Slack.
