# `/mithril-distribution` help

## Usage

```
/mithril-distribution <distribution> <phase> [--dry-run] [--no-issue-tick] [--allow-run-commands]
/mithril-distribution help [<phase>]
```

| Argument | Values |
|---|---|
| `<distribution>` | Distribution number, e.g. `2642`. Used to find the release issue (`Release \`2642\` distribution`), the tags, the rollout log and the announcements file |
| `<phase>` | `prepare` (How "Phase 1"), `pre-release` (How "Phase 2"), `release` (How "Phase 3") |
| `--dry-run` | Rehearse the phase: real read-only checks, no write at all |
| `--no-issue-tick` | No tick command: you check the boxes on the issue, the skill waits for your check before moving on |
| `--allow-run-commands` | Let the skill run the write commands after your `run` (default: it only displays them, you run them and answer `done`) |

Examples: `/mithril-distribution 2642 prepare --dry-run`, `/mithril-distribution 2642 pre-release`, `/mithril-distribution help release`.

## Phases

| Phase | What it does | Deploys |
|---|---|---|
| `prepare` | CI tests, dependencies PR, caches, changelog, draft PRs, announcements preparation | Nothing |
| `pre-release` | Environment configuration, signed tag push, release notes, checks, announcements, 2-epoch wait | `pre-release-preview` (tag push), `pre-release-2-preview` out of the process |
| `release` | KMS rotation, environment configuration, release publication, checks, packages, PR merges, announcements | `release-preprod` and `release-mainnet` (release publication) |

## Steps

The skill reads the unchecked How checkboxes of the phase from the release issue and handles them in order.

| Kind | What the skill does | What you answer |
|---|---|---|
| `check` | Runs a read-only check and shows the result | Nothing, or `retry` / `investigate` / `skip <reason>` on failure |
| `action` | Shows the exact command and the expected result, detects the result | You run it and answer `done` (with `--allow-run-commands`: `run` or `I'll run it`), or `skip <reason>` |
| `impact` | Same as `action`, for operations that change an environment (variables, secrets, tag push, release), shown after your `ack` | `ack` once the team agrees, then as `action` |
| `manual` | Gives the instructions and waits for the checkbox | Check the box on GitHub (or `skip <reason>`) |
| `wait` | Records the target epoch and pauses the phase | Run the same command again after the target epoch |

After each step, the skill shows the checkbox tick as a `gh issue edit` command for you to run (with `--allow-run-commands`, it runs it after your `run`, with `--no-issue-tick`, it waits for your check instead). The same edit ticks the What items of the distribution being built (never the next one) whose How steps are all checked, including steps you checked yourself.

## Answers

| Answer | Effect |
|---|---|
| `go` | Start the phase after the context summary |
| `run` | Run the previewed command (for `impact`, only after `ack`) |
| `ack` | Records that the team agreed to the previewed `impact` command |
| `I'll run it` | You run the command yourself, the skill detects the effect (every 30 s, up to 30 min) |
| `skip <reason>` | Skip the step, logged with the reason |
| `retry` / `investigate` | After a failure: re-run the check, or start debugging |
| `wait more` | Keep polling after the 30 min timeout |
| `rehearsed` / `issue: <text>` | Dry run answers for `manual` steps |
| `stop` | Stop the phase, the next run resumes from the issue checkboxes and the log |

## Dry run

Same flow on the same issue and networks. Checks run for real. Commands are rendered and validated but never run, `ack` is still asked, announcements are printed but not saved, ticks are shown but not applied, the wait computes the target epoch without pausing. It ends with a report saved in the log.

## Files

| File | Content |
|---|---|
| `~/markdowns/mithril/releases/<distribution>-ROLLOUT.md` | Rollout log: runs, steps, commands, `ack`, baselines, waits, dry run reports |
| `~/Desktop/mithril/distributions/mithril-<distribution>.md` | Discord and Slack announcement drafts |

## Rules

- By default the skill runs no write command, you run what it displays. With `--allow-run-commands`, nothing is published without your `run` on the exact command shown, and one `run` covers one command or one batch (the commands of a step grouped by default, e.g. all the variables of an environment).
- Environment variables and secrets of a phase are changed before its tag push or release publication.
- The skill never connects to GCP, SSH hosts or hosting platforms, and never posts to Discord or Slack.
