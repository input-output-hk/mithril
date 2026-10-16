# mithril-distribution

A Claude Code skill that walks the rollout of a Mithril distribution (prepare, pre-release, release) step by step, from the How checkboxes of the `Release <distribution> distribution` issue of [IntersectMBO/mithril](https://github.com/IntersectMBO/mithril).

The skill checks the state of each step, renders the exact commands to run, detects their result, keeps a rollout log to resume from, and ticks the issue checkboxes. By default it never runs a write command itself: you run what it displays.

## Requirements

| Tool                                                                                                   | Used for                                                       |
| ------------------------------------------------------------------------------------------------------ | -------------------------------------------------------------- |
| [Claude Code](https://claude.com/claude-code)                                                          | Running the skill                                              |
| `gh` authenticated on `IntersectMBO/mithril` (`gh auth status`) with access to the GitHub environments | Issues, releases, workflows, environment variables and secrets |
| `git` with a GPG signing key (`git config user.signingkey`)                                            | Signed distribution tag                                        |
| `jq`, `curl`, `python3`, GNU `date`, `awk`, `sed`                                                      | Scripts                                                        |
| `ab` (Apache Bench, `apache2-utils` package)                                                           | Prover benchmark                                               |

The skill never connects to GCP, SSH hosts or hosting platforms, and never posts to Discord or Slack.

## Rights

The scripts only read. The commands they print need the following rights on `IntersectMBO/mithril` (role of your GitHub account, and the matching permission of a fine-grained token). ⚠️ marks the commands that need more than the Write role: `env-commands.sh` and `secret-date-check.sh` print a warning when your role is below Admin.

| Command                                                                        | Phase                | Role                                                                                                   | Token permission          |
| ------------------------------------------------------------------------------ | -------------------- | ------------------------------------------------------------------------------------------------------ | ------------------------- |
| Read the issue, releases, workflow runs, aggregators                           | all                  | none (public)                                                                                          | none                      |
| `gh issue edit` (ticks), `gh issue comment` (benchmark)                        | all                  | Triage (edit), any account (comment)                                                                   | Issues write              |
| ⚠️ `gh variable set` / `gh variable delete`, `gh variable list --env`          | pre-release, release | Admin                                                                                                  | Environments write / read |
| ⚠️ `gh secret list --env` (update dates only)                                  | release              | Admin                                                                                                  | Environments read         |
| `git push origin <tag>` (signed with your GPG key, public key on your account) | pre-release          | Write                                                                                                  | Contents write            |
| `gh api …/releases/generate-notes`, `gh release edit`                          | pre-release, release | Write                                                                                                  | Contents write            |
| `gh workflow run test-client.yml`                                              | prepare, pre-release | Write                                                                                                  | Actions write             |
| `gh pr merge`                                                                  | prepare, release     | Write (plus the branch protection rules)                                                               | Pull requests write       |
| `gh cache delete --all`                                                        | prepare              | Write                                                                                                  | Actions write             |
| KMS key rotation commands of `kms-rotation-gcloud.sh`                          | release              | Service Account Key Admin (`roles/iam.serviceAccountKeyAdmin`) on the ancillary signer service account | outside GitHub            |

## Setup

1. Install the skill, either as a project skill (shared with the repository):

```bash
# from a checkout of IntersectMBO/mithril where the skill is merged
ls .claude/skills/mithril-distribution/SKILL.md
```

or as a personal skill (available in every session, independent of the checked out branch):

```bash
mkdir -p ~/.claude/skills
cp -r <mithril checkout>/.claude/skills/mithril-distribution ~/.claude/skills/
```

Use the personal copy while the project skill is not merged on `main`: the Tag step switches the main checkout to `main`, which would remove a project skill that only exists on another branch.

2. Choose where the rollout logs and the announcement drafts are stored, in the `env` block of `~/.claude/settings.json` (defaults: `~/.mithril-distribution/logs` and `~/.mithril-distribution/announcements`):

```json
{
  "env": {
    "MITHRIL_DISTRIBUTION_LOG_DIR": "/path/to/rollout/logs",
    "MITHRIL_DISTRIBUTION_ANNOUNCEMENTS_DIR": "/path/to/announcement/drafts"
  }
}
```

Previous drafts named `mithril-<distribution>.md` in the announcements directory are read as style references for the next ones.

3. Start Claude Code from the main checkout of `IntersectMBO/mithril` (its root is used as `REPOSITORY_DIR`), then check the setup:

```bash
bash ~/.claude/skills/mithril-distribution/scripts/resolve-context.sh <distribution>
```

It prints the issue, tags, commit, versions and the log and drafts locations.

## Usage

```
/mithril-distribution <distribution> <phase> [--dry-run | --plan | --check] [--no-issue-tick] [--allow-run-commands]
/mithril-distribution help [<phase>]
```

| Argument               | Meaning                                                                                                                                          |
| ---------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------ |
| `<phase>`              | `prepare` (How Phase 1), `pre-release` (Phase 2), `release` (Phase 3)                                                                            |
| `--dry-run`            | Rehearse the phase: real read-only checks, no write at all                                                                                       |
| `--plan`               | Print every command of the phase from its first step, in order, then stop, also written to `<distribution>-<phase>-PLAN.md` in the log directory |
| `--check`              | Report each step: done and checked, done not checked, checked not done, not done not checked, then stop                                          |
| `--no-issue-tick`      | No tick command, you check the boxes on the issue and the skill waits for your check                                                             |
| `--allow-run-commands` | Let the skill run the write commands after your `run` (default: it displays them, you run them and answer `done`)                                |

Examples: `/mithril-distribution 2642 pre-release --dry-run`, `/mithril-distribution 2642 release`.

A run starts with the context and the steps of the phase, then waits for `go`. For each step the skill shows one command block and what it waits for: `done` (you ran it), `ack` (team agreement, for commands that change an environment), `run` (with `--allow-run-commands`) or `skip <reason>`. The phase pauses at the 2-epoch wait: run the same command again after the target epoch.

## Scripts

Every script stands alone (explicit arguments, usage in its header) and is read-only: write commands are printed, not run. Only `prover-benchmark.sh run` sends load to the aggregator under test.

| Script                           | Purpose                                                                |
| -------------------------------- | ---------------------------------------------------------------------- |
| `resolve-context.sh`             | Distribution context                                                   |
| `issue-steps.py`                 | How and What scope steps, tick or strike body, `--print-command`       |
| `detect-step.sh`, `wait-step.sh` | Probe a step once, poll it                                             |
| `network-check.sh`               | Network health, baseline, `--deployed-at`, `--epochs`, `--wait-target` |
| `env-commands.sh`                | GitHub environment variable batch, `print` and `verify`                |
| `tag-commands.sh`                | Tag blocks for the user, `check` of the result                         |
| `release-notes.sh`               | Pre-release notes and edit command, `--guard-only`                     |
| `release-publish.sh`             | Release publication command with its guards, `verify`                  |
| `release-compatibility.sh`       | Networks and node compatibility tables of the release notes            |
| `client-test-command.sh`         | Client multi-platform test dispatch with certified hashes              |
| `prover-benchmark.sh`            | Benchmark `run`, `previous` results, `comment`                         |
| `secret-date-check.sh`           | Update date of an environment secret (never reads a value)             |
| `kms-rotation-gcloud.sh`         | `gcloud` commands of the ancillary signer key rotation, run by you     |
| `announcement.sh`                | Raw source of one announcement message                                 |

Example, outside the skill:

```bash
bash scripts/network-check.sh pre-release-preview --expect-commit <sha>
```

## Safety rules

- A pre-release is never marked Latest: `release.yml` runs on `released` and deploys `release-preprod` and `release-mainnet`.
- Environment variables and secrets of a phase are changed before its tag push or release publication, and a baseline of each network is saved right before its deployment.
- The tag is created on `main` synced with `origin/main`, and pushed only after the check of its `git log` line.
- Commands that change an environment need an `ack` first.

## Files

| File                                                                | Content                                                          |
| ------------------------------------------------------------------- | ---------------------------------------------------------------- |
| `SKILL.md`                                                          | Skill instructions                                               |
| `references/catalog.md`                                             | How each step is matched, run and detected                       |
| `references/benchmark.md`                                           | Prover benchmark procedure                                       |
| `references/announcements.md`                                       | Announcement templates                                           |
| `references/help.md`                                                | Output of `/mithril-distribution help`                           |
| `$MITHRIL_DISTRIBUTION_LOG_DIR/<distribution>-ROLLOUT.md`           | Rollout log (runs, steps, commands, baselines, waits)            |
| `$MITHRIL_DISTRIBUTION_LOG_DIR/<distribution>-<phase>-PLAN.md`      | Plan of a phase written by `--plan`: every step and its commands |
| `$MITHRIL_DISTRIBUTION_ANNOUNCEMENTS_DIR/mithril-<distribution>.md` | Discord and Slack drafts                                         |
