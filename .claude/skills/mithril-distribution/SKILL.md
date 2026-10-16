---
name: mithril-distribution
description: Use when preparing, pre-releasing or releasing a Mithril distribution (e.g. "/mithril-distribution 2642 pre-release", "rehearse the release", "deploy the pre-release", "tag the distribution", "help on the distribution skill"), following the How checkboxes of the "Release `<distribution>` distribution" issue of IntersectMBO/mithril.
---

# Mithril distribution rollout

Walks one phase of a Mithril distribution rollout step by step with the user, using the release issue How checkboxes as the plan, the step catalog to know how to run and detect each step, and a local log to resume.

**By default the skill runs no write: it displays the exact command or batch, the user runs it and the skill detects the result. With `--allow-run-commands`, writes happen only on the user's `run` for the exact command or batch shown. Impactful writes also need the team's `ack` first, in both modes. Violating the letter of these rules is violating their spirit.**

## Arguments

`/mithril-distribution <distribution> <phase> [--dry-run] [--no-issue-tick] [--allow-run-commands]` or `/mithril-distribution help [<phase>]`.

- `--allow-run-commands`: the skill may run the write commands (`action`, `impact`, ticks, comments) after the user's `run`. Without it (default), every write command is only displayed and run by the user. Read-only checks and probes always run. The Tag blocks are always run by the user.
- `--no-issue-tick`: no tick command is prepared. Once a step is done, the skill polls its checkbox (`wait-step.sh --interval 60 --timeout 480 checkbox …`, in the background) and moves on only when the user has checked it on the issue.

- `help`: print `references/help.md` (no other call). `help <phase>`: also print one table of the catalog entries that apply to the phase (the phase table, then the common entries), with match, kind, command template, probe and `ack` needed, followed by the ordering rules. No script, no network or GitHub call.
- `<phase>`: `prepare` = How "Phase 1", `pre-release` = "Phase 2", `release` = "Phase 3".
- Anything else or a missing distribution: print the usage block of `references/help.md` and point to `help`.

Scripts live in this skill's `scripts/` folder and are always called through their interpreter: `bash scripts/<name>.sh`, `python3 -I scripts/issue-steps.py`. Each one is usable on its own (explicit arguments, usage in its header). None writes to GitHub: they print the write commands. Only `prover-benchmark.sh run` loads the aggregator under test.

| Script | Use |
|---|---|
| `resolve-context.sh` | Distribution context |
| `issue-steps.py` | How and What scope steps, tick or strike body, `--print-command` |
| `detect-step.sh`, `wait-step.sh` | Probe a step once, poll it in the background |
| `network-check.sh` | Network health, baseline, `--deployed-at`, `--epochs`, `--wait-target` |
| `env-commands.sh` | Environment variable batch, `print` and `verify` |
| `tag-commands.sh` | Tag blocks for the user, `check` of the result |
| `release-notes.sh` | Pre-release notes file and edit command, `--guard-only` |
| `client-test-command.sh` | Client test dispatch command with certified hashes |
| `prover-benchmark.sh` | Benchmark `run`, `previous` results, `comment` |
| `announcement.sh` | Raw source of one announcement message, `--list` |

## Output

Be brief: per step, one line of state, the command block, and what is awaited (`run`, `ack`, `done`). Before every command the skill runs itself, even a read-only check or probe, show it as a one-line `bash` block (the script call with its arguments), never only a description. No recap of earlier steps, no restated rules, tables only for check results. Paths are given once, when a file is first produced.

Stay responsive: one tool call per step where possible (probe, render and log chained in one `bash` call), background polls started in the same message as the instructions, no re-reading of files or issue sections already in context, and no extra verification beyond the step's probe.

## Start of a run

1. `bash scripts/resolve-context.sh <distribution> [--commit <sha>] [--patch <n>]`. If `ISSUE_NUMBER` is empty, ask the user to pick from `ISSUE_CANDIDATES`.
2. Choose `$TAG`, `$COMMIT`, `$IMAGE_ID`: before the tag is pushed (`prepare`, `pre-release` until the tag step), use `NEXT_TAG`, `COMMIT` (head of `main` unless the user names one) and `NEXT_IMAGE_ID`. After it (later `pre-release` steps, `release`), use `LATEST_TAG`, `LATEST_TAG_COMMIT` and `LATEST_IMAGE_ID`. Show `git log --oneline -5 <commit>` and get the user's confirmation of tag and commit.
3. Read the log `LOG_FILE` if it exists: last real run of the phase (baselines, `paused until epoch`), earlier dry run reports.
4. `python3 -I scripts/issue-steps.py <issue> <phase number>`, and `python3 -I scripts/issue-steps.py <issue> --scope <distribution>`.
5. Classify each How step of the phase by comparing its text with the same phase of `PREVIOUS_ISSUE_NUMBER` and `OLDER_ISSUE_NUMBER` (`python3 -I scripts/issue-steps.py <issue> <phase number>` on both). Texts are compared after replacing distribution numbers, tags and image ids (`2642`, `2642.0`, `26XX.0`, `2617.0-2478748`) with a placeholder and collapsing spaces:
   - **recurring**: present in both previous issues (standard rollout step, never flagged)
   - **copied**: present in the previous issue only, so a one-off step of the previous release copied over (flagged, unless it clearly serves a What scope item)
   - **specific**: absent from the previous issue (release specific, must serve a What scope item)
6. Print: the context table, the phase steps with their state (🟢 done, 🔴 open), class, matched catalog entry and kind, and the scope findings:
   - **copied** steps, and **specific** steps that serve no What scope item
   - What scope items with no **specific** step in any phase of the How
   - Leftover placeholders (`26XX`, `XXXX`) and image ids, versions, branches or tags of the previous distribution
   - What scope items to tick: unchecked items of `### Distribution <distribution>` whose How steps are all checked (see "What scope ticks"), proposed as one edit before `go`
   - `prepare` only: the breaking change verdict and, without breaking change, the strike of the breaking change announcements (see "Breaking change announcements"), proposed as one edit before `go`
7. Append a run section to the log, marked `real` or `dry run` (create the file with its header the first time), then wait for `go`.

## Step lifecycle

For each unchecked step of the phase, in order (a parent with children is done child by child, then ticked):

1. **Announce**: checkbox text, catalog entry, kind, definition of done (probe).
2. **Probe first**: run the probe once with `bash scripts/detect-step.sh <probe> <args>`. Exit `0`: already done, go to 6.
3. **Prepare**: for `action` and `impact`, render the exact command with the resolved values (by default one batch: all the commands of the step, or of its children, that share the same kind, in one block run in order and stopped at the first failure, e.g. the variables of an environment), plus the current state and the expected state. For `manual`, give the instructions and the checkbox awaited. Values not in the issue or the context are asked, never guessed silently.
4. **Answer**: default, display the command and await `done` (the user ran it) or `skip <reason>`. With `--allow-run-commands`: `run`, `I'll run it` or `skip <reason>`. For `impact`, the command is displayed only after the user has typed `ack` for this command or batch, and the `ack` time is recorded. A `manual` step needs no answer: go to 5 right after the instructions.
5. **Detect**: start `bash scripts/wait-step.sh <probe> <args>` with `run_in_background` (30 s interval, 30 min timeout) right after displaying the command (default), after running it (`--allow-run-commands`) or after `I'll run it`, and keep talking with the user meanwhile. For a `manual` step, start `bash scripts/wait-step.sh --interval 60 --timeout 480 checkbox <issue> <phase number> '<step text>'` with `run_in_background` in the same message as the instructions, never wait for the user to say it is done. The step text is the exact checkbox text printed by `issue-steps.py` (not its index, which shifts when the How is edited), and `'<parent text> > <child text>'` for a child. Exit `0` done, `3` failed (stop on the step), `4` timeout (ask `wait more` or `stop`), `2` probe error (show it, ask: the text was probably edited, list the steps again and restart the poll with the new text).
6. **Verify**: run the follow-up probes and checks of the catalog entry.
7. **Tick** (with `--no-issue-tick`: no command, start the checkbox poll of the step in the background and wait for the user's check, What scope items to tick are only listed): list the steps and the What scope again and take the current indexes of the texts, then `python3 -I scripts/issue-steps.py <issue> <phase number> --tick <indexes> --scope <distribution> --scope-tick <scope indexes> --out <scratchpad file> --print-command` (one body for both, see "What scope ticks"), which prints the checked lines and the command. Show only the lines that get checked (`- [x] <text>`, no diff, no file path, no blank line note) and the command `gh issue edit <issue> --repo IntersectMBO/mithril --body-file <file>`, run by the user (default) or by the skill after `run` (`--allow-run-commands`), then detected by the checkbox probe. Group the ticks of consecutive done steps in one edit. Re-read the issue right before writing, to never overwrite a concurrent edit. A step checked by the user needs no How tick, but its What scope ticks are still proposed (with `python3 -I scripts/issue-steps.py <issue> --scope <distribution> --scope-tick <scope indexes> --out <scratchpad file>`).
8. **Log**: append the step row to the run section.

A failed check or probe stops the phase on the step: show the output, then `retry`, `investigate` or `skip <reason>`.

## What scope ticks

The What section lists the scope of `### Distribution <distribution>` and of the next distribution (`### Distribution <distribution>+1`). Only the items of `### Distribution <distribution>` are ever ticked.

- A What item is ticked when every How step serving it, in any phase, is checked (e.g. the DMQ `pre-release-preview` item with the Phase 2 child, the `release-mainnet` item only with its Phase 3 child).
- A What parent is ticked when all its children are checked.
- An item with no serving How step (placeholders such as `TBD` or `Distribution XXXX`) is never ticked, it is reported as a scope finding.
- An item is never unticked, and a match that is not obvious is asked to the user, never guessed.

## Ordering rules

- Every `github-environment-variable` and `github-environment-secret` step of the phase is checked or skipped with a reason before the tag push (`pre-release`) or the release publication (`release`) is prepared. Refuse to prepare those commands otherwise.
- Save a baseline of each network right before the step that deploys it: `bash scripts/network-check.sh <network> --save-baseline <log dir>/<distribution>-<network>-baseline.json`. The tag push deploys `pre-release-preview`, the release publication deploys `release-preprod` and `release-mainnet`. A dry run saves baselines too, with a `-dry-run` suffix.
- Networks: `pre-release` checks `pre-release-preview`, `release` checks `release-preprod` and `release-mainnet`. `pre-release-2-preview` is not part of the process (see its catalog entry).
- The wait step pauses the phase (see "Wait" in `references/catalog.md`). There is no polling loop for it, the user resumes the phase after the target epoch.

## Announcements

The `prepare` announcements step (`announcements-highlights`) drafts the full set of the distribution in `ANNOUNCEMENT_FILE`, as in the files of `ANNOUNCEMENT_STYLE_FILES`: the pre-release and release Discord messages and, when the distribution has breaking changes (see "Breaking change announcements"), the pre-release and release Slack messages. Each message is written in Markdown as it will be posted (no code fence), from the templates of `references/announcements.md`, reading the style files for wording only. Highlights are drafted from the What scope and the `CHANGELOG.md` entry, then edited with the user. Emojis are written as Discord and Slack shortcodes (`:zap:`, `:rocket:`, `:fire:`, `:slight_smile:`), never as Unicode characters. A message is always printed to the user as its raw Markdown source inside a ```` ```markdown ```` code block (never rendered, the terminal would drop the bold, code spans and links), so it can be copied as is. For `announce-*` steps of later phases and on request, print the matching message of `ANNOUNCEMENT_FILE` with `bash scripts/announcement.sh --file <file> --message <n>` (`--list` to find its number) (draft and append it first if it is missing, not in a dry run). The user posts it and checks the box.

## Breaking change announcements

During `prepare`, look for breaking changes in the `<distribution>.0` entry of `CHANGELOG.md` (on the CHANGELOG PR branch while it is open, on `main` after) and in the description of the CHANGELOG PR: `**REMOVED**`, `BREAKING` or `breaking change` (case insensitive).

- Breaking change found: print the matching lines, the `Announce any breaking change in:` steps stay.
- None found: propose to strike through and tick every `Announce any breaking change in:` checkbox and its children, in every phase (a "breaking/new feature" announcement is kept). Render it with `python3 -I scripts/issue-steps.py <issue> <phase number> --strike <indexes> --out <scratchpad file>` (one phase per call, chained with `--in <previous file>`), show the diff, then `gh issue edit` after `run` as for a tick. The verdict is asked to the user when the match is not obvious, and logged.

## Dry run

Same flow, same issue, networks and log file (its run section is marked `dry run` and never changes the state of a real run), with these differences:

| Kind | Dry run |
|---|---|
| `check` | Run for real |
| `action`, `impact` | Render and validate, never run. `ack` is still asked |
| `manual` | Instructions, then `rehearsed` or `issue: <text>` |
| `wait` | Target epochs and dates computed, no pause recorded |
| Announcements | Printed, not appended |
| Tick (How and What scope) | Diff shown, not applied |

Validation without running: tag (Tag blocks rendered, commit equal to the head of `origin/main`, `$PROBE tag` pending, `git config user.signingkey` set and its secret key present), variables and secrets (environment readable with `gh variable list --env`, variable to delete present, value to set provided), `gh workflow run` (workflow exists, inputs accepted by its `workflow_dispatch`), `gh release edit` (pre-release edits carry `--prerelease` and no `--latest`, release exists or created by the tag step, release notes generated from `$PREVIOUS_TAG` to `$COMMIT` and the notes file rendered without the workflow part when the release does not exist yet), `gh pr merge` (PR open, CI green, approved). End with a report (steps that would run, validation failures, manual issues, scope findings) printed and appended to the log.

## Log format

`LOG_FILE` (create `~/markdowns/mithril/releases/` if missing), each paragraph on one line:

```markdown
# Distribution <distribution> rollout

| Item | Value |
|---|---|
| Issue | #<issue> |
| Tag | `<tag>` |
| Commit | `<sha>` <title> |
| Image id | `<image id>` |
| Signer | `<version>` |
| Previous distribution | `<previous tag>` |

## <phase> (<real|dry run>) <UTC start time>

| Step | Kind | Command | Run by | Ack | Result | Duration |
|---|---|---|---|---|---|---|

### Baselines
### Wait
### Dry run report
```

The step result is 🟢 `done`, 🟡 `skipped <reason>`, 🔴 `failed`, or ⚪ `rehearsed` / ⚪ `issue: <text>` in a dry run. Results of `network-check.sh` keep its markers (🟢 pass, 🔴 fail, ⚪ info or n/a).

## Red flags (stop)

| Thought | Reality |
|---|---|
| "The user said go, so I can run the commands of the phase" | `go` starts the phase. Without `--allow-run-commands` the skill runs no write at all, with it each write needs its own `run`. |
| "The ack was given for the previous variable" | One `ack` covers one previewed `impact` command or batch. |
| "It is only a checkbox tick" | Editing the issue is publishing: diff, command, `run`. |
| "A dry run can dispatch the terraform plan workflow" | A dispatch is a write. A dry run makes no write. |
| "I will set the variable after the tag, it is faster" | Environment configuration always comes before the deployment. |
| "I can create or push the tag myself, it is faster" | The user runs the two Tag blocks exactly as written: `git switch main`, `git pull --rebase`, `git tag -s`, `git log` check, then the push. |
| "Editing the pre-release notes cannot deploy anything" | Unsetting the pre-release flag or marking it Latest fires `release.yml` on `release-preprod` and `release-mainnet`. Always `--prerelease`, never `--latest` before phase 3. |
| "The latest certificates are recent, so the new aggregator works" | They may come from the old aggregator. `CardanoTransactions` or `CardanoBlocksTransactions` needs an artifact created after the deployment (`--deployed-at`), all the types are checked after 2 epochs. |
| "The probe is pending but the user probably did it" | Only the probe or the checkbox ends a step. |
| "The user will tell me when the manual step is done" | The checkbox poll starts with the instructions. The user only checks the box. |
| "I can read the KMS key or terraform state to check" | Never GCP, SSH or hosting platforms. Use the public API or ask the user. |
