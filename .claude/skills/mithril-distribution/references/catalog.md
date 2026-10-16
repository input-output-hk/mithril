# Step catalog

Matching of a How checkbox:

1. Children of a `wait` step are never matched separately: they are handled by the "Wait" section.
2. A child whose text is only a network name (e.g. `` `pre-release-preview` ``) is matched with its parent's entry for that network, except `pre-release-2-preview`, which always gets the `pre-release-2-preview` entry (out of the process).
3. Otherwise, the checkbox gets the first entry, in the order **phase table first, then common entries in the order of this file**, whose **Match** keywords all appear in its text as whole words or phrases (case insensitive, "create" does not match "created").
4. A checkbox that matches no entry is a `manual` step caught on its checkbox.

Variables come from `resolve-context.sh`: `$DISTRIBUTION`, `$ISSUE_NUMBER`, `$TAG` (the tag of the run: `NEXT_TAG` before the tag push, `LATEST_TAG` after), `$COMMIT`, `$PREVIOUS_TAG` (the latest release), `$IMAGE_ID` (`NEXT_IMAGE_ID` or `LATEST_IMAGE_ID`), `$SIGNER_VERSION`, `$CLIENT_LIBRARY_VERSION`, `$CLIENT_WASM_VERSION`, `$PHASE_NUMBER`. `$PROBE` is `bash scripts/detect-step.sh`. `$STEP` is the exact checkbox text printed by `issue-steps.py` (`<parent text> > <child text>` for a child). `$STARTED_AT` is the UTC time at which the step's command was run or handed to the user.

Kinds: `check` (read-only, run directly), `action` (write, needs `run`), `impact` (write that changes an environment, needs `ack` then `run`), `manual` (user does it and checks the box), `wait` (pause until a target epoch).

## Common entries (all phases)

### github-environment-variable

- **Match:** a checkbox (or its children) naming an environment variable in capitals with underscores to set, remove or update, on a GitHub environment (`pre-release-preview`, `release-preprod`, `release-mainnet`)
- **Kind:** `impact`, one batch per environment (one `ack`), rendered by `bash scripts/env-commands.sh print --environment <environment> [set NAME=value]... [delete NAME or pattern]...` (current values table, then the commands), verified by the same call with `verify`
- **Command:** `gh variable set <NAME> --env <environment> --repo IntersectMBO/mithril --body '<value>'` or `gh variable delete <NAME> --env <environment> --repo IntersectMBO/mithril`
- **Before:** show the current value with `gh variable get <NAME> --env <environment> --repo IntersectMBO/mithril` (or "absent"). A name with `*` is expanded against `gh variable list --env <environment> --repo IntersectMBO/mithril`, and the expanded list is shown for confirmation. A value not written in the issue is asked, and a value produced by an earlier step of the run (e.g. the URLs of the on-chain configuration) is taken from the log, where that step recorded it
- **Probe:** `$PROBE env-var <environment> <NAME> '<value>'` or `$PROBE env-var-absent <environment> <NAME>`
- **Order:** must be done before the tag push or the release publication of the phase

### github-environment-secret

- **Match:** a checkbox naming a GitHub secret to add, rotate or remove on a GitHub environment
- **Kind:** `impact`
- **Command:** `gh secret set <NAME> --env <environment> --repo IntersectMBO/mithril < <file the user names>` (never echo the value) or `gh secret delete <NAME> --env <environment> --repo IntersectMBO/mithril`
- **Probe:** `$PROBE env-secret <environment> <NAME>` or `$PROBE env-secret-absent <environment> <NAME>` (a rotation of an existing secret cannot be detected: ask the user to confirm the update, then log it)
- **Order:** before the tag push or the release publication of the phase

### pre-release-2-preview

- **Match:** only through rule 2 (a child whose text is only `pre-release-2-preview`)
- **Kind:** none, this network is not part of the rollout process: the step is skipped with the reason "`pre-release-2-preview` out of the process", never probed, checked, baselined or ticked, and a What scope item about it is neither ticked nor reported as a finding. A parent naming several networks, such as "Make sure `pre-release-preview` and `pre-release-2-preview` are working as expected", is run on the other networks only

### on-chain-configuration

- **Match:** `on-chain` + (`create` or `protocol-config`)
- **Kind:** `manual` (needs signing keys and a funded wallet)
- **Probe:** checkbox, then ask the user for the URLs of the published configuration files (`protocol-config.addr`, `protocol-config.vk`), check each with `$PROBE url-ok <url>`, and record them in the log for the variable steps that need them

### working-as-expected

- **Match:** `make sure` + `working as expected`
- **Kind:** `check`, one run per network named in the checkbox
- **Command:** `bash scripts/network-check.sh <network> --expect-commit $COMMIT --baseline <baseline file> --deployed-at <deployment time> [--since <deployment time>]`, with `--since` in the `release` phase to check the snapshot with ancillary files. The deployment time is the first time the `aggregator-commit` probe of the deploying step reported the new commit (its `wait-step.sh` output line), recorded in that step's log row
- **New artifacts:** a certificate of the current or previous epoch is not enough, since the old aggregator may have produced it. With `--deployed-at`, `CardanoTransactions` or `CardanoBlocksTransactions` must have an artifact created after the deployment (one of them is enough, they are produced within minutes). All the signed entity types are checked by the `wait` step after 2 epochs, on epochs that start after the deployment
- **Pass:** exit `0`. Exit `1` (failure) stops the phase. Exit `3` (no transactions artifact after the deployment yet, no failure): rerun the check in the background every 5 minutes (30 min timeout), the step stays unticked until exit `0`

### grafana

- **Match:** `grafana`
- **Kind:** `manual` (needs the user's authenticated session)

### announce-discord

- **Match:** `announce` + `discord`
- **Kind:** `manual`, the skill drafts the message (see `announcements.md`), the user posts it and checks the box

### announce-slack

- **Match:** `announce` + (`breaking` or `slack`)
- **Kind:** `manual`, the skill drafts the `:fire: @channel` message once for all the listed channels
- **Without breaking change:** struck through and ticked during `prepare` (see "Breaking change announcements" in `SKILL.md`)

### dev-blog-post

- **Match:** `blog post`
- **Kind:** `manual`, the skill lists open PRs touching `docs/website/blog` with `gh pr list --repo IntersectMBO/mithril --search "blog in:title"` as information

## `prepare` (How "Phase 1")

| Id | Match | Kind | Command | Probe |
|---|---|---|---|---|
| npm-token | `npm token` | `manual` | | checkbox |
| manual-restoration | `restoration` | `manual` | See "Restoration" below | checkbox |
| backward-compatibility | `backward compatibility` | `action` | `gh workflow run backward-compatibility.yml --repo IntersectMBO/mithril -f release-to-test=unstable -f e2e-release=unstable` | `$PROBE workflow-run backward-compatibility.yml $STARTED_AT` |
| stress-test | `stress test` | `action` | `gh workflow run aggregator-stress-test.yml --repo IntersectMBO/mithril` | `$PROBE workflow-run aggregator-stress-test.yml $STARTED_AT` |
| client-test | `client multi-platform test` | `action` | `bash scripts/client-test-command.sh --commit <head of main> --image-id unstable --network preview --aggregator https://aggregator.testing-preview.api.mithril.network/aggregator --genesis-key-url https://raw.githubusercontent.com/IntersectMBO/mithril/main/mithril-infra/configuration/testing-preview/genesis.vkey --ancillary-key-url https://raw.githubusercontent.com/IntersectMBO/mithril/main/mithril-infra/configuration/testing-preview/ancillary.vkey --transactions-file <repository>/mithril-test-lab/benchmark/aggregator-prover/transactions-preview.txt --blocks-file <repository>/mithril-test-lab/benchmark/aggregator-prover/blocks-preview.txt` prints the `gh workflow run test-client.yml …` command (the dispatch has no default hashes) | `$PROBE workflow-run test-client.yml $STARTED_AT` |
| dependencies-pr | `upgrade repository dependencies` + `prepare` | `check` | | `gh pr list --repo IntersectMBO/mithril --state all --search "upgrade dependencies in:title"`, the user confirms the PR number |
| reset-caches | `reset the ci` + `caches` | `action` | `gh cache delete --all --repo IntersectMBO/mithril` (run right before the merge of the dependencies PR) | `$PROBE cache-empty` |
| dependencies-merge | `merge the pr to upgrade` | `action` | `gh pr merge <pr> --repo IntersectMBO/mithril --merge` (check CI green and approved first) | `$PROBE pr-merged <pr>` |
| cardano-min-versions | `cardano minimum supported versions` | `manual` | | checkbox |
| draft-prs | `draft pr` | `check` | | the user gives the PR number, `gh pr view <pr> --json isDraft,state` |
| changelog-pr | `changelog` | `check` then `action` for "merge" | `gh pr merge <pr> --repo IntersectMBO/mithril --merge` | `$PROBE pr-merged <pr>`, then `gh api repos/IntersectMBO/mithril/contents/CHANGELOG.md --jq .content \| base64 -d \| grep -m1 "$DISTRIBUTION"` |
| announcements-highlights | `prepare announcements` | `manual` | Full set of drafts in `ANNOUNCEMENT_FILE` (Discord pre-release and release, Slack pre-release and release when breaking changes), see "Announcements" in `SKILL.md` | checkbox |

## `pre-release` (How "Phase 2")

| Id | Match | Kind | Command | Probe |
|---|---|---|---|---|
| tag | `tag and deploy` | `manual` then `impact` (user run) | See "Tag" below | `$PROBE tag $TAG`, then `$PROBE docker-images $IMAGE_ID`, `$PROBE workflow-run pre-release.yml $STARTED_AT $COMMIT`, `$PROBE aggregator-commit pre-release-preview $COMMIT` |
| release-notes | `fill information in github release` | `action` | `bash scripts/release-notes.sh --tag $TAG --previous-tag $PREVIOUS_TAG --commit $COMMIT --highlights <highlights file> --out <notes file>` prints `gh release edit … --prerelease`, then `--guard-only` after the edit. The highlights file is the `Highlights:` bullets of the release message of `ANNOUNCEMENT_FILE`. See "Release notes" below | `$PROBE release-body-contains $TAG '<first highlight line>'`, then `$PROBE release-body-contains $TAG '**Full Changelog**'` and `$PROBE release-body-contains $TAG '## Crates Versions'` |
| npm-next | `npm` + `next` | `check` | | `$PROBE npm-dist-tag @mithril-dev/mithril-client-wasm next $CLIENT_WASM_VERSION` |
| client-test | `client multi-platform test` | `action` | `bash scripts/client-test-command.sh --commit $COMMIT --image-id $IMAGE_ID --network preview --aggregator https://aggregator.pre-release-preview.api.mithril.network/aggregator --genesis-key-url https://raw.githubusercontent.com/IntersectMBO/mithril/main/mithril-infra/configuration/pre-release-preview/genesis.vkey --transactions-file <repository>/mithril-test-lab/benchmark/aggregator-prover/transactions-preview.txt --blocks-file <repository>/mithril-test-lab/benchmark/aggregator-prover/blocks-preview.txt` prints the `gh workflow run test-client.yml …` command | `$PROBE workflow-run test-client.yml $STARTED_AT` |
| prover-benchmark | `prover benchmark` | `action` | `bash scripts/prover-benchmark.sh run …` (printed for the user), then `previous --issue $PREVIOUS_ISSUE_NUMBER --out <csv>` and `comment …`, which prints `gh issue comment …`: see `references/benchmark.md` | `gh issue view $ISSUE_NUMBER --comments` has a `## Cardano transactions prover benchmark` comment posted after `$STARTED_AT`, then the regression verdict |
| wait | `wait for at least` | `wait` | See "Wait" below | See "Wait" below |

## `release` (How "Phase 3")

| Id | Match | Kind | Command | Probe |
|---|---|---|---|---|
| kms-rotation | `kms` or `service account` (except "check that KMS credentials are working") | `manual` | | checkbox |
| kms-check | `kms credentials are working` | `check` | `bash scripts/network-check.sh release-mainnet --since <release time>` | Row "Snapshot with ancillary files" passes |
| release | `release the` + `distribution` | `impact` | `bash scripts/release-publish.sh print --tag $TAG --previous-tag $PREVIOUS_TAG --baseline <release-preprod baseline> --baseline <release-mainnet baseline>` (pre-release, Latest unchanged, baselines saved today) prints `gh release edit $TAG --repo IntersectMBO/mithril --prerelease=false --latest`, the only command that fires `release.yml`. After it, `bash scripts/release-publish.sh verify --tag $TAG` | `$PROBE release-published $TAG`, then `$PROBE workflow-run release.yml $STARTED_AT`, `$PROBE aggregator-commit release-preprod $COMMIT`, `$PROBE aggregator-commit release-mainnet $COMMIT` |
| release-compatibility | `fill information in github release` | `action` | `bash scripts/release-compatibility.sh --tag $TAG --compatible release-preprod,release-mainnet --networks-json <main checkout>/networks.json --repository <main checkout> --out <notes file>` (networks marked ✔, Cardano and DMQ tables regenerated with the repository generators) prints the changed lines and `gh release edit $TAG --repo IntersectMBO/mithril --notes-file <file>`, after the publication | `$PROBE release-body-contains $TAG '| release-mainnet | ✔ |'` and `$PROBE release-body-contains $TAG '| release-preprod | ✔ |'` |
| crates | `crates.io` | `check` | | `$PROBE crate-version mithril-client $CLIENT_LIBRARY_VERSION` |
| npm-latest | `npm` + `latest` | `check` | | `$PROBE npm-dist-tag @mithril-dev/mithril-client-wasm latest $CLIENT_WASM_VERSION` |
| merge-prs | `merge the pr` | `action` | `gh pr merge <pr> --repo IntersectMBO/mithril --merge` (the user confirms the PR number, CI green and approved first) | `$PROBE pr-merged <pr>` |
| roadmap-hydra-node | `roadmap` or `hydra` or `next cardano node release` | `manual` | | checkbox |

## Client test hashes

The `workflow_dispatch` of `test-client.yml` has no default for `transactions_hashes_to_certify` and `blocks_hashes_to_certify`, and an empty value fails every `certify` step ("required arguments were not provided"). Always pass 3 transactions hashes and 3 blocks hashes, comma separated, taken from the first lines of `mithril-test-lab/benchmark/aggregator-prover/transactions-<cardano network>.txt` and `blocks-<cardano network>.txt` (`head -3 <file> | paste -sd,`). Before dispatching, check they are certified by the aggregator under test: `non_certified_transactions` of `/proof/cardano-transaction?transaction_hashes=<hashes>` and `non_certified_blocks` of `/proof/v2/cardano-block?block_hashes=<hashes>` are empty, otherwise take the next lines.

## Restoration

Run by the user on their machine, on `testing-preview` (built from `main`) with the `unstable` client. Before printing the blocks, check that `https://aggregator.testing-preview.api.mithril.network/aggregator/artifact/cardano-database` serves a snapshot of the current or previous epoch, and print its size (`total_db_size_uncompressed`): the work folder needs about twice that free space. Each block runs in the same shell.

1. Work folder (on disk, not in a `tmpfs` `/tmp`) and client:

```bash
export RESTORE_DIR=$(mktemp -d /var/tmp/mithril-restore-XXXXXX)
cd $RESTORE_DIR
df -h $RESTORE_DIR
curl --proto '=https' --tlsv1.2 -sSf https://raw.githubusercontent.com/IntersectMBO/mithril/refs/heads/main/mithril-install.sh | sh -s -- -c mithril-client -d unstable -p $RESTORE_DIR
./mithril-client -V
```

2. Environment:

```bash
export CARDANO_NETWORK=preview
export AGGREGATOR_ENDPOINT=https://aggregator.testing-preview.api.mithril.network/aggregator
export GENESIS_VERIFICATION_KEY=$(curl -sSf https://raw.githubusercontent.com/IntersectMBO/mithril/main/mithril-infra/configuration/testing-preview/genesis.vkey)
export ANCILLARY_VERIFICATION_KEY=$(curl -sSf https://raw.githubusercontent.com/IntersectMBO/mithril/main/mithril-infra/configuration/testing-preview/ancillary.vkey)
export CARDANO_NODE_VERSION=$(curl -sSf $AGGREGATOR_ENDPOINT/status | jq -r .cardano_node_version)
export SNAPSHOT_DIGEST=latest
```

3. Restoration with ancillary files:

```bash
./mithril-client cardano-db snapshot show $SNAPSHOT_DIGEST
./mithril-client cardano-db download --include-ancillary --download-dir $RESTORE_DIR $SNAPSHOT_DIGEST
```

Pass: the client ends with "Immutable files have been successfully verified with Mithril" and `$RESTORE_DIR/db` exists.

4. Restart of a Cardano node on the restored database (own container name and bind mounts, so no existing `cardano-node-*` volume is touched):

```bash
mkdir -p $RESTORE_DIR/ipc $RESTORE_DIR/data
docker run -d --name mithril-restore-node -v $RESTORE_DIR/ipc:/ipc -v $RESTORE_DIR/data:/data --mount type=bind,source=$RESTORE_DIR/db,target=/data/db/ -e NETWORK=$CARDANO_NETWORK ghcr.io/intersectmbo/cardano-node:$CARDANO_NODE_VERSION
timeout 900 sh -c 'docker logs -f mithril-restore-node 2>&1 | grep -m1 "Chain extended"'
```

Pass: a "Chain extended" line within 15 minutes, without a ledger replay from genesis (the ancillary ledger snapshot is used).

5. Cleanup (the node writes as root in the bind mounts):

```bash
docker rm -f mithril-restore-node
cd ~ && sudo rm -rf $RESTORE_DIR
```

The user reports the client version, the snapshot digest and the time to the first "Chain extended" line. The skill logs them in the step row, then waits for the checkbox.

## Tag

Rendered by `bash scripts/tag-commands.sh print --tag $TAG --commit $COMMIT`, checked after step 1 by `bash scripts/tag-commands.sh check --tag $TAG --commit $COMMIT --repository <main checkout>`. Run by the user, in two steps, in the main checkout of `IntersectMBO/mithril` (`origin` = `git@github.com:IntersectMBO/mithril.git`). Print each block exactly as below with the resolved values, the user runs it and answers `done`. Never run these commands, never change their order or content.

Step 1, create the tag on `main`:

```bash
DISTRIBUTION_VERSION=$DISTRIBUTION
PATCH_VERSION=<patch of $TAG>
COMMIT_ID=$COMMIT

git switch main
git pull --rebase
git tag -s $DISTRIBUTION_VERSION.$PATCH_VERSION $COMMIT_ID -m "$DISTRIBUTION_VERSION.$PATCH_VERSION release"
git log --oneline --decorate -1
```

Check after `done`: the `git log` line carries `HEAD -> main`, `origin/main` and `tag: $TAG` on `$COMMIT`. Otherwise stop: `$COMMIT` is not the head of `origin/main`, confirm the commit again with the user.

Step 2, push the tag (`impact`, deploys `pre-release-preview` through `pre-release.yml`, `ack` first). Prepared only when every `github-environment-variable` and `github-environment-secret` step of the phase is checked or skipped:

```bash
git push origin $DISTRIBUTION_VERSION.$PATCH_VERSION
```

Then the probes of the `tag` entry run in the background.

## Release notes

The `pre-release.yml` workflow creates the release with its own part (`## Crates Versions` to `## Download a Docker image`). The body is completed on top of it, in this order (see the previous releases, e.g. `gh release view $PREVIOUS_TAG --json body --jq .body`):

1. `## Highlights`: the `- **Topic**: sentence.` bullets of the announcement drafts (`ANNOUNCEMENT_FILE`), drafted from the What scope and the `CHANGELOG.md` entry.
2. `## What's Changed`, `## New Contributors` and `**Full Changelog**`: the PRs merged since the latest release (`$PREVIOUS_TAG`), generated by GitHub with `gh api --method POST repos/IntersectMBO/mithril/releases/generate-notes -f tag_name=$TAG -f previous_tag_name=$PREVIOUS_TAG -f target_commitish=$COMMIT --jq .body`. This call creates nothing and is not publishing (user decision): run it directly while preparing the step, in a dry run too, without asking.
3. The current body (`gh release view $TAG --repo IntersectMBO/mithril --json body --jq .body`), unchanged.

A part already in the current body (`## Highlights`, `## What's Changed`) is not added twice. The file is written to the scratchpad, shown in full with its diff against the current body, then `gh release edit $TAG --repo IntersectMBO/mithril --notes-file <file> --prerelease` waits for `run`.

Never mark a pre-release as "Latest": `release.yml` runs on `release: types: [released]`, so a release that stops being a pre-release deploys `release-preprod` and `release-mainnet`. In the `pre-release` phase, every `gh release edit` of `$TAG` carries `--prerelease` and never `--prerelease=false` nor `--latest`, and is refused otherwise. Before and after each edit, check `gh release view $TAG --repo IntersectMBO/mithril --json isPrerelease` is `true` and `gh release view --repo IntersectMBO/mithril --json tagName` (the Latest release) is still `$PREVIOUS_TAG`. A failed check stops the phase. Only the `release` entry of phase 3 removes the pre-release flag.

## Wait

When the phase reaches the `wait` step:

1. `bash scripts/network-check.sh <network> --wait-target <deployment epoch> 2` per network: target epoch, its start date and the check command for the resume (exit `3` while not reached).
2. Log `paused until epoch <target>` per network in the run section, and stop the phase.

On resume, if a target epoch is not reached, show the epochs left and stop. Otherwise run `bash scripts/network-check.sh <network> --epochs <start> <target> --baseline <baseline file>` for each network, where `<start>` is the first epoch that starts after the deployment (deployment epoch + 1), so every signed entity type is checked on artifacts of the new aggregator. The "certificates are created" child is checked by this run. The "previous signers" and "new signers" children stay `manual` (signer versions are only on the dashboard, read with `/browse` only when the user asks).
