# Announcement templates

Templates extracted from the `mithril-*.md` drafts of `ANNOUNCEMENTS_DIR`. Read the files listed in `ANNOUNCEMENT_STYLE_FILES` for the current wording before drafting, but fill every value from the resolved context: these files are copies of each other and carry stale blocks (old warnings, reused Discord links).

Placeholders: `<TAG>` (e.g. `2642.0`), `<DISTRIBUTION>` (e.g. `2642`), `<SIGNER_VERSION>`, `<HIGHLIGHTS>` (bullets `- **Topic**: sentence.` drafted from the What scope and the `CHANGELOG.md` entry, edited by the user), `<TIMING>` (asked, e.g. "expected early next week").

Drafts are printed, then appended to `ANNOUNCEMENT_FILE` with a blank line, the separator `-------------------------------` and a blank line before each new message (not in a dry run). The fences below only delimit the templates: messages are written in the file as plain Markdown, without fence. The `prepare` phase writes the full set (pre-release and release Discord messages, then the pre-release and release Slack messages when there are breaking changes), each Slack pair kept next to each other.

## Pre-release (Discord)

```
@here **The pre-release of the new Mithril distribution `<DISTRIBUTION>` is available** :zap:

We have created a new `<TAG>` distribution in **pre-release** (https://github.com/IntersectMBO/mithril/releases/tag/<TAG>)

Highlights:
<HIGHLIGHTS>

If you are running a **Mithril signer** on:
- the **pre-release-preview** network: you can upgrade the binary of your signer node to the version `<SIGNER_VERSION>`, no configuration update is needed.
- the **release-preprod** network: there is nothing to do at the moment, we will provide an update and instructions when the distribution is released (<TIMING>)
- the **release-mainnet** network: there is nothing to do at the moment, we will provide an update and instructions when the distribution is released (<TIMING>)

You can **easily update your Mithril signer with this one line command** (it will be downloaded in the current directory, a custom folder can be specified with `-p` option):
`curl --proto '=https' --tlsv1.2 -sSf https://raw.githubusercontent.com/IntersectMBO/mithril/refs/heads/main/mithril-install.sh | sh -s -- -c mithril-signer -d <TAG> -p "$(pwd)"`

Let us know if everything works fine on your node, and feel free to ask us any questions on #general-mithril :slight_smile:.
```

When a signer configuration change is needed, replace "no configuration update is needed" with the change and a link to the documentation.

## Updated pre-release (Discord, on request)

```
@here **Updated pre-release `<TAG>` of the new Mithril distribution is available** :warning:

:warning: We have noticed <PROBLEM> following the creation of the `<PREVIOUS_TAG>` pre-release deployed in `pre-release-preview`.

We have created a new `<TAG>` distribution in **pre-release** (https://github.com/IntersectMBO/mithril/releases/tag/<TAG>) that fixes the problem.
If you have already upgraded your signer node to the `<PREVIOUS_TAG>` distribution, you will also need to upgrade to `<TAG>` distribution.
```

Followed by the "If you are running a **Mithril signer** on" block, the install command and the closing line of the pre-release template.

## Signer adoption reminder (Discord, on request)

```
@here, the pre-release of the new Mithril distribution `<DISTRIBUTION>` has been released last week (<LINK TO THE PRE-RELEASE MESSAGE, asked to the user>).
Before we can release it, we would need more signers running it by upgrading their Mithril signer node to version `<SIGNER_VERSION>` on the `pre-release-preview` network :slight_smile:.
```

## Release (Discord)

```
@here **The new Mithril distribution `<TAG>` is available** :rocket:

We have released the `<TAG>` distribution (https://github.com/IntersectMBO/mithril/releases/tag/<TAG>)

Highlights:
<HIGHLIGHTS>

This new distribution has been deployed to the **Mithril aggregator** of the `release-mainnet` and `release-preprod` networks.

If you are running a **Mithril signer** on:
- the **pre-release-preview** network: you can upgrade the binary of your signer node to the version `<SIGNER_VERSION>`, no configuration update is needed.
- the **release-preprod** network: you can upgrade the binary of your signer node to the version `<SIGNER_VERSION>`, no configuration update is needed.
- the **release-mainnet** network: you can upgrade the binary of your signer node to the version `<SIGNER_VERSION>`, no configuration update is needed.

You can **easily update your Mithril signer with this one line command** (it will be downloaded in the current directory, a custom folder can be specified with `-p` option):
`curl --proto '=https' --tlsv1.2 -sSf https://raw.githubusercontent.com/IntersectMBO/mithril/refs/heads/main/mithril-install.sh | sh -s -- -c mithril-signer -d <TAG> -p "$(pwd)"`

Let us know if everything works fine on your node, and feel free to ask us any questions on #general-mithril :slight_smile:.
```

## Hotfix (Discord, on request)

```
@here **The hotfix Mithril distribution `<TAG>` is available** :fire:

We have released the hotfix `<TAG>` distribution (https://github.com/IntersectMBO/mithril/releases/tag/<TAG>)

This version fixes a bug in the `<PREVIOUS_TAG>` distribution which **<BUG IMPACT>**.
More information is available in this blog post: <BLOG POST URL>

Make sure you are using the **Mithril signer** with version `<SIGNER_VERSION>`.
```

Followed by the install command and the closing line.

## Signer problem warning (Discord, on request)

```
:warning: @here, we have noticed a problem with the Mithril signer `<SIGNER_VERSION>`, released with the distribution `<TAG>`, which **<IMPACT>**.

We are currently working on a **hotfix** for the signer which will be released <WHEN>.

In the mean time, we strongly recommend to **not update** to the version `<SIGNER_VERSION>`. In case of problem with your signer, you can **safely rollback** to the previous stable version `<ROLLBACK_SIGNER_VERSION>`.

Feel free to ask us any questions :slight_smile:.
```

## Breaking change (Slack, `pre-release` phase)

```
:fire: @channel, we will introduce a **breaking change** in the **<COMPONENTS>** in the next Mithril distribution:
  - <CHANGE AND WHO MUST ACT>
- The **pre-release** distribution `<TAG>` is now [available](https://github.com/IntersectMBO/mithril/releases/tag/<TAG>) for testing this breaking change.
- The **release** distribution `<TAG>` is <TIMING>.
```

## Breaking change (Slack, `release` phase)

```
:fire: @channel, we have introduced a **breaking change** in the **<COMPONENTS>** in the new `<TAG>` Mithril distribution:
  - <CHANGE AND WHO MUST ACT>
The new distribution `<DISTRIBUTION>` is now [released](https://github.com/IntersectMBO/mithril/releases/tag/<TAG>) with this breaking change.
```

Use "breaking changes" and plural verbs when several changes are listed. When the release has no breaking change, say so to the user and draft nothing for Slack.
