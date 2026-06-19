# Discontinuing a Signed Entity support

## Context

During the Mithril network lifecycle, support for a signed entity may be discontinued.

Reasons include replacement by a new signed entity, protocol changes, or security concerns.

This document describes the developer process only. Operational aspects must be handled separately.

## Process

> [!IMPORTANT]
> Release each step in a **separate** Mithril distribution to give users time to upgrade and report issues.

### 1. Deprecate support in Mithril Clients

#### Mithril Client Library:

Use the `#[deprecated]` attribute on the signed entity client.

> [!NOTE]
> Use `#[allow(deprecated)]` where needed to suppress warnings in downstream code.

Example from `CardanoImmutableFilesFull` removal:

```rust
#[deprecated(since = "0.12.35", note = "superseded by `CardanoDatabaseClient`")]
pub struct SnapshotClient {
    // omitted
}
```

#### Mithril Client CLI:

Add a deprecation notice to the relevant CLI commands.

Helper function example from `CardanoImmutableFilesFull` removal:

```rust
/// Print in stderr a warning about the deprecation of the v1 backend and its scheduled removal in 2026
pub fn warn_deprecated_v1_backend(context: &CommandContext) {
    let message = "The `v1` backend is deprecated and is scheduled to be removed early 2026. \
    Please use the `v2` backend instead. \
    No other change is required in your command line.";

    print_simple_warning(message, context.is_json_output_enabled());
}
```

#### Mithril Client Wasm library:

Document the deprecation on impacted APIs.

Example from `CardanoImmutableFilesFull` removal:

```rust
/// Call the client to get a snapshot from a digest
///
/// @deprecated superseded by `get_cardano_database_v2_snapshot`
#[wasm_bindgen]
pub async fn get_cardano_database_snapshot(&self, digest: &str) -> WasmResult {
    // omitted
}
```

### 2. Remove support in Mithril Clients

Remove the code deprecated in the previous step. Also remove type aliases, message builders, and adjacent code related
to the discontinued signed entity

Backward compatibility:

- Do not remove the existing Mithril end-to-end client assertion yet. Gate it behind a client version check instead:

```rust
if client.version().is_below("0.13.0") {
    assertions::assert_client_can_verify_snapshot(&mut client, &digest).await?;
}
```

### 3. Remove support in Mithril Aggregator and Mithril Signer

1. Remove the signed entity from the `SignedEntityType` enum and related code.
2. Add a variant with the previous name to `DiscontinuedSignedEntityType`.
3. Update related documentation: website, README files, and API references.

> [!IMPORTANT]
>
> - Add migrations to remove the discontinued signed entity from Mithril Signer and Mithril Aggregator databases.
> - The `DiscontinuedSignedEntityType` variant lets updated nodes discard messages containing the discontinued signed entity without failing.
> - Do not remove tests blindly. First, check whether the discontinued signed entity matters for each test. If it does not, switch the test to another signed entity.

#### Suggested removal strategy

This step can remove several thousand lines. To avoid one large commit, do not remove the `SignedEntityType` variant immediately.

Suggested commit sequence:

1. In the Mithril Aggregator, remove the artifact builder.
2. Replace its call in `MithrilSignedEntityService` with an error:

```rust
match signed_entity_type {
// omitted ..
    // Todo: fully remove CardanoImmutableFilesFull from the SignedEntityType enum.
    SignedEntityType::CardanoImmutableFilesFull(_beacon) => Err(anyhow!("Support for CardanoImmutableFilesFull was removed")),
// omitted ..
}
```

3. Remove code used only by that artifact builder (e.g., `Snapshotter::snapshot_all_completed_immutables` was removed
   when we removed `CardanoImmutableFilesFull`)
4. Repeat the same process for the associated signable builder.
5. Remove related routes from `openapi.yaml`.
6. Remove related routes code from:

- `mithril-aggregator`
- `mithril-aggregator-fake`
- `mithril-aggregator-client`

7. Remove related messages from `mithril-common`.
8. Remove the discontinued signed entity from `SignedEntityType`.
9. Update all remaining code paths.

Additional steps:

- `mithril-build-script`'s `fake_aggregator` module may need updates.
- Update Mithril Aggregator Fake data by running its `import.sh` script. See [README.md](../../../mithril-test-lab/mithril-aggregator-fake/README.md).
