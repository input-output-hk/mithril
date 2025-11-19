# Backward compatibility

The end-to-end tests scenarios are backward compatible with previous nodes versions.

This is achieved by differentiating the scenarios based on the version of the nodes they are testing.

There are multiple ways to do this:
- disabling part of the scenarios 
- adding or removing arguments to the binaries (i.e. `--backend` flag for the client-cli)

---

## Supported changes

List of breaking changes that are supported by the end-to-end tests.

format is: `- since 'X.Y.Z' (distribution version) [to 'X.Y.Z' (distribution version) (optional)]: supported change`

### Mithril client

- since `0.12.34` (2543.0): test of new `--epoch` filter to `cardano-db list` (disabled on lower versions)
- since `0.12.11` (2524.0): removal of `cardano-db-v2` replaced with `cardano-db [command] --backend [v1,v2]`
- since `0.12.0` (2517.0): addition of global `--origin-tag` parameter
- since `0.12.0` (2517.0): addition of `--include-ancillary` flag to `cardano-db download`

### Mithril aggregator

- since `0.7.94` (next to 2543.1): only the leader aggregator must be restarted when updating protocol parameters

### Mithril signer

---

## Not supported changes

List of breaking changes that are NOT supported by the end-to-end tests, running a node with those versions against
`unstable` will result in an error.

format is: `- 'X.Y.Z' (distribution version) with the below distributions: unsupported change`

### Mithril client

### Mithril aggregator

- `0.7.91` (next to 2543.1) with the below distribution: new `/protocol-configuration/{epoch}` aggregator route to update network parameters (required by signer `0.2.274` and up)

### Mithril signer

- `0.2.221` (2450.0) with the below distributions: addition of the `CardanoDatabase` signed entity type, previous signers
  are not able to handle unknown signed entities