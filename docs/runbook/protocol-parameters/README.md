# Update the protocol parameters of a Mithril network

## Introduction

The protocol parameters of a network are currently defined when starting the aggregator of the network.
During startup, the aggregator will store the parameters in its stores, and will use them **3** epochs later. The protocol parameters are broadcasted by the aggregator to the signers of the network through the `/epoch-settings` route.

## Update parameters of a Mithril network
The aggregator has the following configuration parameter used to set the protocol parameters: `protocol_parameters` which is a JSON representation of the `ProtocolParameter` type:
```bash
pub struct ProtocolParameters {
    /// Quorum parameter
    pub k: u64,

    /// Security parameter (number of lotteries)
    pub m: u64,

    /// f in phi(w) = 1 - (1 - f)^w, where w is the stake of a participant
    pub phi_f: f64,
}
```

Each parameter can also be set via an environment variable:
- `PROTOCOL_PARAMETERS__K` for `k`
- `PROTOCOL_PARAMETERS__M` for `m`
- `PROTOCOL_PARAMETERS__PHI_F` for `phi-f`

When setting up a Mithril network with a `terraform` deployment, the protocol parameters are set with a JSON definition.

## Find the workflow used to deploy a Mithril network

Currently, the following [Mithril networks](https://mithril.network/doc/manual/developer-docs/references#mithril-networks) are generally available, and deployed with `terraform`:
- `testing-preview`: with the workflow [`.github/workflows/ci.yml`](../../github/workflows/ci.yml)
- `pre-release-preview`: with the workflow [`.github/workflows/pre-release.yml`](../../github/workflows/pre-release.yml)
- `release-preprod`: with the workflow [`.github/workflows/release.yml`](../../github/workflows/release.yml)
- `release-mainnet`: with the workflow [`.github/workflows/release.yml`](../../github/workflows/release.yml)

## Update the protocol parameters

Update the following value of the targeted network in the deployment matrix with the new values that need to be used:
```bash
mithril_protocol_parameters: |
    {
        k     = 5
        m     = 100
        phi_f = 0.6
    }
```

Which will be replaced eg with:
```bash
mithril_protocol_parameters: |
    {
        k     = 2422
        m     = 20973
        phi_f = 0.2
    }
```

The modifications should be created in a dedicated PR, and the result of the **Plan** job of the terraform deployment should be analyzed precisely to make sure that the change has been taken into consideration.

## Deployment of the new protocol parameters

The update of the new protocol parameters will take place as detailed in the following table:
| Workflow | Deployed at | Effective at
|------------|------------|------------
| [`.github/workflows/ci.yml`](../../github/workflows/ci.yml) | Merge on `main` branch | **3** epochs later
| [`.github/workflows/pre-release.yml`](../../github/workflows/pre-release.yml) | Pre-release of a distribution | **3** epochs later
| [`.github/workflows/release.yml`](../../github/workflows/release.yml) | Release of a distribution | **3** epochs later

For more information about the CD, please refer to [Release process and versioning](https://mithril.network/doc/adr/3).