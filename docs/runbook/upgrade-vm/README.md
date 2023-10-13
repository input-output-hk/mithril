# Upgrade the VM of the infrastructure of a Mithril network.

## Introduction

From time to time, we may need to upgrade or downgrade the VM that is powering the infrastructure of a Mithril network (Mithril aggregator, Mithril signers, Cardano nodes, ...).

## Find the workflow used to deploy a Mithril network

Currently, the following [Mithril networks](https://mithril.network/doc/manual/developer-docs/references#mithril-networks) are generally available, and deployed with `terraform`:

- `testing-preview`: with the workflow [`.github/workflows/ci.yml`](../../github/workflows/ci.yml)
- `pre-release-preview`: with the workflow [`.github/workflows/pre-release.yml`](../../github/workflows/pre-release.yml)
- `release-preprod`: with the workflow [`.github/workflows/release.yml`](../../github/workflows/release.yml)
- `release-mainnet`: with the workflow [`.github/workflows/release.yml`](../../github/workflows/release.yml)

## Update the VM instance type

Update the following value of the targeted VM (`google_machine_type`) in the deployment matrix of the network with the new value that need to be used:

```yaml
google_machine_type: e2-highmem-4
```

Which will be replaced eg with:

```yaml
google_machine_type: e2-highmem-8
```

The list of available machines on GCP is available [here](https://cloud.google.com/compute/all-pricing)

The modifications should be created in a dedicated PR, and the result of the **Plan** job of the terraform deployment should be analyzed precisely to make sure that the change has been taken into consideration.

:warning: The modification of the VM is **usually** an operation that can be performed **in place** by terraform. This means that the modification is transparent and does not imply any **loss of data**. If the **Plan** does not explicitely state that the modification will be done in place, it means that a more complex upgrade operation needs to be implemented.

Example of a terraform plan with an in place modification of the VM:

```bash
Terraform will perform the following actions:

  # google_compute_instance.vm_instance will be updated in-place
  ~ resource "google_compute_instance" "vm_instance" {
        id                        = "projects/iog-hydra/zones/europe-west1-b/instances/mithril-testing-preview-vm"
      ~ machine_type              = "e2-highmem-4" -> "e2-highmem-8"
        name                      = "mithril-testing-preview-vm"
        tags                      = [
            "mithril",
            "mithril-testing-preview",
            "preview",
            "testing",
        ]
        # (20 unchanged attributes hidden)

        # (5 unchanged blocks hidden)
    }
```

## Deployment of the new protocol parameters

The update of the new protocol parameters will take place as detailed in the following table:
| Workflow | Deployed at | Effective at
|------------|------------|------------
| [`.github/workflows/ci.yml`](../../github/workflows/ci.yml) | Merge on `main` branch | Immediately
| [`.github/workflows/pre-release.yml`](../../github/workflows/pre-release.yml) | Pre-release of a distribution | Immediately
| [`.github/workflows/release.yml`](../../github/workflows/release.yml) | Release of a distribution | Immediately

For more information about the CD, please refer to [Release process and versioning](https://mithril.network/doc/adr/3).
