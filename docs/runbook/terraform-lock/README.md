# Fix terraform deployment lock

## Introduction

When the CI cancels a job that is in the middle of a terraform deployment, there is a chance that the lock file used by terraform under the hood to avoid concurrent deployment is not removed. In that cas, the next time a CI job tries to deploy, it will receive an error stating that there is a lock that prevents the deployment to be operated.

## Find the workflow used to deploy a Mithril network

Currently, the following [Mithril networks](https://mithril.network/doc/manual/developer-docs/references#mithril-networks) are generally available, and deployed with `terraform`:
- `testing-preview`: with the workflow [`.github/workflows/ci.yml`](../../github/workflows/ci.yml)
- `pre-release-preview`: with the workflow [`.github/workflows/pre-release.yml`](../../github/workflows/pre-release.yml)
- `release-preprod`: with the workflow [`.github/workflows/release.yml`](../../github/workflows/release.yml)
- `release-mainnet`: with the workflow [`.github/workflows/release.yml`](../../github/workflows/release.yml)


## Identify the terraform backend bucket
 In the workflow file, there is a `terraform_backend_bucket` that details the GCP bucket that is used by terraform to store the state of the deployment.

## Reset the terraform lock

A user with administrator rights can simply remove the lock file:
- In GCP [**Cloud Storage**](https://console.cloud.google.com/storage/browser)
- In the terraform administration bucket that you have identified earlier, the file that needs to be removed is at path `**TERRAFORM_BACKEND_BUCKET**/terraform/mithril-**MITHRIL_NETWORK_IDENTIFIER**/.terraform.lock.hcl` (e.g. `mithril-terraform-prod/terraform/mithril-release-mainnet/terraform.lock.hcl`) 

:warning: never delete/modify the `**TERRAFORM_BACKEND_BUCKET**/terraform/mithril-**MITHRIL_NETWORK_IDENTIFIER**/default.tfstate` file.