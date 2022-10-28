# Mithril infrastructure

**This is a work in progress** :hammer_and_wrench:

This infrastructure creates an ad hoc Mithril network on Google Cloud platform

---

## Pre-requisites

**Install Terraform**

- Install [terraform](https://www.terraform.io/downloads) tools (latest stable version).


## Create a new environment

If you want to run this terraform automation for multiple environments from the same source, you need to create a new workspace:

* Create en env variable:
```bash
DEPLOY_ENVIRONMENT=**new-environment**
```

* Create the terraform worspace if needed:
```bash
terraform workspace new $DEPLOY_ENVIRONMENT
```

* Switch to the terraform worspace if needed:
```bash
terraform workspace select $DEPLOY_ENVIRONMENT
```

* Init terraform state for this workspace:
```bash
terraform init
```

* Create a terraform variable file (and then add the variable values that you want to set):
```bash
touch env.$DEPLOY_ENVIRONMENT.tfvars
```

:warning: You can also add a custom variable value by using the cli arg `-var "var_name=**THE_VALUE**"`

* Lint terraform deployment (optional):
```bash
terraform fmt -check
```

* Plan terraform deployment:
```bash
terraform plan --var-file=env.$DEPLOY_ENVIRONMENT.tfvars
```

* Apply terraform deployment:
```bash
terraform apply --var-file=env.$DEPLOY_ENVIRONMENT.tfvars
```

You should see this output from terraform:
```bash
aggregator_endpoint = "https://aggregator.***.api.mithril.network/aggregator"
api_subdomain = "***.api.mithril.network."
external-ip = "35.195.148.171"
google_project = "mithril"
name_servers = tolist([
  "ns-cloud-d1.googledomains.com.",
  "ns-cloud-d2.googledomains.com.",
  "ns-cloud-d3.googledomains.com.",
  "ns-cloud-d4.googledomains.com.",
])
```

You will need to update the `NS` records of the **API sub domain** given by `api_subdomain` with the name servers listed in `name_servers`.

## Destroy an environment

* Destroy terraform deployment (optional):
```bash
terraform destroy --var-file=env.$DEPLOY_ENVIRONMENT.tfvars
```

## Bootstrap a Genesis Certificate (test only)

:warning: This operation will reset and invalidate the current `Certificate Chain`

The commands to run when bootstrapping is:

* Open a bash terminal on the VM and run the following command once connected:
```bash
docker exec mithril-aggregator /app/bin/mithril-aggregator -vvv genesis bootstrap
```
