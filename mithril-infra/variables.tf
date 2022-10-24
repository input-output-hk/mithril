variable "environment_prefix" {
  type        = string
  description = "The environment prefix to deploy: testing, pre-release or release"
}

variable "environment_suffix" {
  type        = string
  description = "The environment suffix to deploy"
}

variable "cardano_network" {
  type        = string
  description = "The Cardano network name to attach: preview, preprod or mainnet"
}

locals {
  environment_name_short = format("%s%s", "${var.environment_prefix}-${var.cardano_network}", var.environment_suffix != "" ? "-${var.environment_suffix}" : "")
  environment_name       = "mithril-${local.environment_name_short}"
}

variable "google_region" {
  type        = string
  description = "The region on GCP"
}

variable "google_zone" {
  type        = string
  description = "The zone on GCP"
}

variable "google_machine_type" {
  type        = string
  description = "The machine type on which to run the VM on GCP"
}

variable "google_service_credentials_json" {
  type        = string
  description = "The credentials of the GCP service account"
}

variable "google_storage_bucket_max_age" {
  type        = number
  description = "Number of days after which an object in the storage bucket expires"
  default     = 14
}

locals {
  google_service_credentials_json_decoded = jsondecode(file(var.google_service_credentials_json))
  google_service_account_private_key      = local.google_service_credentials_json_decoded.private_key
  google_project_id                       = local.google_service_credentials_json_decoded.project_id
}

variable "mithril_api_domain" {
  type        = string
  description = "The Mithril api (sub)domain name of service to deploy"
}

variable "mithril_image_id" {
  type        = string
  description = "The Mithril image tag of service to deploy"
}

variable "mithril_genesis_verification_key_url" {
  type        = string
  description = "The url of the Mithril genesis verification key used by to verify a genesis certificate"
}
variable "mithril_genesis_secret_key" {
  type        = string
  description = "The Mithril genesis secret key used by the aggregator to bootstrap a genesis certificate (test only)"
}

variable "mithril_signers" {
  type = map(object({
    pool_id = string
  }))
}
