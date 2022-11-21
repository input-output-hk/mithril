variable "environment_prefix" {
  type        = string
  description = "The environment prefix to deploy: testing, pre-release or release"
}

variable "environment_suffix" {
  type        = string
  description = "The environment suffix to deploy"
  default     = ""
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
  default     = "europe-west1"
}

variable "google_zone" {
  type        = string
  description = "The zone on GCP"
  default     = "europe-west1-b"
}

variable "google_machine_type" {
  type        = string
  description = "The machine type on which to run the VM on GCP"
  default     = "e2-medium"
}

variable "google_compute_instance_boot_disk_size" {
  type        = number
  description = "Size of the boot disk in GB"
  default     = 200
}

variable "google_compute_instance_boot_disk_type" {
  type        = string
  description = "Type of disk"
  default     = "pd-standard"
}

variable "google_compute_instance_boot_disk_image" {
  type        = string
  description = "Image of the boot disk"
  default     = "ubuntu-os-cloud/ubuntu-2204-lts"
}

variable "google_compute_instance_boot_disk_snapshot" {
  type        = string
  description = "Snapshot used to restore the boot disk"
  default     = ""
}

variable "google_service_credentials_json_file" {
  type        = string
  description = "The credentials of the GCP service account"
}

variable "google_storage_bucket_max_age" {
  type        = number
  description = "Number of days after which an object in the storage bucket expires"
  default     = 14
}

variable "google_snapshot_max_retention_days" {
  type        = number
  description = "Number of days after a disk snapshot is dropped"
  default     = 30
}

locals {
  google_service_credentials_json_file_decoded = jsondecode(file(var.google_service_credentials_json_file))
  google_service_account_private_key           = local.google_service_credentials_json_file_decoded.private_key
  google_project_id                            = local.google_service_credentials_json_file_decoded.project_id
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

variable "mithril_protocol_parameters" {
  type = object({
    k     = number,
    m     = number,
    phi_f = number
  })
  description = "The Mithril protocol parameters used to aggregate multi signatures"
  default = {
    k     = 5
    m     = 100
    phi_f = 0.65
  }
}

variable "mithril_signers" {
  type = map(object({
    type    = string
    pool_id = string
  }))
  default = {
    "1" = {
      type    = "unverified",
      pool_id = "pool15qde6mnkc0jgycm69ua0grwxmmu0tke54h5uhml0j8ndw3kcu9x",
    }
  }
}
