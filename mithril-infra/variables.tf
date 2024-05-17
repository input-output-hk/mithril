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

variable "google_compute_instance_boot_disk_snapshot_max_retention_days" {
  type        = number
  description = "Number of days after a boot disk snapshot is dropped"
  default     = 30
}

variable "google_compute_instance_boot_disk_snapshot_pace_days" {
  type        = number
  description = "Pace of the boot disk snapshot in days"
  default     = 1
}

variable "google_compute_instance_boot_disk_snapshot_start_time" {
  type        = string
  description = "Start time of the boot disk snapshot"
  default     = "04:00"
}

variable "google_compute_instance_data_disk_size" {
  type        = number
  description = "Size of the data disk in GB"
  default     = 250
}

variable "google_compute_instance_data_disk_type" {
  type        = string
  description = "Type of disk"
  default     = "pd-standard"
}

variable "google_compute_instance_data_disk_snapshot" {
  type        = string
  description = "Snapshot used to restore the data disk"
  default     = ""
}

variable "google_compute_instance_data_disk_snapshot_max_retention_days" {
  type        = number
  description = "Number of days after a data disk snapshot is dropped"
  default     = 30
}

variable "google_compute_instance_data_disk_snapshot_pace_days" {
  type        = number
  description = "Pace of the data disk snapshot in days"
  default     = 1
}

variable "google_compute_instance_data_disk_snapshot_start_time" {
  type        = string
  description = "Start time of the data disk snapshot"
  default     = "03:00"
}

variable "google_service_credentials_json_file" {
  type        = string
  description = "The credentials of the GCP service account"
}

variable "google_storage_bucket_force_destroy" {
  type        = bool
  description = "Force destroy all items of the bucket when destroying the associated terraform resource"
  default     = false
}

variable "google_storage_bucket_max_age" {
  type        = number
  description = "Number of days after which an object in the storage bucket expires"
  default     = 14
}

locals {
  google_service_credentials_json_file_decoded = jsondecode(file(var.google_service_credentials_json_file))
  google_service_account_private_key           = local.google_service_credentials_json_file_decoded.private_key
  google_project_id                            = local.google_service_credentials_json_file_decoded.project_id
}

variable "cardano_image_id" {
  type        = string
  description = "The Cardano image tag of service to deploy"
  default     = "8.9.0"
}

variable "cardano_image_registry" {
  type        = string
  description = "The Cardano image repository of service to deploy"
  default     = "ghcr.io/intersectmbo/cardano-node"
}

variable "cardano_configurations_repository_commit" {
  type        = string
  description = "The Cardano configurations commit to use"
  default     = "692010ed0f454bfbb566c06443227c79e2f4dbab"
}

variable "mithril_api_domain" {
  type        = string
  description = "The Mithril api (sub)domain name of service to deploy"
}

variable "mithril_image_id" {
  type        = string
  description = "The Mithril image tag of service to deploy"
}

variable "mithril_container_logging_driver" {
  type        = string
  description = "The logging driver used by Mithril containers"
  default     = "json-file"
}

variable "mithril_use_p2p_network" {
  type        = bool
  description = "Use the P2P network layer (experimental, for test only)"
  default     = false
}

variable "mithril_p2p_signer_registration_repeat_delay" {
  type        = number
  description = "The repeat delay in milliseconds for the signer registration when operating in P2P mode (defaults to 1 hour)"
  default     = 3600 * 1000
}

locals {
  mithril_network_type_suffix = var.mithril_use_p2p_network ? "-p2p" : ""
}


variable "mithril_aggregator_signed_entity_types" {
  type        = string
  description = "The custom signed list of entity types used by the mithril aggregator (discriminants names in an ordered comma separated list)."
  default     = ""
}

variable "mithril_aggregator_chain_observer_type" {
  type        = string
  description = "The chain observer type used by the mithril aggregator."
  default     = "pallas"
}

variable "mithril_aggregator_snapshot_compression_algorithm" {
  type        = string
  description = "The compression algorithm of the snapshot archive"
  default     = "zstandard"
}

variable "mithril_aggregator_zstandard_parameters_level" {
  type        = string
  description = "Zstandard compression level parameter"
  default     = "9"
}

variable "mithril_aggregator_zstandard_parameters_workers" {
  type        = string
  description = "Zstandard number of workers parameter"
  default     = "4"
}

variable "mithril_aggregator_snapshot_use_cdn_domain" {
  type        = bool
  description = "Use CDN domain for constructing snapshot url"
  default     = false
}

variable "mithril_aggregator_cdn_cname" {
  type        = string
  description = "The CNAME field used for the mithril aggregator CDN"
  default     = "c.storage.googleapis.com."
}

variable "mithril_aggregator_auth_username" {
  type        = string
  description = "The username for authentication on the mithril aggregator"
  default     = ""
}

variable "mithril_aggregator_auth_password" {
  type        = string
  description = "The password for authentication on the mithril aggregator"
  default     = ""
}

variable "mithril_aggregator_cexplorer_pools_url" {
  type        = string
  description = "The CExplorer url of the list of pools that is used by the mithril aggregator"
  default     = ""
}

variable "mithril_aggregator_allow_unparsable_block" {
  type        = bool
  description = "If set no error is returned in case of unparsable block and an error log is written instead. Will be ignored on (pre)production networks."
  default     = false
}

variable "prometheus_auth_username" {
  type        = string
  description = "The username for authentication on local prometheus endpoint"
  default     = ""
}

variable "prometheus_auth_password" {
  type        = string
  description = "The password for authentication on local prometheus endpoint"
  default     = ""
}

variable "prometheus_ingest_host" {
  type        = string
  description = "The host to ingest on remote prometheus endpoint"
  default     = ""
}

variable "prometheus_ingest_username" {
  type        = string
  description = "The username to ingest on remote prometheus endpoint"
  default     = ""
}

variable "prometheus_ingest_password" {
  type        = string
  description = "The password to ingest on remote prometheus endpoint"
  default     = ""
}

variable "loki_auth_username" {
  type        = string
  description = "The username for authentication on local loki endpoint"
  default     = ""
}

variable "loki_auth_password" {
  type        = string
  description = "The password for authentication on local loki endpoint"
  default     = ""
}

variable "loki_ingest_host" {
  type        = string
  description = "The host to ingest on remote loki endpoint"
  default     = ""
}

variable "loki_ingest_username" {
  type        = string
  description = "The username to ingest on remote loki endpoint"
  default     = ""
}

variable "loki_ingest_password" {
  type        = string
  description = "The password to ingest on remote loki endpoint"
  default     = ""
}

locals {
  mithril_aggregator_type        = var.mithril_aggregator_auth_username == "" ? "noauth" : "auth"
  mithril_aggregator_credentials = var.mithril_aggregator_auth_username == "" ? "" : format("%s:%s@", var.mithril_aggregator_auth_username, var.mithril_aggregator_auth_password)
  prometheus_credentials         = var.prometheus_auth_username == "" ? "" : format("%s:%s@", var.prometheus_auth_username, var.prometheus_auth_password)
  loki_credentials               = var.loki_auth_username == "" ? "" : format("%s:%s@", var.loki_auth_username, var.loki_auth_password)
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

variable "mithril_era_reader_adapter_type" {
  type        = string
  description = "The Mithril era reader adapter used to read the era markers"
  default     = "cardano-chain"
}

variable "mithril_era_reader_address_url" {
  type        = string
  description = "The url of the Mithril era reader address used to query the on chain Utxo containing the era markers payload"
  default     = ""
}

variable "mithril_era_reader_verification_key_url" {
  type        = string
  description = "The url of the Mithril era reader verification key used by to verify an era markers payload"
  default     = ""
}
variable "mithril_era_reader_secret_key" {
  type        = string
  description = "The Mithril genesis secret key used by the aggregator to generate an era marker payload TxDatum file (test only)"
  default     = ""
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

