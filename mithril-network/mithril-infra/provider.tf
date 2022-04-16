variable "google_provider_file" {
  type        = string
  description = "The path to file containing GCP credentials for the service account deploying the resources"
  default     = "./hydra.json"
}

variable "google_provider_region" {
  type        = string
  description = "The GCP region to deploy the resources to"
  default     = "europe-west1"
}

variable "google_provider_zone" {
  type        = string
  description = "The GCP zone to deploy the resources to"
  default     = "europe-west1-b"
}

variable "google_provider_project" {
  type        = string
  description = "The identifier of the project to deploy resources in"
  default     = "iog-hydra"
}

provider "google" {
  region      = var.google_provider_region
  zone        = var.google_provider_zone
  project     = var.google_provider_project
}

variable "image_id" {
  type        = string
  description = "The image tag of service to deploy"
}
