terraform {
  required_providers {
    google = {
      source  = "hashicorp/google"
      version = "3.5.0"
    }
  }
}

provider "google" {
  credentials = file(var.google_service_credentials_json)
  project     = local.google_project_id
  region      = var.google_region
  zone        = var.google_zone
}

resource "google_compute_network" "vpc_network" {
  name = "${local.environment_name}-network"
}

resource "google_compute_instance" "vm_instance" {
  name         = "${local.environment_name}-vm"
  machine_type = var.google_machine_type
  tags         = ["mithril", local.environment_name, var.environment_prefix, var.cardano_network]

  allow_stopping_for_update = true

  metadata = {
    sshKeys = file("./assets/ssh_keys")
  }

  metadata_startup_script = file("./assets/startup-vm.sh")

  boot_disk {
    initialize_params {
      size  = 200
      image = "ubuntu-os-cloud/ubuntu-2204-lts"
    }
  }

  network_interface {
    network = google_compute_network.vpc_network.name
    access_config {
      nat_ip = google_compute_address.mithril-external-address.address
    }
  }
}

resource "google_compute_address" "mithril-external-address" {
  name = "${local.environment_name}-ip"
}

