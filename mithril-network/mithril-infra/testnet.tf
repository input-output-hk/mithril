resource "google_service_account" "mithril_service_account" {
  account_id   = "mithril-aggregator"
  display_name = "Mithril Aggregator Service Account"
}

resource "google_project_iam_member" "mithril_account" {
  project = "iog-hydra"
  role    = "roles/storage.objectAdmin"
  member  = "serviceAccount:${google_service_account.mithril_service_account.email}"
}

# an instance running a mithril aggregator alongside a cardano node
# on public testnet
resource "google_compute_instance" "mithril-aggregator-testnet" {
  name = "mithril-aggregator-testnet-1"

  # https://cloud.google.com/compute/docs/compute-optimized-machines
  # 2 vCPU, 16GB
  machine_type              = "e2-highmem-2"
  allow_stopping_for_update = true

  tags = ["mithril-aggregator", "testnet"]

  metadata = {
    # sets the public keys that are authorised to log on this machin
    sshKeys = file("ssh_keys")
  }

  boot_disk {
    initialize_params {
      size  = 200
      image = "iog-mithril-1650102670"
    }
  }

  network_interface {
    network = "default"
    access_config {
      # A statically allocated IP
      nat_ip = google_compute_address.mithril-aggregator-testnet-address.address
    }
  }

  service_account {
    # Google recommends custom service accounts that have cloud-platform scope and permissions granted via IAM Roles.
    email  = "mithril-aggregator@iog-hydra.iam.gserviceaccount.com"
    scopes = ["cloud-platform"]
  }

  # upload configuration script file
  # this is run only once, when the VM is provisioned, it only downloads a testnet snapshot
  # TODO: replace with some provisioning tool?
  provisioner "file" {
    source      = "scripts/configure-testnet.sh"
    destination = "/home/curry/configure-testnet.sh"

    connection {
      type = "ssh"
      user = "curry"
      host = self.network_interface.0.access_config.0.nat_ip
    }
  }


  # run configuration file on the remote machine
  provisioner "remote-exec" {
    inline = [
      "chmod +x /home/curry/configure-testnet.sh",
      "/home/curry/configure-testnet.sh"
    ]

    connection {
      type = "ssh"
      user = "curry"
      host = self.network_interface.0.access_config.0.nat_ip
    }
  }

}

resource "google_compute_address" "mithril-aggregator-testnet-address" {
  name = "mithril-aggregator-testnet-address"
}

output "mithril-aggregator-testnet-ip" {
  value = google_compute_address.mithril-aggregator-testnet-address.address
}

output "project" {
  value = google_compute_instance.mithril-aggregator-testnet.project
}
