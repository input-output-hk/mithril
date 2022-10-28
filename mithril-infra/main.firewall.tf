resource "google_compute_firewall" "mithril-vm-firewall" {
  name    = "${local.environment_name}-firewall"
  network = google_compute_network.vpc_network.id

  allow {
    protocol = "tcp"
    ports    = ["22", "80", "443"]
  }

  source_ranges = ["0.0.0.0/0"]
  target_tags   = [local.environment_name]
}
