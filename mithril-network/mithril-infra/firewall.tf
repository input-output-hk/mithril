resource "google_compute_firewall" "mithril-aggregator-fw" {
  name    = "mithril-aggregator-fw"
  network = "default"

  allow {
    protocol = "tcp"
    ports    = ["22", "80"]
  }

  source_ranges = ["0.0.0.0/0"]
  target_tags   = ["mithril-aggregator"]
}
