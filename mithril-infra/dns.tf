resource "google_dns_managed_zone" "mithril-api-zone" {
  name        = "mithril-api"
  dns_name    = "api.mithril.network."
  description = "DNS zone to manage Mithril API"
  visibility  = "public"
}

# A entry pointing to VM hosting the aggregator
resource "google_dns_record_set" "aggregator" {
  name         = "aggregator.${google_dns_managed_zone.mithril-api-zone.dns_name}"
  managed_zone = google_dns_managed_zone.mithril-api-zone.name
  type         = "A"
  ttl          = 300

  rrdatas = [google_compute_address.mithril-aggregator-testnet-address.address]
}

output "name_servers" {
  value = google_dns_managed_zone.mithril-api-zone.name_servers
}
