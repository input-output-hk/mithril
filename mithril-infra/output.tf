output "google_project" {
  value = local.google_project_id
}

output "aggregator_endpoint" {
  value = local.mithril_aggregator_endpoint_url
}

output "storage_bucket" {
  value = google_storage_bucket.cloud_storage.name
}

output "external-ip" {
  value = google_compute_address.mithril-external-address.address
}

output "api_subdomain" {
  value = google_dns_managed_zone.mithril-api-zone.dns_name
}

output "name_servers" {
  value = google_dns_managed_zone.mithril-api-zone.name_servers
}
