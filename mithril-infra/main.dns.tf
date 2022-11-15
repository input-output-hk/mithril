resource "google_dns_managed_zone" "mithril-api-zone" {
  name        = "${local.environment_name}-dns"
  dns_name    = "${local.environment_name_short}.${var.mithril_api_domain}."
  description = "DNS zone to manage Mithril API"
  visibility  = "public"
}

resource "google_dns_record_set" "mithril-aggregator-endpoint" {
  name         = "aggregator.${google_dns_managed_zone.mithril-api-zone.dns_name}"
  managed_zone = google_dns_managed_zone.mithril-api-zone.name
  type         = "A"
  ttl          = 300
  rrdatas      = [google_compute_address.mithril-external-address.address]
}

resource "google_dns_record_set" "mithril-signer-endpoint" {
  for_each = var.mithril_signers

  name         = "mithril-signer-${each.key}.${google_dns_managed_zone.mithril-api-zone.dns_name}"
  managed_zone = google_dns_managed_zone.mithril-api-zone.name
  type         = "A"
  ttl          = 300
  rrdatas      = [google_compute_address.mithril-external-address.address]
}

locals {
  mithril_aggregator_host         = trimsuffix(google_dns_record_set.mithril-aggregator-endpoint.name, ".")
  mithril_aggregator_endpoint_url = format("https://%s/aggregator", local.mithril_aggregator_host)
  mithril_signers_host = {
    for key, signer in var.mithril_signers :
    key => "mithril-signer-${key}.${trimsuffix(google_dns_managed_zone.mithril-api-zone.dns_name, ".")}"
  }
  mithril_signers_endpoint_url = [for key, signer in var.mithril_signers :
    format("https://%s", "mithril-signer-${key}.${trimsuffix(google_dns_managed_zone.mithril-api-zone.dns_name, ".")}")
  ]
}
