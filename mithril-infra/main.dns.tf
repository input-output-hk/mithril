resource "google_dns_managed_zone" "mithril-api-zone" {
  name        = "${local.environment_name}-dns"
  dns_name    = "${local.environment_name_short}.${var.mithril_api_domain}."
  description = "DNS zone to manage Mithril API"
  visibility  = "public"
}

output "mithril_api_zone" {
  value = google_dns_managed_zone.mithril-api-zone.dns_name
}

resource "google_dns_record_set" "mithril-aggregator-endpoint" {
  name         = "aggregator.${google_dns_managed_zone.mithril-api-zone.dns_name}"
  managed_zone = google_dns_managed_zone.mithril-api-zone.name
  type         = "A"
  ttl          = 300
  rrdatas      = [google_compute_address.mithril-external-address.address]
}

resource "google_project_service" "siteverification" {
  service            = "siteverification.googleapis.com"
  disable_on_destroy = false
}

data "googlesiteverification_dns_token" "mithril-aggregator-cdn-endpoint" {
  domain     = "aggregator.${google_dns_managed_zone.mithril-api-zone.dns_name}"
  depends_on = [google_project_service.siteverification]
}

resource "googlesiteverification_dns" "mithril-aggregator" {
  domain     = "aggregator.${google_dns_managed_zone.mithril-api-zone.dns_name}"
  token      = data.googlesiteverification_dns_token.mithril-aggregator-cdn-endpoint.record_value
  depends_on = [google_dns_managed_zone.mithril-api-zone]
}

resource "google_dns_record_set" "mithril-aggregator-txt" {
  managed_zone = google_dns_managed_zone.mithril-api-zone.name
  name         = "aggregator.${google_dns_managed_zone.mithril-api-zone.dns_name}"
  type         = data.googlesiteverification_dns_token.mithril-aggregator-cdn-endpoint.record_type
  ttl          = 60
  rrdatas      = [data.googlesiteverification_dns_token.mithril-aggregator-cdn-endpoint.record_value]
}

resource "google_dns_record_set" "mithril-aggregator-cdn-endpoint-cname" {
  name         = "cdn.aggregator.${google_dns_managed_zone.mithril-api-zone.dns_name}"
  managed_zone = google_dns_managed_zone.mithril-api-zone.name
  type         = "CNAME"
  ttl          = 300
  rrdatas      = [var.mithril_aggregator_cdn_cname]
}

resource "google_dns_record_set" "mithril-signer-endpoint" {
  for_each = var.mithril_signers

  name         = "mithril-signer-${each.key}.${google_dns_managed_zone.mithril-api-zone.dns_name}"
  managed_zone = google_dns_managed_zone.mithril-api-zone.name
  type         = "A"
  ttl          = 300
  rrdatas      = [google_compute_address.mithril-external-address.address]
}

resource "google_dns_record_set" "prometheus-endpoint" {
  name         = "prometheus.${google_dns_managed_zone.mithril-api-zone.dns_name}"
  managed_zone = google_dns_managed_zone.mithril-api-zone.name
  type         = "A"
  ttl          = 300
  rrdatas      = [google_compute_address.mithril-external-address.address]
}

resource "google_dns_record_set" "loki-endpoint" {
  name         = "loki.${google_dns_managed_zone.mithril-api-zone.dns_name}"
  managed_zone = google_dns_managed_zone.mithril-api-zone.name
  type         = "A"
  ttl          = 300
  rrdatas      = [google_compute_address.mithril-external-address.address]
}

locals {
  mithril_aggregator_host         = trimsuffix(google_dns_record_set.mithril-aggregator-endpoint.name, ".")
  mithril_aggregator_endpoint_url = format("https://%s%s/aggregator", local.mithril_aggregator_credentials, local.mithril_aggregator_host)
  mithril_aggregator_cdn_host     = trimsuffix(google_dns_record_set.mithril-aggregator-cdn-endpoint-cname.name, ".")
  mithril_signers_host = {
    for key, signer in var.mithril_signers :
    key => "mithril-signer-${key}.${trimsuffix(google_dns_managed_zone.mithril-api-zone.dns_name, ".")}"
  }
  mithril_signers_endpoint_url = [for key, signer in var.mithril_signers :
    format("https://%s", "mithril-signer-${key}.${trimsuffix(google_dns_managed_zone.mithril-api-zone.dns_name, ".")}")
  ]
  prometheus_host         = trimsuffix(google_dns_record_set.prometheus-endpoint.name, ".")
  prometheus_endpoint_url = format("https://%s%s", local.prometheus_credentials, local.prometheus_host)
  loki_host               = trimsuffix(google_dns_record_set.loki-endpoint.name, ".")
  loki_endpoint_url       = format("https://%s%s", local.loki_credentials, local.loki_host)
}
