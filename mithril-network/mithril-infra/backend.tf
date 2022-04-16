terraform {
  # we use gcloud as backend, configuration of state bucker and zone
  # should be provided on the command-line when init-ing the state
  backend "gcs" {
    bucket = "hydra-terraform-admin"
    backend-config = "prefix=terraform/mithril-aggregator"
  }
}
