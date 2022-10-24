
resource "null_resource" "mithril_aggregator" {
  depends_on = [
    null_resource.mithril_reverse_proxy
  ]

  triggers = {
    image_id = var.mithril_image_id
  }

  connection {
    type        = "ssh"
    user        = "curry"
    private_key = local.google_service_account_private_key
    host        = google_compute_address.mithril-external-address.address
  }

  provisioner "remote-exec" {
    inline = [
      "mkdir -p /home/curry/data/${var.cardano_network}",
      "mkdir -p /home/curry/data/${var.cardano_network}/mithril-aggregator/cardano/db",
      "mkdir -p /home/curry/data/${var.cardano_network}/mithril-aggregator/cardano/ipc",
      "mkdir -p /home/curry/data/${var.cardano_network}/mithril-aggregator/mithril/stores",
      "mkdir -p /home/curry/data/${var.cardano_network}/mithril-aggregator/mithril/snapshots"
    ]
  }

  provisioner "remote-exec" {
    inline = [
      "export NETWORK=${var.cardano_network}",
      "export IMAGE_ID=${var.mithril_image_id}",
      "export AGGREGATOR_HOST=${local.mithril_aggregator_host}",
      "export GOOGLE_APPLICATION_CREDENTIALS_JSON='${local.google_cloud_storage_credentials_json}'",
      "export SNAPSHOT_BUCKET_NAME='${google_storage_bucket.cloud_storage.name}'",
      "export GENESIS_VERIFICATION_KEY=$(wget -q -O - ${var.mithril_genesis_verification_key_url})",
      "export GENESIS_SECRET_KEY='${var.mithril_genesis_secret_key}'",
      "export CURRENT_UID=$(id -u)",
      "export DOCKER_GID=$(getent group docker | cut -d: -f3)",
      "docker-compose -f /home/curry/docker/docker-compose-aggregator.yaml --profile all up -d",
    ]
  }
}
