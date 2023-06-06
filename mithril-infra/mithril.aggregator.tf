
resource "null_resource" "mithril_aggregator" {
  depends_on = [
    null_resource.mithril_reverse_proxy
  ]

  triggers = {
    image_id                         = var.mithril_image_id,
    vm_instance                      = google_compute_instance.vm_instance.id,
    mithril_aggregator_auth_username = var.mithril_aggregator_auth_username,
    mithril_aggregator_auth_password = var.mithril_aggregator_auth_password
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
      "export CARDANO_IMAGE_ID=${var.cardano_image_id}",
      "export MITHRIL_IMAGE_ID=${var.mithril_image_id}",
      "export AGGREGATOR_HOST=${local.mithril_aggregator_host}",
      "export GOOGLE_APPLICATION_CREDENTIALS_JSON='${local.google_cloud_storage_credentials_json}'",
      "export SNAPSHOT_BUCKET_NAME='${google_storage_bucket.cloud_storage.name}'",
      "export GENESIS_VERIFICATION_KEY=$(wget -q -O - ${var.mithril_genesis_verification_key_url})",
      "export GENESIS_SECRET_KEY='${var.mithril_genesis_secret_key}'",
      "export PROTOCOL_PARAMETERS__K='${var.mithril_protocol_parameters.k}'",
      "export PROTOCOL_PARAMETERS__M='${var.mithril_protocol_parameters.m}'",
      "export PROTOCOL_PARAMETERS__PHI_F='${var.mithril_protocol_parameters.phi_f}'",
      "export ERA_READER_ADAPTER_TYPE='${var.mithril_era_reader_adapter_type}'",
      "export ERA_READER_ADAPTER_PARAMS=$(jq -nc --arg address $(wget -q -O - ${var.mithril_era_reader_address_url}) --arg verification_key $(wget -q -O - ${var.mithril_era_reader_verification_key_url}) '{\"address\": $address, \"verification_key\": $verification_key}')",
      "export ERA_READER_SECRET_KEY='${var.mithril_era_reader_secret_key}'",
      "export LOGGING_DRIVER='${var.mithril_container_logging_driver}'",
      "export AUTH_USER_PASSWORD=$(htpasswd -nb ${var.mithril_aggregator_auth_username} ${var.mithril_aggregator_auth_password})",
      "export CURRENT_UID=$(id -u)",
      "export DOCKER_GID=$(getent group docker | cut -d: -f3)",
      "docker-compose -f /home/curry/docker/docker-compose-aggregator-${local.mithril_aggregator_type}.yaml --profile all up -d",
    ]
  }
}
