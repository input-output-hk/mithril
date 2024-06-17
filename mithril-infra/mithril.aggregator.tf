
locals {
  mithril_aggregator_relay_mithril_listen_port = 6060
}

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
      "mkdir -p /home/curry/data/${var.cardano_network}/mithril-aggregator/cardano/config",
      "mkdir -p /home/curry/data/${var.cardano_network}/mithril-aggregator/cardano/db",
      "mkdir -p /home/curry/data/${var.cardano_network}/mithril-aggregator/cardano/ipc",
      "mkdir -p /home/curry/data/${var.cardano_network}/mithril-aggregator/mithril/stores",
      "mkdir -p /home/curry/data/${var.cardano_network}/mithril-aggregator/mithril/snapshots",
      <<-EOT
set -e
# Setup cardano node configuration
AGGREGATOR_CONFIG_DIRECTORY=/home/curry/data/${var.cardano_network}/mithril-aggregator/cardano/config
cp -R /home/curry/docker/cardano-configurations/network/${var.cardano_network} $AGGREGATOR_CONFIG_DIRECTORY
cat $AGGREGATOR_CONFIG_DIRECTORY/${var.cardano_network}/cardano-node/config.json | jq ".hasPrometheus[0] |= \"cardano-node-aggregator\"" > $AGGREGATOR_CONFIG_DIRECTORY/${var.cardano_network}/cardano-node/config.json.new
rm -f $AGGREGATOR_CONFIG_DIRECTORY/${var.cardano_network}/cardano-node/config.json
mv $AGGREGATOR_CONFIG_DIRECTORY/${var.cardano_network}/cardano-node/config.json.new $AGGREGATOR_CONFIG_DIRECTORY/${var.cardano_network}/cardano-node/config.json
EOT
    ]
  }

  provisioner "remote-exec" {
    inline = [
      "export NETWORK=${var.cardano_network}",
      "export CARDANO_IMAGE_ID=${var.cardano_image_id}",
      "export CARDANO_IMAGE_REGISTRY=${var.cardano_image_registry}",
      "export MITHRIL_IMAGE_ID=${var.mithril_image_id}",
      "export AGGREGATOR_HOST=${local.mithril_aggregator_host}",
      "export GOOGLE_APPLICATION_CREDENTIALS_JSON='${local.google_cloud_storage_credentials_json}'",
      "export SIGNED_ENTITY_TYPES='${var.mithril_aggregator_signed_entity_types}'",
      "export SNAPSHOT_BUCKET_NAME='${google_storage_bucket.cloud_storage.name}'",
      "export SNAPSHOT_USE_CDN_DOMAIN='${var.mithril_aggregator_snapshot_use_cdn_domain}'",
      "export SNAPSHOT_COMPRESSION_ALGORITHM=${var.mithril_aggregator_snapshot_compression_algorithm}",
      "export ZSTANDARD_PARAMETERS__LEVEL=${var.mithril_aggregator_zstandard_parameters_level}",
      "export ZSTANDARD_PARAMETERS__NUMBER_OF_WORKERS=${var.mithril_aggregator_zstandard_parameters_workers}",
      "export GENESIS_VERIFICATION_KEY=$(wget -q -O - ${var.mithril_genesis_verification_key_url})",
      "export GENESIS_SECRET_KEY='${var.mithril_genesis_secret_key}'",
      "export PROTOCOL_PARAMETERS__K='${var.mithril_protocol_parameters.k}'",
      "export PROTOCOL_PARAMETERS__M='${var.mithril_protocol_parameters.m}'",
      "export PROTOCOL_PARAMETERS__PHI_F='${var.mithril_protocol_parameters.phi_f}'",
      "export CHAIN_OBSERVER_TYPE='${var.mithril_aggregator_chain_observer_type}'",
      "export ERA_READER_ADAPTER_TYPE='${var.mithril_era_reader_adapter_type}'",
      "export ERA_READER_ADAPTER_PARAMS=$(jq -nc --arg address $(wget -q -O - ${var.mithril_era_reader_address_url}) --arg verification_key $(wget -q -O - ${var.mithril_era_reader_verification_key_url}) '{\"address\": $address, \"verification_key\": $verification_key}')",
      "export ERA_READER_SECRET_KEY='${var.mithril_era_reader_secret_key}'",
      "export CEXPLORER_POOLS_URL='${var.mithril_aggregator_cexplorer_pools_url}'",
      "export ALLOW_UNPARSABLE_BLOCK=${var.mithril_aggregator_allow_unparsable_block}",
      "export CARDANO_TRANSACTIONS_PROVER_CACHE_POOL_SIZE=${var.mithril_aggregator_cardano_transactions_prover_cache_pool_size}",
      "export CARDANO_TRANSACTIONS_DATABASE_CONNECTION_POOL_SIZE=${var.mithril_aggregator_cardano_transactions_database_connection_pool_size}",
      "export LOGGING_DRIVER='${var.mithril_container_logging_driver}'",
      "export AUTH_USER_PASSWORD=$(htpasswd -nb ${var.mithril_aggregator_auth_username} ${var.mithril_aggregator_auth_password})",
      "export AGGREGATOR_RELAY_LISTEN_PORT='${local.mithril_aggregator_relay_mithril_listen_port}'",
      "export CURRENT_UID=$(id -u)",
      "export DOCKER_GID=$(getent group docker | cut -d: -f3)",
      "docker compose -f /home/curry/docker/docker-compose-aggregator-${local.mithril_aggregator_type}${local.mithril_network_type_suffix}.yaml --profile all up -d",
    ]
  }
}
