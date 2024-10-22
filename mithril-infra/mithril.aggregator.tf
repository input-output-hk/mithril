
locals {
  mithril_aggregator_relay_mithril_listen_port = 6060
}

resource "null_resource" "mithril_aggregator" {
  depends_on = [
    null_resource.mithril_reverse_proxy
  ]

  triggers = {
    vm_instance                                                               = google_compute_instance.vm_instance.id,
    cardano_image_id                                                          = var.cardano_image_id,
    cardano_image_registry                                                    = var.cardano_image_registry,
    image_id                                                                  = var.mithril_image_id,
    mithril_aggregator_auth_username                                          = var.mithril_aggregator_auth_username,
    mithril_aggregator_auth_password                                          = var.mithril_aggregator_auth_password
    mithril_aggregator_signed_entity_types                                    = var.mithril_aggregator_signed_entity_types,
    mithril_aggregator_snapshot_compression_algorithm                         = var.mithril_aggregator_snapshot_compression_algorithm,
    mithril_aggregator_zstandard_parameters_level                             = var.mithril_aggregator_zstandard_parameters_level,
    mithril_aggregator_zstandard_parameters_workers                           = var.mithril_aggregator_zstandard_parameters_workers,
    mithril_aggregator_cardano_transactions_prover_cache_pool_size            = var.mithril_aggregator_cardano_transactions_prover_cache_pool_size,
    mithril_aggregator_cardano_transactions_database_connection_pool_size     = var.mithril_aggregator_cardano_transactions_database_connection_pool_size,
    mithril_aggregator_cardano_transactions_signing_config_security_parameter = var.mithril_aggregator_cardano_transactions_signing_config_security_parameter,
    mithril_aggregator_cardano_transactions_signing_config_step               = var.mithril_aggregator_cardano_transactions_signing_config_step,
    mithril_aggregator_cexplorer_pools_url                                    = var.mithril_aggregator_cexplorer_pools_url,
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
# Copy the cardano node configuration files to the aggregator (exact version, and fallback to minor version)
CARDANO_NODE_EXACT_VERSION="${var.cardano_image_id}"
CARDANO_NODE_MINOR_VERSION=$(echo $CARDANO_NODE_EXACT_VERSION | cut -d. -f1,2)
CARDANO_NODE_VERSIONS="$CARDANO_NODE_EXACT_VERSION $CARDANO_NODE_MINOR_VERSION"
FOUND_CONFIGURATION=false
for CARDANO_NODE_VERSION in $CARDANO_NODE_VERSIONS; do
  if [ -d "/home/curry/docker/cardano/config/$CARDANO_NODE_VERSION/${var.cardano_network}" ]; then 
    AGGREGATOR_CONFIG_DIRECTORY=/home/curry/data/${var.cardano_network}/mithril-aggregator/cardano/config
    rm -rf $AGGREGATOR_CONFIG_DIRECTORY
    mkdir -p $AGGREGATOR_CONFIG_DIRECTORY
    cp -R /home/curry/docker/cardano/config/$CARDANO_NODE_VERSION/${var.cardano_network} $AGGREGATOR_CONFIG_DIRECTORY
    echo $CARDANO_NODE_VERSION > $AGGREGATOR_CONFIG_DIRECTORY/config.version
    cat $AGGREGATOR_CONFIG_DIRECTORY/${var.cardano_network}/cardano-node/config.json | jq ".hasPrometheus[0] |= \"cardano-node-aggregator\"" > $AGGREGATOR_CONFIG_DIRECTORY/${var.cardano_network}/cardano-node/config.json.new
    rm -f $AGGREGATOR_CONFIG_DIRECTORY/${var.cardano_network}/cardano-node/config.json
    mv $AGGREGATOR_CONFIG_DIRECTORY/${var.cardano_network}/cardano-node/config.json.new $AGGREGATOR_CONFIG_DIRECTORY/${var.cardano_network}/cardano-node/config.json
    FOUND_CONFIGURATION=true
    break
  fi
done
# Check if a configuration was found
if [ "$FOUND_CONFIGURATION" = "false" ]; then
  echo "No cardano node configuration found for version $CARDANO_NODE_EXACT_VERSION"
  exit 1
fi
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
      "export CARDANO_TRANSACTIONS_SIGNING_CONFIG__SECURITY_PARAMETER=${var.mithril_aggregator_cardano_transactions_signing_config_security_parameter}",
      "export CARDANO_TRANSACTIONS_SIGNING_CONFIG__STEP=${var.mithril_aggregator_cardano_transactions_signing_config_step}",
      "export ENABLE_METRICS_SERVER=true",
      "export METRICS_SERVER_IP=0.0.0.0",
      "export METRICS_SERVER_PORT=9090",
      "export LOGGING_DRIVER='${var.mithril_container_logging_driver}'",
      "export AUTH_USER_PASSWORD=$(htpasswd -nb ${var.mithril_aggregator_auth_username} ${var.mithril_aggregator_auth_password})",
      "export AGGREGATOR_RELAY_LISTEN_PORT='${local.mithril_aggregator_relay_mithril_listen_port}'",
      "export CURRENT_UID=$(id -u)",
      "export DOCKER_GID=$(getent group docker | cut -d: -f3)",
      "docker compose -f /home/curry/docker/docker-compose-aggregator-${local.mithril_aggregator_type}${local.mithril_network_type_suffix}.yaml --profile all up -d",
    ]
  }
}
