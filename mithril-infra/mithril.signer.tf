locals {
  mithril_signers_index                       = [for key, signer in var.mithril_signers : key]
  mithril_signers_www_port                    = { for key, signer in var.mithril_signers : key => index(local.mithril_signers_index, key) + 1 + 8080 }
  mithril_signers_relay_listen_port           = { for key, signer in var.mithril_signers : key => index(local.mithril_signers_index, key) + 1 + 6060 }
  mithril_signers_relay_server_port           = { for key, signer in var.mithril_signers : key => index(local.mithril_signers_index, key) + 1 + 7070 }
  mithril_signers_relay_cardano_port          = { for key, signer in var.mithril_signers : key => index(local.mithril_signers_index, key) + 1 + 9090 }
  mithril_signers_block_producer_cardano_port = { for key, signer in var.mithril_signers : key => index(local.mithril_signers_index, key) + 1 + 10000 }
}

resource "null_resource" "mithril_signer" {
  for_each = var.mithril_signers

  depends_on = [
    null_resource.mithril_aggregator
  ]

  triggers = {
    vm_instance            = google_compute_instance.vm_instance.id,
    cardano_image_id       = var.cardano_image_id,
    cardano_image_registry = var.cardano_image_registry,
    image_id               = var.mithril_image_id,
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
      "mkdir -p /home/curry/data/${var.cardano_network}/mithril-signer-${each.key}/cardano/config",
      "mkdir -p /home/curry/data/${var.cardano_network}/mithril-signer-${each.key}/cardano/db",
      "mkdir -p /home/curry/data/${var.cardano_network}/mithril-signer-${each.key}/cardano/db/passive",
      "mkdir -p /home/curry/data/${var.cardano_network}/mithril-signer-${each.key}/cardano/db/block-producer",
      "mkdir -p /home/curry/data/${var.cardano_network}/mithril-signer-${each.key}/cardano/db/relay",
      "mkdir -p /home/curry/data/${var.cardano_network}/mithril-signer-${each.key}/cardano/ipc",
      "mkdir -p /home/curry/data/${var.cardano_network}/mithril-signer-${each.key}/cardano/pool",
      "mkdir -p /home/curry/data/${var.cardano_network}/mithril-signer-${each.key}/cardano/www",
      "mkdir -p /home/curry/data/${var.cardano_network}/mithril-signer-${each.key}/mithril/stores",
      "mkdir -p /home/curry/data/${var.cardano_network}/mithril-signer-${each.key}/mithril/snapshots",
      "echo -n ${local.mithril_signers_relay_cardano_port[each.key]} > /home/curry/data/${var.cardano_network}/mithril-signer-${each.key}/cardano/pool/port",
      <<-EOT
set -e
# Setup cardano node configuration
CARDANO_NODE_EXACT_VERSION="${var.cardano_image_id}"
CARDANO_NODE_MINOR_VERSION=$(echo $CARDANO_NODE_EXACT_VERSION | cut -d. -f1,2)
CARDANO_NODE_VERSIONS="$CARDANO_NODE_EXACT_VERSION $CARDANO_NODE_MINOR_VERSION"
SIGNER_TYPES="full relay block-producer"
for SIGNER_TYPE in $SIGNER_TYPES; do
  # Copy the cardano node configuration files to the signer and setup topology files (exact version, and fallback to minor version of the Cardano node)
  FOUND_CONFIGURATION=false
  for CARDANO_NODE_VERSION in $CARDANO_NODE_VERSIONS; do
    if [ -d "/home/curry/docker/cardano/config/$CARDANO_NODE_VERSION/${var.cardano_network}" ]; then 
      # Copy the configuration files to the signer
      SIGNER_TYPE_CONFIG_DIRECTORY=/home/curry/data/${var.cardano_network}/mithril-signer-${each.key}/cardano/config/$SIGNER_TYPE
      rm -rf $SIGNER_TYPE_CONFIG_DIRECTORY
      mkdir -p $SIGNER_TYPE_CONFIG_DIRECTORY
      cp -R /home/curry/docker/cardano/config/$CARDANO_NODE_VERSION/${var.cardano_network} $SIGNER_TYPE_CONFIG_DIRECTORY
      echo $CARDANO_NODE_VERSION > $SIGNER_TYPE_CONFIG_DIRECTORY/config.version
      cat $SIGNER_TYPE_CONFIG_DIRECTORY/${var.cardano_network}/cardano-node/config.json | jq ".hasPrometheus[0] |= \"cardano-node-$SIGNER_TYPE-signer-${each.key}\"" > $SIGNER_TYPE_CONFIG_DIRECTORY/${var.cardano_network}/cardano-node/config.json.new
      rm -f $SIGNER_TYPE_CONFIG_DIRECTORY/${var.cardano_network}/cardano-node/config.json
      mv $SIGNER_TYPE_CONFIG_DIRECTORY/${var.cardano_network}/cardano-node/config.json.new $SIGNER_TYPE_CONFIG_DIRECTORY/${var.cardano_network}/cardano-node/config.json
      
      # Setup cardano node block producer topology
      cat /home/curry/docker/cardano/config/$CARDANO_NODE_VERSION/${var.cardano_network}/cardano-node/topology.json | jq 'del(.bootstrapPeers)' | jq 'del(.useLedgerAfterSlot)' | jq '.localRoots[0].accessPoints[0] |= . + { "address": "${google_compute_address.mithril-external-address.address}", "port": ${local.mithril_signers_relay_cardano_port[each.key]}}' > /home/curry/data/${var.cardano_network}/mithril-signer-${each.key}/cardano/pool/topology-block-producer.json

      # Setup cardano node relay topology
      cat /home/curry/docker/cardano/config/$CARDANO_NODE_VERSION/${var.cardano_network}/cardano-node/topology.json > /home/curry/data/${var.cardano_network}/mithril-signer-${each.key}/cardano/pool/topology-relay.json

      FOUND_CONFIGURATION=true
      break
    fi
  done
  # Check if a configuration was found
  if [ "$FOUND_CONFIGURATION" = "false" ]; then
    echo "No cardano node configuration found for version $CARDANO_NODE_EXACT_VERSION of type $SIGNER_TYPE"
    exit 1
  fi
done

EOT
    ]
  }

  provisioner "remote-exec" {
    inline = [
      "export SIGNER_ID=${each.key}",
      "export PARTY_ID=${each.value.pool_id}",
      "export NETWORK=${var.cardano_network}",
      "export CARDANO_IMAGE_ID=${var.cardano_image_id}",
      "export CARDANO_IMAGE_REGISTRY=${var.cardano_image_registry}",
      "export MITHRIL_IMAGE_ID=${var.mithril_image_id}",
      "export AGGREGATOR_CREDENTIALS=${local.mithril_aggregator_credentials}",
      "export SIGNER_HOST=${local.mithril_signers_host[each.key]}",
      "export SIGNER_WWW_PORT=${local.mithril_signers_www_port[each.key]}",
      "export SIGNER_CARDANO_RELAY_ADDR=0.0.0.0",
      "export SIGNER_CARDANO_RELAY_PORT=${local.mithril_signers_relay_cardano_port[each.key]}",
      "export SIGNER_CARDANO_BLOCK_PRODUCER_ADDR=0.0.0.0",
      "export SIGNER_CARDANO_BLOCK_PRODUCER_PORT=${local.mithril_signers_block_producer_cardano_port[each.key]}",
      "export ERA_READER_ADAPTER_TYPE='${var.mithril_era_reader_adapter_type}'",
      "export ERA_READER_ADAPTER_PARAMS=$(jq -nc --arg address $(wget -q -O - ${var.mithril_era_reader_address_url}) --arg verification_key $(wget -q -O - ${var.mithril_era_reader_verification_key_url}) '{\"address\": $address, \"verification_key\": $verification_key}')",
      "export AGGREGATOR_RELAY_LISTEN_PORT='${local.mithril_aggregator_relay_mithril_listen_port}'",
      "export SIGNER_RELAY_LISTEN_PORT='${local.mithril_signers_relay_listen_port[each.key]}'",
      "export SIGNER_RELAY_SERVER_PORT='${local.mithril_signers_relay_server_port[each.key]}'",
      "export SIGNER_RELAY_REGISTRATION_REPEATER_DELAY='${var.mithril_p2p_signer_registration_repeat_delay}'",
      "export ENABLE_METRICS_SERVER=true",
      "export METRICS_SERVER_IP=0.0.0.0",
      "export METRICS_SERVER_PORT=9090",
      "export LOGGING_DRIVER='${var.mithril_container_logging_driver}'",
      "export CURRENT_UID=$(id -u)",
      "export DOCKER_GID=$(getent group docker | cut -d: -f3)",
      "docker compose -p $SIGNER_ID -f /home/curry/docker/docker-compose-signer-${each.value.type}${local.mithril_network_type_suffix}.yaml --profile all up -d",
    ]
  }
}
