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
# Setup cardano node topology
cat > /home/curry/data/${var.cardano_network}/mithril-signer-${each.key}/cardano/pool/topology-block-producer.json << EOF
{
  "Producers": [
    {
      "addr": "${google_compute_address.mithril-external-address.address}",
      "port": ${local.mithril_signers_relay_cardano_port[each.key]},
      "valency": 1
    }
  ]
}
EOF
cat /home/curry/docker/cardano-configurations/network/${var.cardano_network}/cardano-node/topology.json | jq '.Producers[1] |= . + { "addr": "${google_compute_address.mithril-external-address.address}", "port": ${local.mithril_signers_block_producer_cardano_port[each.key]}, "valency": 1}' > /home/curry/data/${var.cardano_network}/mithril-signer-${each.key}/cardano/pool/topology-relay.json
EOT
      ,
      <<-EOT
# Setup cardano node configuration
SIGNER_TYPES="full relay block-producer"
for SIGNER_TYPE in $SIGNER_TYPES; do
  SIGNER_TYPE_CONFIG_DIRECTORY=/home/curry/data/${var.cardano_network}/mithril-signer-${each.key}/cardano/config/$SIGNER_TYPE
  mkdir -p $SIGNER_TYPE_CONFIG_DIRECTORY
  cp -R /home/curry/docker/cardano-configurations/network/${var.cardano_network} $SIGNER_TYPE_CONFIG_DIRECTORY
  cat $SIGNER_TYPE_CONFIG_DIRECTORY/${var.cardano_network}/cardano-node/config.json | jq ".hasPrometheus[0] |= \"cardano-node-$SIGNER_TYPE-signer-${each.key}\"" > $SIGNER_TYPE_CONFIG_DIRECTORY/${var.cardano_network}/cardano-node/config.json.new
  rm -f $SIGNER_TYPE_CONFIG_DIRECTORY/${var.cardano_network}/cardano-node/config.json
  mv $SIGNER_TYPE_CONFIG_DIRECTORY/${var.cardano_network}/cardano-node/config.json.new $SIGNER_TYPE_CONFIG_DIRECTORY/${var.cardano_network}/cardano-node/config.json
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
      "export LOGGING_DRIVER='${var.mithril_container_logging_driver}'",
      "export CURRENT_UID=$(id -u)",
      "export DOCKER_GID=$(getent group docker | cut -d: -f3)",
      "docker compose -p $SIGNER_ID -f /home/curry/docker/docker-compose-signer-${each.value.type}${local.mithril_network_type_suffix}.yaml --profile all up -d",
    ]
  }
}
