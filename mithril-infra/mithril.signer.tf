locals {
  mithril_signers_index                       = [for key, signer in var.mithril_signers : key]
  mithril_signers_www_port                    = { for key, signer in var.mithril_signers : key => index(local.mithril_signers_index, key) + 1 + 8080 }
  mithril_signers_relay_cardano_port          = { for key, signer in var.mithril_signers : key => index(local.mithril_signers_index, key) + 1 + 9090 }
  mithril_signers_block_producer_cardano_port = { for key, signer in var.mithril_signers : key => index(local.mithril_signers_index, key) + 1 + 10000 }
}
resource "null_resource" "mithril_signer" {
  for_each = var.mithril_signers

  depends_on = [
    null_resource.mithril_aggregator
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
    ]
  }

  provisioner "remote-exec" {
    inline = [
      "export SIGNER_ID=${each.key}",
      "export PARTY_ID=${each.value.pool_id}",
      "export NETWORK=${var.cardano_network}",
      "export CARDANO_IMAGE_ID=${var.cardano_image_id}",
      "export MITHRIL_IMAGE_ID=${var.mithril_image_id}",
      "export SIGNER_HOST=${local.mithril_signers_host[each.key]}",
      "export SIGNER_WWW_PORT=${local.mithril_signers_www_port[each.key]}",
      "export SIGNER_CARDANO_RELAY_ADDR=0.0.0.0",
      "export SIGNER_CARDANO_RELAY_PORT=${local.mithril_signers_relay_cardano_port[each.key]}",
      "export SIGNER_CARDANO_BLOCK_PRODUCER_ADDR=0.0.0.0",
      "export SIGNER_CARDANO_BLOCK_PRODUCER_PORT=${local.mithril_signers_block_producer_cardano_port[each.key]}",
      "export ERA_READER_ADAPTER_TYPE='${var.mithril_era_reader_adapter_type}'",
      "export ERA_READER_ADAPTER_PARAMS=$(jq -nc --arg address $(wget -q -O - ${var.mithril_era_reader_address_url}) --arg verification_key $(wget -q -O - ${var.mithril_era_reader_verification_key_url}) '{\"address\": $address, \"verification_key\": $verification_key}')",
      "export CURRENT_UID=$(id -u)",
      "export DOCKER_GID=$(getent group docker | cut -d: -f3)",
      "docker-compose -p $SIGNER_ID -f /home/curry/docker/docker-compose-signer-${each.value.type}.yaml --profile all up -d",
    ]
  }
}
