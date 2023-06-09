resource "null_resource" "mithril_bootstrap" {

  connection {
    type        = "ssh"
    user        = "curry"
    private_key = local.google_service_account_private_key
    host        = google_compute_address.mithril-external-address.address
  }

  triggers = {
    image_id                                 = var.mithril_image_id,
    vm_instance                              = google_compute_instance.vm_instance.id,
    mithril_aggregator_auth_username         = var.mithril_aggregator_auth_username,
    mithril_aggregator_auth_password         = var.mithril_aggregator_auth_password,
    cardano_configurations_repository_commit = var.cardano_configurations_repository_commit
  }

  provisioner "file" {
    source      = "assets/infra.version"
    destination = "/home/curry/infra.version"
  }

  provisioner "file" {
    source      = "assets/docker"
    destination = "/home/curry"
  }

  provisioner "file" {
    source      = "assets/tools"
    destination = "/home/curry"
  }

  provisioner "remote-exec" {
    inline = [
      <<-EOT
# Wait for VM startup script to complete
while ! test -f "/startup-ready.txt"; do
  sleep 2
  echo "Waiting for startup script to complete..."
done
echo "Startup script complete!"
EOT
      ,
      "find /home/curry/tools -name '*.sh' -type f | xargs chmod u+x",
      <<-EOT
# Clone Cardano configurations repository if not exists
if [ ! -d "/home/curry/docker/cardano-configurations" ] ; then
  git clone https://github.com/input-output-hk/cardano-configurations.git /home/curry/docker/cardano-configurations
fi

# Checkout specific commit
cd /home/curry/docker/cardano-configurations
git checkout .
git checkout master
git pull
git checkout ${var.cardano_configurations_repository_commit}
EOT
      ,
      <<-EOT
# Deactivate P2P in preview
CONFIG_JSON=$(cat /home/curry/docker/cardano-configurations/network/preview/cardano-node/config.json | jq '.EnableP2P = false') && echo $CONFIG_JSON > /home/curry/docker/cardano-configurations/network/preview/cardano-node/config.json
cat > /home/curry/docker/cardano-configurations/network/preview/cardano-node/topology.json << EOF 
{
  "Producers": [
    {
      "addr": "preview-node.world.dev.cardano.org",
      "port": 30002,
      "valency": 1
    }
  ]
}
EOF

# Deactivate P2P in preprod
CONFIG_JSON=$(cat /home/curry/docker/cardano-configurations/network/preprod/cardano-node/config.json | jq '.EnableP2P = false') && echo $CONFIG_JSON > /home/curry/docker/cardano-configurations/network/preprod/cardano-node/config.json
cat > /home/curry/docker/cardano-configurations/network/preprod/cardano-node/topology.json << EOF 
{
  "Producers": [
    {
      "addr": "preprod-node.world.dev.cardano.org",
      "port": 30000,
      "valency": 1
    }
  ]
}
EOF

EOT

    ]
  }
}
