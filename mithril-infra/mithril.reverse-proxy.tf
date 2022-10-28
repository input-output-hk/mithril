resource "null_resource" "mithril_reverse_proxy" {
  depends_on = [
    null_resource.mithril_bootstrap
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
      "chmod 600 /home/curry/docker/traefik/acme.json",
    ]
  }

  provisioner "remote-exec" {
    inline = [
      "docker network inspect mithril_network >/dev/null 2>&1 || docker network create mithril_network",
      "export CURRENT_UID=$(id -u)",
      "export DOCKER_GID=$(getent group docker | cut -d: -f3)",
      "docker-compose -f /home/curry/docker/docker-compose-reverse-proxy.yaml --profile all up -d",
    ]
  }
}
