resource "null_resource" "mithril-aggregator" {

  # trigger a deployment of the aggregator whwen the
  # image_id is updated
  triggers = {
    image_id = var.image_id
  }

  connection {
    type        = "ssh"
    user        = "curry"
    private_key = var.private_key
    host        = google_compute_instance.mithril-aggregator-testnet.network_interface.0.access_config.0.nat_ip
  }

  provisioner "file" {
    source      = "docker-compose.yaml"
    destination = "/home/curry/docker-compose.yaml"
  }

  # logs shipment to grafana cloud
  provisioner "file" {
    source      = "promtail-config.yml"
    destination = "/home/curry/promtail-config.yml"
  }

  # prometheus configuration file, used in the docker-compose file
  provisioner "file" {
    source      = "prometheus.yml"
    destination = "/home/curry/prometheus.yml"
  }

  provisioner "remote-exec" {
    inline = [
      "IMAGE_ID=${var.image_id} GOOGLE_APPLICATION_CREDENTIALS_JSON='${var.google_application_credentials_json}' CURRENT_UID=$(id -u)  docker-compose -f /home/curry/docker-compose.yaml up -d"
    ]
  }
}
