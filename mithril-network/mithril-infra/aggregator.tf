resource "null_resource" "mithril-aggregator" {

  # trigger a deployment of the aggregator whwen the
  # image_id is updated
  triggers = {
    image_id = var.image_id
  }

  provisioner "file" {
    source      = "docker-compose.yaml"
    destination = "/home/curry/docker-compose.yaml"

    connection {
      type = "ssh"
      user = "curry"
      host = google_compute_instance.mithril-aggregator-testnet.network_interface.0.access_config.0.nat_ip
    }
  }

  # logs shipment to grafana cloud
  provisioner "file" {
    source      = "promtail-config.yml"
    destination = "/home/curry/promtail-config.yml"

    connection {
      type = "ssh"
      user = "curry"
      host = google_compute_instance.mithril-aggregator-testnet.network_interface.0.access_config.0.nat_ip
    }
  }

  # prometheus configuration file, used in the docker-compose file
  provisioner "file" {
    source      = "prometheus.yml"
    destination = "/home/curry/prometheus.yml"

    connection {
      type = "ssh"
      user = "curry"
      host = google_compute_instance.mithril-aggregator-testnet.network_interface.0.access_config.0.nat_ip
    }
  }
  provisioner "remote-exec" {
    inline = [
      "usermod -G docker curry"
    ]

    connection {
      type = "ssh"
      user = "root"
      host = google_compute_instance.mithril-aggregator-testnet.network_interface.0.access_config.0.nat_ip
    }
  }

  provisioner "remote-exec" {
    inline = [
      "IMAGE_ID=${var.image_id} docker-compose -f /home/curry/docker-compose.yaml up -d"
    ]

    connection {
      type = "ssh"
      user = "root"
      host = google_compute_instance.mithril-aggregator-testnet.network_interface.0.access_config.0.nat_ip
    }
  }
}
