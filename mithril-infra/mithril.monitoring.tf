
resource "null_resource" "mithril_monitoring" {
  depends_on = [
    null_resource.mithril_reverse_proxy
  ]

  triggers = {
    vm_instance = google_compute_instance.vm_instance.id
  }

  connection {
    type        = "ssh"
    user        = "curry"
    private_key = local.google_service_account_private_key
    host        = google_compute_address.mithril-external-address.address
  }

  provisioner "remote-exec" {
    inline = [
      "docker-compose -f /home/curry/docker/docker-compose-monitoring.yaml --profile all up -d",
    ]
  }
}
