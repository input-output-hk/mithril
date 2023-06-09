resource "null_resource" "mithril_network" {
  depends_on = [
    null_resource.mithril_bootstrap,
    null_resource.mithril_mount_data_disk
  ]

  triggers = {
    image_id    = var.mithril_image_id,
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
      "docker network inspect mithril_network >/dev/null 2>&1 || docker network create mithril_network"
    ]
  }
}
