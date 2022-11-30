resource "null_resource" "mithril_bootstrap" {

  /*depends_on = [
    null_resource.vm_startup
  ]*/

  connection {
    type        = "ssh"
    user        = "curry"
    private_key = local.google_service_account_private_key
    host        = google_compute_address.mithril-external-address.address
  }

  triggers = {
    image_id = var.mithril_image_id
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
if [[ ! -f '/home/curry/docker/cardano-configurations' ]] ; then
  git clone https://github.com/input-output-hk/cardano-configurations.git /home/curry/docker/cardano-configurations
fi
EOT
    ]
  }
}
