
resource "google_compute_attached_disk" "data" {
  depends_on = [
    google_compute_instance.vm_instance,
    google_compute_disk.data
  ]

  disk        = google_compute_disk.data.id
  instance    = google_compute_instance.vm_instance.id
  device_name = "mithril-data-disk"
}

resource "google_compute_disk" "data" {
  depends_on = [
    google_compute_instance.vm_instance
  ]

  name     = "${local.environment_name}-data"
  type     = var.google_compute_instance_data_disk_type
  zone     = var.google_zone
  size     = var.google_compute_instance_data_disk_size
  snapshot = var.google_compute_instance_data_disk_snapshot
  labels = {
    environment = local.environment_name
    type        = "data"
  }
}

resource "google_compute_resource_policy" "policy-data" {
  name   = "${local.environment_name}-policy-data"
  region = var.google_region
  snapshot_schedule_policy {
    schedule {
      daily_schedule {
        days_in_cycle = var.google_compute_instance_data_disk_snapshot_pace_days
        start_time    = var.google_compute_instance_data_disk_snapshot_start_time
      }
    }
    retention_policy {
      max_retention_days    = var.google_compute_instance_data_disk_snapshot_max_retention_days
      on_source_disk_delete = "KEEP_AUTO_SNAPSHOTS"
    }
  }
}

resource "google_compute_disk_resource_policy_attachment" "policy-attachment-data" {
  name = google_compute_resource_policy.policy-data.name
  disk = google_compute_disk.data.name
  zone = var.google_zone
}

resource "null_resource" "mithril_mount_data_disk" {
  depends_on = [
    google_compute_attached_disk.data
  ]

  triggers = {
    attached_disk = google_compute_attached_disk.data.id
  }

  connection {
    type        = "ssh"
    user        = "curry"
    private_key = local.google_service_account_private_key
    host        = google_compute_address.mithril-external-address.address
  }

  provisioner "remote-exec" {
    inline = [
      <<-EOT
set -e
# Format data disk if necessary
if sudo blkid /dev/sdb; then 
  echo "Data disk already formatted"
else 
  # Format data disk
  echo "Format data disk"
  sudo mkfs.ext4 -m 0 -E lazy_itable_init=0,lazy_journal_init=0,discard /dev/disk/by-id/google-mithril-data-disk
fi

# Mount data disk
echo "Mount data disk"
mkdir -p /home/curry/data
sudo mount -o discard,defaults /dev/disk/by-id/google-mithril-data-disk /home/curry/data

# Update rights of data directory
sudo chown "curry":"curry" /home/curry/data -R
echo "Data disk mounted!"
    EOT
    ]
  }
}
