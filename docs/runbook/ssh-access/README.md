# Manage SSH access to infrastructure

## Add access to a user

### Create a SSH keypair for a user (if needed)

Create a new SSH keypair, with `ed25519` cryptography for maximum security:
```bash
ssh-keygen -t ed25519 -C "your_email@example.com"
```

Then, add your keypair to the ssh-agent:
```bash
ssh-add ~/.ssh/id_ed25519
```

### Retrieve the public key of your SSH keypair

Run the following command to retrieve your public key:
```bash
cat ~/.ssh/id_ed25519.pub
```

### Declare the public key

Add a line with the format `**REMOTE_USER**:*PUBLIC_KEY**` in the `mithril-infra/assets/ssh_keys` file for each:
```bash
echo "curry:ssh-ed25519 AAAE53AC3NzQ2vlZDI1aC1O4CpX+S2y1X9NTB4rv4k3pAAAAIF3b7L9sPV5ZiGgogmko your_email@example.com" >> **REPOSITORY_PATH**/mithril-infra/assets/ssh_keys
```

Then, create a PR with the updated `ssh_keys` file.

## Remove access to a user

To remove an access, simply remove the line(s) related to this user.

Then, create a PR with the updated `ssh_keys` file.

## When are the modifications applied?

The modifications will be applied the next time the terraform deployment is done:
- next **merge** in `main` branch for `testing-preview`
- next **pre-release** created for `pre-release-preview`
- next **release** created for `release-preprod`
- next **release** created for `release-mainnet`

When the modifications are applied, the VM is updated in place by terraform.

:warning: In case of emergency, the SSH keys can be modified by an administrator:
- In GCP [**Compute Engine**](https://console.cloud.google.com/compute/instances)
- The SSH keys can be edited in the targeted VM(s)