#!/bin/bash -e
# configure Ubuntu-based machines with everything needed for running
# docker containers through docker-compose

# ensure apt does not try to 'Dialog' with a user
export DEBIAN_FRONTEND=noninteractive

# Update the package list for basic stuff
sudo -E apt-get update
sudo apt install -y apt-transport-https apt-utils ca-certificates curl software-properties-common

# install neovim
sudo add-apt-repository ppa:neovim-ppa/stable

# install Docker
curl -fsSL https://download.docker.com/linux/ubuntu/gpg | sudo gpg --dearmor -o /usr/share/keyrings/docker-archive-keyring.gpg
echo \
  "deb [arch=$(dpkg --print-architecture) signed-by=/usr/share/keyrings/docker-archive-keyring.gpg] https://download.docker.com/linux/ubuntu \
  $(lsb_release -cs) stable" | sudo tee /etc/apt/sources.list.d/docker.list > /dev/null

# Update the package list
sudo -E apt-get update

# Ugrade image
sudo -E apt-get upgrade -y

# TODO trim down the list of packages to install as most of them should be provided by nix
sudo -E apt install -y rsync git bzip2 containerd.io curl docker-ce docker-ce-cli gnupg2 inotify-tools jq language-pack-en  neovim ripgrep tmux wget

# install docker-compose
sudo curl -L "https://github.com/docker/compose/releases/download/1.29.2/docker-compose-$(uname -s)-$(uname -m)" -o /usr/local/bin/docker-compose
sudo chmod +x /usr/local/bin/docker-compose

# give user curry access to docker socket (should use TLS?)
sudo adduser curry docker

# prefer ipv4 connections over ipv6
echo "precedence ::ffff:0:0/96  100" | sudo tee -a /etc/gai.conf

# this is needed for proper gpg-agent forwarding to work
sudo tee -a /etc/ssh/sshd_config > /dev/null <<EOF
StreamLocalBindUnlink yes
EOF

# configure unattended upgrades
distro_id=$(lsb_release -is)
distro_codename=$(lsb_release -cs)

cat | sudo tee /etc/apt/apt.conf.d/50unattended-upgrades <<EOF
Unattended-Upgrade::Allowed-Origins {
  "${distro_id}:${distro_codename}";
  "${distro_id}:${distro_codename}-security";
  "${distro_id}:${distro_codename}-updates";
  "${distro_id}ESM:${distro_codename}";
}
Unattended-Upgrade::Remove-Unused-Kernel-Packages "true";
Unattended-Upgrade::Remove-Unused-Dependencies "true";
Unattended-Upgrade::Automatic-Reboot "true";
Unattended-Upgrade::Automatic-Reboot-Time "05:38";
EOF

cat | sudo tee /etc/apt/apt.conf.d/20auto-upgrades <<EOF
APT::Periodic::Update-Package-Lists "1";
APT::Periodic::Download-Upgradeable-Packages "1";
APT::Periodic::AutocleanInterval "7";
APT::Periodic::Unattended-Upgrade "1";
EOF
