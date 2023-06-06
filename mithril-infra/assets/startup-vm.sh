#!/bin/bash

# Remove /startup-ready.txt
rm -f /startup-ready.txt

# Update and install dependencies
sudo apt update -y
sudo apt upgrade -y
sudo apt install -y jq tree ca-certificates curl gnupg lsb-release apache2-utils

# Install sqlite3
curl http://ftp.fr.debian.org/debian/pool/main/s/sqlite3/libsqlite3-0_3.40.1-1_amd64.deb -o libsqlite3-0_3.40.1-1_amd64.deb
dpkg -i libsqlite3-0_3.40.1-1_amd64.deb
rm -f libsqlite3-0_3.40.1-1_amd64.deb
curl http://ftp.fr.debian.org/debian/pool/main/s/sqlite3/sqlite3_3.40.1-1_amd64.deb -o sqlite3_3.40.1-1_amd64.deb
dpkg -i sqlite3_3.40.1-1_amd64.deb
rm -f sqlite3_3.40.1-1_amd64.deb

# Install docker & docker-compose
sudo mkdir -p /etc/apt/keyrings
rm -f /etc/apt/keyrings/docker.gpg
curl -fsSL https://download.docker.com/linux/ubuntu/gpg | sudo gpg --dearmor -o /etc/apt/keyrings/docker.gpg
echo \
  "deb [arch=$(dpkg --print-architecture) signed-by=/etc/apt/keyrings/docker.gpg] https://download.docker.com/linux/ubuntu \
  $(lsb_release -cs) stable" | sudo tee /etc/apt/sources.list.d/docker.list > /dev/null
sudo apt update -y
sudo apt install -y docker-ce docker-ce-cli containerd.io docker-compose-plugin docker-compose
sudo usermod -aG docker curry
sudo chown "curry":"curry" /home/curry/.docker -R
sudo chmod g+rwx "/home/curry/.docker" -R
sudo service docker start

# Add /startup-ready.txt
touch /startup-ready.txt
