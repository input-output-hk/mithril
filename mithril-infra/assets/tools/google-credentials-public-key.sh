#!/bin/bash

GOOGLE_CREDENTIALS_FILE=$1
SSH_KEYS_FILE=$2
VM_USER=$3

# Create a public key from the credentials file and append it to the ssh_keys file
cat $GOOGLE_CREDENTIALS_FILE | jq -r .private_key > temp.sk
CREDENTIALS_EMAIL=$(cat $GOOGLE_CREDENTIALS_FILE | jq -r .client_email)
chmod 0600 temp.sk
ssh-keygen -q -t rsa -y -f temp.sk -C $CREDENTIALS_EMAIL -b 2048 > temp.pub 
PUBLIC_KEY=$(cat temp.pub)
rm -f temp.{sk,pub}

# Add entry to ssh_keys file
echo $VM_USER:$PUBLIC_KEY $CREDENTIALS_EMAIL >> $SSH_KEYS_FILE