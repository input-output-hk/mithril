#!/bin/sh

set -e

# Function to display usage
usage() {
  echo "Install or upgrade a Mithril node"
  echo "Usage: $0 [-n node] [-v version] [-d distribution] [-p path]"
  echo "  -c node          : Mithril node to install or upgrade (mithril-signer, mithril-aggregator, mithril-client)"
  echo "  -d distribution  : Distribution to upgrade to (latest, unstable or distribution version e.g '2445.0')"
  echo "  -p path          : Path to install the component"
  exit 1
}

# Default values
DISTRIBUTION="latest"
GITHUB_ORGANIZATION="input-output-hk"
GITHUB_REPOSITORY="mithril"

# Function to display an error message and exit
error_exit() {
  echo "$1" 1>&2
  exit 1
}

# Parse command line arguments
while getopts "c:v:d:p:" opt; do
  case ${opt} in
    c )
      NODE=$OPTARG
      ;;
    d )
      DISTRIBUTION=$OPTARG
      ;;
    p )
      INSTALL_PATH=$OPTARG
      ;;
    * )
      usage
      ;;
  esac
done

# Detect the operating system
OS=$(uname -s)
OS_CODE=$(echo "$OS" | awk '{print tolower($0)}')

case "$OS" in
  Linux)
    ;;
  Darwin)
    ;;
  *)
    error_exit "Unsupported operating system $OS for $NODE"
    ;;
esac

# Detect the architecture
ARCH=$(uname -m)

case "$ARCH" in
  x86_64)
    ARCH_NAME="x64"
    ;;
  aarch64)
    ARCH_NAME="arm64"
    ;;
  *)
    error_exit "Unsupported architecture: $ARCH"
    ;;
esac

# Set temp file
TEMP_FILE="${INSTALL_PATH}/temp.json"

# Check if node and path are provided
if [ -z "$NODE" ] || [ -z "$DISTRIBUTION" ] || [ -z "$INSTALL_PATH" ]; then
  usage
fi

# Validate node
if [ "$NODE" != "mithril-signer" ] && [ "$NODE" != "mithril-aggregator" ] && [ "$NODE" != "mithril-client" ]; then
  echo "Invalid node: $NODE"
  usage
fi

# Determine the URL for the specified tag
if [ "$DISTRIBUTION" = "latest" ]; then
  RELEASE_URL="https://api.github.com/repos/${GITHUB_ORGANIZATION}/${GITHUB_REPOSITORY}/releases/latest"
else
  RELEASE_URL="https://api.github.com/repos/${GITHUB_ORGANIZATION}/${GITHUB_REPOSITORY}/releases/tags/${DISTRIBUTION}"
fi

# Fetch the release information
echo "Fetching release information from $RELEASE_URL..."
curl --fail -sL -o "$TEMP_FILE" "$RELEASE_URL" || error_exit "Error: Failed to fetch the release information. This probably means that the distribution you provided does not exist."

# Find valid binary for the node
ASSETS_DOWNLOAD_URL=$(jq -r --arg os_code "$OS_CODE" --arg arch_name "$ARCH_NAME" '.assets[] | select(.name | contains($os_code)) | select(.name | contains($arch_name)) | .browser_download_url' < "$TEMP_FILE")
rm -f "$TEMP_FILE"
if [ -z "$ASSETS_DOWNLOAD_URL" ]; then
  error_exit "Error: Failed to fetch assets from release. This probably means that your OS and architecture are not supported."
fi

# Create the install path if it doesn't exist
mkdir -p "$INSTALL_PATH"

# Download the distribution assets
echo "Downloading $NODE to $DISTRIBUTION from $ASSETS_DOWNLOAD_URL..."
ASSETS_FILE_PATH="$INSTALL_PATH/mithril-assets.tar.gz"
curl --fail -sL -o "$ASSETS_FILE_PATH" "$ASSETS_DOWNLOAD_URL" || error_exit "Error: Failed to download the asset file from the release"

# Search for a node file
NODE_FILE_NAME=$(tar -tzf "$ASSETS_FILE_PATH" | grep "$NODE" | awk '{print $NF}')
if [ -z "$NODE_FILE_NAME" ]; then
  error_exit "Error: Failed to find $NODE binary in the distribution asset. This probably means that your OS and architecture are not supported."
fi

tar -xz -f "$ASSETS_FILE_PATH" -C "$INSTALL_PATH" "$NODE"
chmod +x "$INSTALL_PATH/$NODE"
rm -f "$ASSETS_FILE_PATH"

VERSION=$("$INSTALL_PATH/$NODE" --version | awk '{print $NF}')
echo "Congrats! ${NODE} has been upgraded to ${VERSION} from distribution ${DISTRIBUTION} and installed at ${INSTALL_PATH}!"