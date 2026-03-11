#!/usr/bin/env bash

# Mithril Backward Compatibility - Local Testing Tool
#
# Reproduces the CI backward compatibility tests locally, with support for
# custom builds (e.g. with the future_snark feature) and cross-platform
# operation via mithril-install.sh.
#
# Usage:
#   ./mithril-backward-compatibility.sh <command> [options]
#
# Commands:
#   install  - Download released binaries for given distributions
#   build    - Build binaries from the local workspace with optional features
#   list     - List available binary sets in the local store
#   test     - Run backward compatibility E2E tests
#   report   - Generate a compatibility report from test results

set -euo pipefail

# ---------------------------------------------------------------------------
# Constants
# ---------------------------------------------------------------------------
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"
INSTALL_SCRIPT_URL="https://raw.githubusercontent.com/input-output-hk/mithril/refs/heads/main/mithril-install.sh"
DEVNET_SCRIPTS_DIR="$SCRIPT_DIR/../mithril-devnet"
DEFAULT_STORE_DIR="$SCRIPT_DIR/backward-compatibility-binaries"
NODES=(mithril-aggregator mithril-signer mithril-client mithril-relay)
INSTALLABLE_NODES=(mithril-aggregator mithril-signer mithril-client)
GITHUB_ORGANIZATION="input-output-hk"
GITHUB_REPOSITORY="mithril"

# Defaults matching the CI workflow
DEFAULT_TOTAL_RELEASES=3
DEFAULT_CARDANO_NODE_VERSION="10.6.2"
DEFAULT_SIGNED_ENTITY_TYPES="CardanoTransactions,CardanoStakeDistribution,CardanoDatabase,CardanoImmutableFilesFull"
DEFAULT_CARDANO_SLOT_LENGTH="0.25"
DEFAULT_CARDANO_EPOCH_LENGTH="45.0"
DEFAULT_MAX_ATTEMPTS=3

# Colors for terminal output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
BOLD='\033[1m'
RESET='\033[0m'

# ---------------------------------------------------------------------------
# Utility functions
# ---------------------------------------------------------------------------

print_header() {
  echo ""
  echo -e "${BOLD}${BLUE}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${RESET}"
  echo -e "${BOLD}${BLUE}  $1${RESET}"
  echo -e "${BOLD}${BLUE}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${RESET}"
  echo ""
}

print_step() {
  echo -e "${BOLD}>> $1${RESET}"
}

print_success() {
  echo -e "${GREEN}[OK]${RESET} $1"
}

print_warning() {
  echo -e "${YELLOW}[WARN]${RESET} $1"
}

print_error() {
  echo -e "${RED}[ERROR]${RESET} $1"
}

error_exit() {
  print_error "$1"
  exit 1
}

check_requirements() {
  local missing=()
  for tool in curl jq; do
    if ! command -v "$tool" &>/dev/null; then
      missing+=("$tool")
    fi
  done
  if [[ ${#missing[@]} -gt 0 ]]; then
    error_exit "Missing required tools: ${missing[*]}"
  fi
}

ensure_store_directory() {
  local store_directory="$1"
  mkdir -p "$store_directory"
}

# Download the mithril-install.sh script to a temporary location and return its path
download_install_script() {
  local install_script_path
  install_script_path=$(mktemp "${TMPDIR:-/tmp}/mithril-install.XXXXXX.sh")
  curl --proto '=https' --tlsv1.2 -sSf "$INSTALL_SCRIPT_URL" -o "$install_script_path" \
    || error_exit "Failed to download mithril-install.sh from $INSTALL_SCRIPT_URL"
  chmod +x "$install_script_path"
  echo "$install_script_path"
}

# Fetch the N most recent non-prerelease release tags from the GitHub API
fetch_latest_release_tags() {
  local count="$1"
  local exclude_tag="${2:-}"

  local api_url="https://api.github.com/repos/${GITHUB_ORGANIZATION}/${GITHUB_REPOSITORY}/releases?per_page=50"
  local releases
  releases=$(curl --fail -sL "$api_url") || error_exit "Failed to fetch releases from GitHub API"

  local tags
  tags=$(echo "$releases" | jq -r '[.[] | select(.prerelease == false)][].tag_name' | head -n "$count")

  local filtered_tags=()
  for tag in $tags; do
    if [[ -n "$exclude_tag" && "$tag" == "$exclude_tag" ]]; then
      continue
    fi
    filtered_tags+=("$tag")
  done

  printf '%s\n' "${filtered_tags[@]}"
}

# Extract the relay binary from a GitHub release tarball (mithril-install.sh does not support relay)
install_relay_from_release() {
  local distribution="$1"
  local install_path="$2"

  local os_code
  os_code=$(uname -s | tr '[:upper:]' '[:lower:]')
  case "$os_code" in
    darwin) os_code="macos" ;;
    linux) ;;
    *) print_warning "Unsupported OS for relay download: $os_code"; return 1 ;;
  esac

  local arch
  arch=$(uname -m)
  local arch_name
  case "$arch" in
    x86_64) arch_name="x64" ;;
    arm64|aarch64) arch_name="arm64" ;;
    *) print_warning "Unsupported architecture for relay download: $arch"; return 1 ;;
  esac

  local release_url
  if [[ "$distribution" == "latest" ]]; then
    release_url="https://api.github.com/repos/${GITHUB_ORGANIZATION}/${GITHUB_REPOSITORY}/releases/latest"
  else
    release_url="https://api.github.com/repos/${GITHUB_ORGANIZATION}/${GITHUB_REPOSITORY}/releases/tags/${distribution}"
  fi

  local release_json
  release_json=$(curl --fail -sL "$release_url") || {
    print_warning "Failed to fetch release info for relay (distribution: $distribution)"
    return 1
  }

  local download_url
  download_url=$(echo "$release_json" | jq -r --arg os "$os_code" --arg arch "$arch_name" \
    '.assets[] | select(.name | contains($os)) | select(.name | contains($arch)) | .browser_download_url')

  if [[ -z "$download_url" ]]; then
    print_warning "No relay asset found for $os_code-$arch_name in distribution $distribution"
    return 1
  fi

  local temp_archive="$install_path/mithril-relay-archive.tar.gz"
  curl --fail -sL -o "$temp_archive" "$download_url" || {
    print_warning "Failed to download relay archive"
    rm -f "$temp_archive"
    return 1
  }

  if tar -tzf "$temp_archive" | grep -q "mithril-relay"; then
    tar -xzf "$temp_archive" -C "$install_path" mithril-relay
    chmod +x "$install_path/mithril-relay"
    rm -f "$temp_archive"
    return 0
  else
    print_warning "mithril-relay not found in archive for distribution $distribution"
    rm -f "$temp_archive"
    return 1
  fi
}

# ---------------------------------------------------------------------------
# Command: install
# ---------------------------------------------------------------------------
usage_install() {
  cat <<EOF
Usage: $(basename "$0") install [options]

Download released binaries for one or more distributions.

Options:
  -d, --distribution <tag>   Distribution to download (e.g. 'latest', 'unstable', '2543.0')
                              Can be specified multiple times.
  -n, --total-releases <N>   Download the N most recent stable releases (default: $DEFAULT_TOTAL_RELEASES)
  -s, --store-directory <path>  Directory for storing binaries (default: $DEFAULT_STORE_DIR)
  -h, --help                 Show this help message

Examples:
  $(basename "$0") install -d unstable -d 2543.0 -d 2524.0 --store-directory ./store
  $(basename "$0") install --total-releases 3 --store-directory ./store
EOF
  exit 0
}

command_install() {
  local distributions=()
  local total_releases=""
  local store_directory="$DEFAULT_STORE_DIR"

  while [[ $# -gt 0 ]]; do
    case "$1" in
      -d|--distribution) distributions+=("$2"); shift 2 ;;
      -n|--total-releases) total_releases="$2"; shift 2 ;;
      -s|--store-directory) store_directory="$2"; shift 2 ;;
      -h|--help) usage_install ;;
      *) error_exit "Unknown option for install: $1" ;;
    esac
  done

  check_requirements

  if [[ -z "$total_releases" && ${#distributions[@]} -eq 0 ]]; then
    total_releases="$DEFAULT_TOTAL_RELEASES"
  fi

  # If total-releases is set, fetch the latest release tags
  if [[ -n "$total_releases" ]]; then
    print_step "Fetching the $total_releases most recent stable release tags..."
    local release_tags
    mapfile -t release_tags < <(fetch_latest_release_tags "$total_releases")
    for tag in "${release_tags[@]}"; do
      # Avoid duplicates
      local already_listed=false
      for existing in "${distributions[@]}"; do
        if [[ "$existing" == "$tag" ]]; then
          already_listed=true
          break
        fi
      done
      if [[ "$already_listed" == "false" ]]; then
        distributions+=("$tag")
      fi
    done
  fi

  if [[ ${#distributions[@]} -eq 0 ]]; then
    error_exit "No distributions to install. Use -d or -n to specify."
  fi

  ensure_store_directory "$store_directory"

  print_header "Installing Mithril binaries"
  echo "Distributions: ${distributions[*]}"
  echo "Store: $store_directory"
  echo ""

  print_step "Downloading mithril-install.sh..."
  local install_script
  install_script=$(download_install_script)
  trap "rm -f '$install_script'" EXIT
  print_success "mithril-install.sh downloaded to $install_script"
  echo ""

  for distribution in "${distributions[@]}"; do
    local target_directory="$store_directory/$distribution"
    print_step "Installing distribution: $distribution -> $target_directory"

    mkdir -p "$target_directory"

    # Install each supported node via the downloaded mithril-install.sh
    for node in "${INSTALLABLE_NODES[@]}"; do
      echo "  Installing $node..."
      if bash "$install_script" -c "$node" -d "$distribution" -p "$target_directory" 2>&1 | sed 's/^/    /'; then
        print_success "  $node installed"
      else
        print_warning "  $node failed to install for distribution $distribution (may not exist in this release)"
      fi
    done

    # Install relay from tarball (not supported by mithril-install.sh)
    echo "  Installing mithril-relay..."
    if install_relay_from_release "$distribution" "$target_directory" 2>&1 | sed 's/^/    /'; then
      print_success "  mithril-relay installed"
    else
      print_warning "  mithril-relay not available for distribution $distribution"
    fi

    echo ""
  done

  # Write a tags.json file listing all installed distributions
  local tags_json="$store_directory/tags.json"
  printf '%s\n' "${distributions[@]}" | jq -R -s -c 'split("\n") | map(select(. != ""))' > "$tags_json"
  print_success "Installed distributions recorded in $tags_json"
}

# ---------------------------------------------------------------------------
# Command: build
# ---------------------------------------------------------------------------
usage_build() {
  cat <<EOF
Usage: $(basename "$0") build [options]

Build binaries from the local workspace with optional Cargo features.

Options:
  -l, --label <name>         Label for this build (e.g. 'local', 'future-snark') (required)
  -f, --features <features>  Comma-separated Cargo features (e.g. 'future_snark,bundle_tls')
  -s, --store-directory <path>  Directory for storing binaries (default: $DEFAULT_STORE_DIR)
  --build-e2e                Also build the mithril-end-to-end binary
  -h, --help                 Show this help message

Examples:
  $(basename "$0") build --label local --store-directory ./store
  $(basename "$0") build --label future-snark --features future_snark --store-directory ./store
  $(basename "$0") build --label future-snark --features future_snark --build-e2e --store-directory ./store
EOF
  exit 0
}

command_build() {
  local label=""
  local features=""
  local store_directory="$DEFAULT_STORE_DIR"
  local build_e2e=false

  while [[ $# -gt 0 ]]; do
    case "$1" in
      -l|--label) label="$2"; shift 2 ;;
      -f|--features) features="$2"; shift 2 ;;
      -s|--store-directory) store_directory="$2"; shift 2 ;;
      --build-e2e) build_e2e=true; shift ;;
      -h|--help) usage_build ;;
      *) error_exit "Unknown option for build: $1" ;;
    esac
  done

  if [[ -z "$label" ]]; then
    error_exit "A --label is required for the build command"
  fi

  local target_directory="$store_directory/$label"
  ensure_store_directory "$target_directory"

  print_header "Building Mithril binaries"
  echo "Label:    $label"
  echo "Features: ${features:-<default>}"
  echo "Target:   $target_directory"
  echo ""

  local cargo_features_flag=""
  if [[ -n "$features" ]]; then
    cargo_features_flag="--features $features"
  fi

  # Build the four main binaries
  print_step "Building mithril-aggregator, mithril-signer, mithril-client, mithril-relay..."
  local cargo_command="cargo build --release"
  cargo_command+=" --bin mithril-aggregator"
  cargo_command+=" --bin mithril-signer"
  cargo_command+=" --bin mithril-client"
  cargo_command+=" --bin mithril-relay"
  if [[ -n "$cargo_features_flag" ]]; then
    cargo_command+=" $cargo_features_flag"
  fi

  echo "  $cargo_command"
  (cd "$PROJECT_ROOT" && eval "$cargo_command") || error_exit "Cargo build failed"

  # Copy binaries to the store
  for node in "${NODES[@]}"; do
    local binary_path="$PROJECT_ROOT/target/release/$node"
    if [[ -f "$binary_path" ]]; then
      cp "$binary_path" "$target_directory/$node"
      chmod +x "$target_directory/$node"
      print_success "$node copied to $target_directory"
    else
      print_warning "$node binary not found at $binary_path"
    fi
  done

  # Optionally build the E2E runner
  if [[ "$build_e2e" == "true" ]]; then
    print_step "Building mithril-end-to-end..."
    local e2e_command="cargo build --release --bin mithril-end-to-end"
    if [[ -n "$cargo_features_flag" ]]; then
      e2e_command+=" $cargo_features_flag"
    fi

    echo "  $e2e_command"
    (cd "$PROJECT_ROOT" && eval "$e2e_command") || error_exit "E2E build failed"

    local e2e_directory="$store_directory/e2e-$label"
    mkdir -p "$e2e_directory"
    cp "$PROJECT_ROOT/target/release/mithril-end-to-end" "$e2e_directory/mithril-end-to-end"
    chmod +x "$e2e_directory/mithril-end-to-end"
    print_success "mithril-end-to-end copied to $e2e_directory"
  fi
}

# ---------------------------------------------------------------------------
# Command: list
# ---------------------------------------------------------------------------
usage_list() {
  cat <<EOF
Usage: $(basename "$0") list [options]

List all available binary sets in the local store.

Options:
  -s, --store-directory <path>  Directory for storing binaries (default: $DEFAULT_STORE_DIR)
  -h, --help                   Show this help message
EOF
  exit 0
}

command_list() {
  local store_directory="$DEFAULT_STORE_DIR"

  while [[ $# -gt 0 ]]; do
    case "$1" in
      -s|--store-directory) store_directory="$2"; shift 2 ;;
      -h|--help) usage_list ;;
      *) error_exit "Unknown option for list: $1" ;;
    esac
  done

  print_header "Available binary sets"

  if [[ ! -d "$store_directory" ]]; then
    echo "Store directory does not exist: $store_directory"
    echo "Run 'install' or 'build' first."
    exit 0
  fi

  local found_any=false
  for entry in "$store_directory"/*/; do
    [[ -d "$entry" ]] || continue
    found_any=true
    local name
    name=$(basename "$entry")

    # Skip e2e directories in the main listing (show them separately)
    if [[ "$name" == e2e-* ]]; then
      continue
    fi

    echo -e "${BOLD}$name${RESET}"
    for node in "${NODES[@]}"; do
      if [[ -f "$entry/$node" ]]; then
        local version_output
        version_output=$("$entry/$node" --version 2>/dev/null || echo "unknown version")
        echo -e "  ${GREEN}+${RESET} $node  ($version_output)"
      else
        echo -e "  ${RED}-${RESET} $node  (not available)"
      fi
    done
    echo ""
  done

  # Show E2E runners
  local found_e2e=false
  for entry in "$store_directory"/e2e-*/; do
    [[ -d "$entry" ]] || continue
    if [[ "$found_e2e" == "false" ]]; then
      echo -e "${BOLD}E2E runners:${RESET}"
      found_e2e=true
    fi
    local name
    name=$(basename "$entry")
    if [[ -f "$entry/mithril-end-to-end" ]]; then
      local version_output
      version_output=$("$entry/mithril-end-to-end" --version 2>/dev/null || echo "unknown version")
      echo -e "  ${GREEN}+${RESET} $name  ($version_output)"
    fi
  done

  if [[ "$found_any" == "false" ]]; then
    echo "No binary sets found in $store_directory"
    echo "Run 'install' or 'build' first."
  fi
}

# ---------------------------------------------------------------------------
# Command: test
# ---------------------------------------------------------------------------
usage_test() {
  cat <<EOF
Usage: $(basename "$0") test [options]

Run backward compatibility E2E tests with mixed binary sets.

This mirrors the CI strategy: for each (release-tag, node-type) combination,
all binaries come from the "release under test" EXCEPT the specified node,
which uses the older release version.

Options:
  -r, --release-to-test <label>       The "new" release label to test (default: 'unstable')
  -t, --tags <tag1,tag2,...>          Comma-separated list of older release tags to test against
                                      (default: auto-detect from tags.json in the store)
  -e, --e2e-label <label>            Label of the E2E runner to use (default: same as release-to-test)
  -n, --nodes <n1,n2,...>             Node types to substitute (default: mithril-aggregator,mithril-signer,mithril-client)
  --cardano-node-version <ver>        Cardano node version (default: $DEFAULT_CARDANO_NODE_VERSION)
  --signed-entity-types <types>       Signed entity types (default: $DEFAULT_SIGNED_ENTITY_TYPES)
  --cardano-slot-length <len>         Cardano slot length in seconds (default: $DEFAULT_CARDANO_SLOT_LENGTH)
  --cardano-epoch-length <len>        Cardano epoch length in seconds (default: $DEFAULT_CARDANO_EPOCH_LENGTH)
  --max-attempts <N>                  Maximum retry attempts per test (default: $DEFAULT_MAX_ATTEMPTS)
  -s, --store-directory <path>        Binary store directory (default: $DEFAULT_STORE_DIR)
  -w, --e2e-work-directory <path>     Working directory for E2E test artifacts (required)
                                      This is where the devnet, cardano nodes and mithril nodes
                                      write their data during each test run.
  -o, --results-directory <path>      Directory for JSON test results (default: <store>/results)
  -h, --help                          Show this help message

Examples:
  $(basename "$0") test --store-directory ./store --e2e-work-directory /tmp/backward-compat-work --release-to-test future-snark --e2e-label future-snark
  $(basename "$0") test --store-directory ./store --e2e-work-directory /tmp/backward-compat-work --release-to-test unstable --tags 2543.0,2524.0,2517.0
  $(basename "$0") test --store-directory ./store --e2e-work-directory /tmp/backward-compat-work --release-to-test future-snark --nodes mithril-aggregator
EOF
  exit 0
}

command_test() {
  local release_to_test="unstable"
  local tags_csv=""
  local e2e_label=""
  local nodes_csv="mithril-aggregator,mithril-signer,mithril-client"
  local cardano_node_version="$DEFAULT_CARDANO_NODE_VERSION"
  local signed_entity_types="$DEFAULT_SIGNED_ENTITY_TYPES"
  local cardano_slot_length="$DEFAULT_CARDANO_SLOT_LENGTH"
  local cardano_epoch_length="$DEFAULT_CARDANO_EPOCH_LENGTH"
  local max_attempts="$DEFAULT_MAX_ATTEMPTS"
  local store_directory="$DEFAULT_STORE_DIR"
  local e2e_work_directory=""
  local results_directory=""

  while [[ $# -gt 0 ]]; do
    case "$1" in
      -r|--release-to-test) release_to_test="$2"; shift 2 ;;
      -t|--tags) tags_csv="$2"; shift 2 ;;
      -e|--e2e-label) e2e_label="$2"; shift 2 ;;
      -n|--nodes) nodes_csv="$2"; shift 2 ;;
      --cardano-node-version) cardano_node_version="$2"; shift 2 ;;
      --signed-entity-types) signed_entity_types="$2"; shift 2 ;;
      --cardano-slot-length) cardano_slot_length="$2"; shift 2 ;;
      --cardano-epoch-length) cardano_epoch_length="$2"; shift 2 ;;
      --max-attempts) max_attempts="$2"; shift 2 ;;
      -s|--store-directory) store_directory="$2"; shift 2 ;;
      -w|--e2e-work-directory) e2e_work_directory="$2"; shift 2 ;;
      -o|--results-directory) results_directory="$2"; shift 2 ;;
      -h|--help) usage_test ;;
      *) error_exit "Unknown option for test: $1" ;;
    esac
  done

  if [[ -z "$e2e_work_directory" ]]; then
    error_exit "The --e2e-work-directory option is required. It specifies where the E2E test runner writes devnet and node artifacts."
  fi

  if [[ -z "$e2e_label" ]]; then
    e2e_label="$release_to_test"
  fi

  if [[ -z "$results_directory" ]]; then
    results_directory="$store_directory/results"
  fi

  # Resolve tags
  local tags=()
  if [[ -n "$tags_csv" ]]; then
    IFS=',' read -ra tags <<< "$tags_csv"
  else
    local tags_file="$store_directory/tags.json"
    if [[ -f "$tags_file" ]]; then
      mapfile -t tags < <(jq -r '.[]' "$tags_file")
      # Exclude the release-to-test from comparison tags
      local filtered_tags=()
      for tag in "${tags[@]}"; do
        if [[ "$tag" != "$release_to_test" ]]; then
          filtered_tags+=("$tag")
        fi
      done
      tags=("${filtered_tags[@]}")
    else
      error_exit "No tags specified and no tags.json found in $store_directory. Use --tags or run 'install' first."
    fi
  fi

  # Resolve node types
  local nodes=()
  IFS=',' read -ra nodes <<< "$nodes_csv"

  # Validate directories
  local release_directory="$store_directory/$release_to_test"
  if [[ ! -d "$release_directory" ]]; then
    error_exit "Release-to-test binaries not found: $release_directory. Run 'install' or 'build' first."
  fi

  local e2e_binary
  local e2e_directory="$store_directory/e2e-$e2e_label"
  if [[ -f "$e2e_directory/mithril-end-to-end" ]]; then
    e2e_binary="$e2e_directory/mithril-end-to-end"
  elif [[ -f "$PROJECT_ROOT/target/release/mithril-end-to-end" ]]; then
    print_warning "No E2E binary found at $e2e_directory, falling back to target/release/mithril-end-to-end"
    e2e_binary="$PROJECT_ROOT/target/release/mithril-end-to-end"
  else
    error_exit "No mithril-end-to-end binary found. Build it with 'build --label $e2e_label --build-e2e'."
  fi

  for tag in "${tags[@]}"; do
    local tag_directory="$store_directory/$tag"
    if [[ ! -d "$tag_directory" ]]; then
      error_exit "Binaries for tag $tag not found: $tag_directory. Run 'install' first."
    fi
  done

  mkdir -p "$results_directory"

  print_header "Running backward compatibility tests"
  echo "Release under test:  $release_to_test"
  echo "E2E binary:          $e2e_binary"
  echo "E2E work directory:  $e2e_work_directory"
  echo "Comparison tags:     ${tags[*]}"
  echo "Node types:          ${nodes[*]}"
  echo "Cardano node:        $cardano_node_version"
  echo "Signed entities:     $signed_entity_types"
  echo "Max attempts:        $max_attempts"
  echo ""

  local total_tests=$(( ${#tags[@]} * ${#nodes[@]} ))
  local current_test=0
  local passed=0
  local incompatible=0
  local failed=0

  for tag in "${tags[@]}"; do
    for node in "${nodes[@]}"; do
      current_test=$((current_test + 1))
      print_step "[$current_test/$total_tests] Testing $node from $tag against $release_to_test"

      # Assemble a mixed binary directory
      local e2e_run_directory="$e2e_work_directory/e2e-run-${tag}-${node}"
      rm -rf "$e2e_run_directory"
      mkdir -p "$e2e_run_directory/binaries"
      mkdir -p "$e2e_run_directory/artifacts"

      # Start with all binaries from the release under test
      cp "$e2e_binary" "$e2e_run_directory/binaries/mithril-end-to-end"
      for n in "${NODES[@]}"; do
        if [[ -f "$release_directory/$n" ]]; then
          cp "$release_directory/$n" "$e2e_run_directory/binaries/$n"
        fi
      done

      # Overwrite the single node with the older version
      local old_binary="$store_directory/$tag/$node"
      if [[ -f "$old_binary" ]]; then
        cp --remove-destination "$old_binary" "$e2e_run_directory/binaries/$node"
      else
        print_warning "Binary $node not found for tag $tag, skipping"
        continue
      fi

      # Ensure all binaries are executable
      chmod +x "$e2e_run_directory/binaries/"*

      # Determine version tags for the result JSON
      local aggregator_tag="$release_to_test"
      local signer_tag="$release_to_test"
      local client_tag="$release_to_test"
      case "$node" in
        mithril-aggregator) aggregator_tag="$tag" ;;
        mithril-signer) signer_tag="$tag" ;;
        mithril-client) client_tag="$tag" ;;
      esac

      # Run the E2E test with retries
      local result="failure"
      local attempt=0
      while [[ $attempt -lt $max_attempts ]]; do
        attempt=$((attempt + 1))
        echo "  Attempt $attempt/$max_attempts..."

        # Clean artifacts between attempts
        rm -rf "$e2e_run_directory/artifacts"
        mkdir -p "$e2e_run_directory/artifacts"

        local e2e_command="$e2e_run_directory/binaries/mithril-end-to-end -vvv"
        e2e_command+=" --bin-directory $e2e_run_directory/binaries"
        e2e_command+=" --work-directory $e2e_run_directory/artifacts"
        e2e_command+=" --devnet-scripts-directory $DEVNET_SCRIPTS_DIR"
        e2e_command+=" --cardano-node-version $cardano_node_version"
        e2e_command+=" --cardano-slot-length $cardano_slot_length"
        e2e_command+=" --cardano-epoch-length $cardano_epoch_length"
        e2e_command+=" --signed-entity-types $signed_entity_types"
        echo "  Command: $e2e_command"

        local exit_code=0
        eval "$e2e_command" > >(sed 's/^/    /') 2>&1 || exit_code=$?
        wait

        if [[ $exit_code -eq 0 ]]; then
          result="success"
          break
        elif [[ $exit_code -eq 3 ]]; then
          result="incompatible"
          break
        elif [[ $exit_code -eq 2 ]]; then
          if [[ $attempt -lt $max_attempts ]]; then
            print_warning "  Retryable error (exit code 2), retrying..."
            continue
          fi
          result="failure"
          break
        else
          result="failure"
          break
        fi
      done

      # Record result
      case "$result" in
        success)
          print_success "$node@$tag: compatible"
          passed=$((passed + 1))
          ;;
        incompatible)
          print_warning "$node@$tag: incompatible"
          incompatible=$((incompatible + 1))
          ;;
        failure)
          print_error "$node@$tag: FAILED"
          failed=$((failed + 1))
          ;;
      esac

      # Write result JSON
      local result_file_name="result-tag_${tag}-node_${node}"
      jq -n \
        --arg TAG "$tag" \
        --arg NODE "$node" \
        --arg CARDANO_NODE "$cardano_node_version" \
        --arg AGGREGATOR "$aggregator_tag" \
        --arg SIGNER "$signer_tag" \
        --arg CLIENT "$client_tag" \
        --arg RESULT "$result" \
        '{tag: $TAG, node: $NODE, mithril_signer: $SIGNER, mithril_aggregator: $AGGREGATOR, mithril_client: $CLIENT, cardano_node_version: $CARDANO_NODE, result: $RESULT}' \
        > "$results_directory/$result_file_name.json"

      echo ""
    done
  done

  # Write summary
  jq -s '.' "$results_directory"/result-*.json > "$results_directory/summary.json" 2>/dev/null || true

  print_header "Test results"
  echo "Total:        $total_tests"
  echo -e "Passed:       ${GREEN}$passed${RESET}"
  echo -e "Incompatible: ${YELLOW}$incompatible${RESET}"
  echo -e "Failed:       ${RED}$failed${RESET}"
  echo ""
  echo "Results stored in: $results_directory"

  if [[ $failed -gt 0 ]]; then
    exit 1
  fi
}

# ---------------------------------------------------------------------------
# Command: report
# ---------------------------------------------------------------------------
usage_report() {
  cat <<EOF
Usage: $(basename "$0") report [options]

Generate a compatibility report from test results.

Options:
  -o, --results-directory <path>  Directory containing result JSON files (default: <store>/results)
  -s, --store-directory <path>    Binary store directory (default: $DEFAULT_STORE_DIR)
  -f, --format <format>           Output format: 'table', 'markdown' or 'json' (default: table)
  -r, --release-to-test <label>   Label of the release under test (for the report header)
  -h, --help                      Show this help message

Examples:
  $(basename "$0") report --store-directory ./store
  $(basename "$0") report --store-directory ./store --format markdown
  $(basename "$0") report --store-directory ./store --format json
  $(basename "$0") report --store-directory ./store --release-to-test future-snark
EOF
  exit 0
}

command_report() {
  local store_directory="$DEFAULT_STORE_DIR"
  local results_directory=""
  local format="table"
  local release_to_test="unstable"

  while [[ $# -gt 0 ]]; do
    case "$1" in
      -o|--results-directory) results_directory="$2"; shift 2 ;;
      -s|--store-directory) store_directory="$2"; shift 2 ;;
      -f|--format) format="$2"; shift 2 ;;
      -r|--release-to-test) release_to_test="$2"; shift 2 ;;
      -h|--help) usage_report ;;
      *) error_exit "Unknown option for report: $1" ;;
    esac
  done

  if [[ -z "$results_directory" ]]; then
    results_directory="$store_directory/results"
  fi

  local summary_file="$results_directory/summary.json"

  if [[ ! -f "$summary_file" ]]; then
    # Try to assemble from individual results
    if ls "$results_directory"/result-*.json &>/dev/null; then
      jq -s '.' "$results_directory"/result-*.json > "$summary_file"
    else
      error_exit "No test results found in $results_directory. Run 'test' first."
    fi
  fi

  if [[ "$format" == "json" ]]; then
    jq '.' "$summary_file"
    return 0
  fi

  # Extract metadata from results
  local cardano_node_version
  cardano_node_version=$(jq -r '.[0].cardano_node_version // "N/A"' "$summary_file" 2>/dev/null || echo "N/A")

  if [[ "$format" == "markdown" ]]; then
    report_markdown "$summary_file" "$release_to_test" "$cardano_node_version"
    return 0
  fi

  report_table "$summary_file" "$release_to_test" "$cardano_node_version" "$results_directory"
}

report_markdown() {
  local summary_file="$1"
  local release_to_test="$2"
  local cardano_node_version="$3"

  local check_mark=":heavy_check_mark:"
  local warn_mark=":warning:"
  local cross_mark=":no_entry:"

  echo "## Distributions backward compatibility"
  echo ""
  echo "This is the compatibility report of the latest distributions against **'${release_to_test}'**."
  echo ""
  echo "**Cardano node version**: ${cardano_node_version}"
  echo ""
  echo "| Compatibility | mithril-signer | mithril-aggregator | mithril-client |"
  echo "| --- | :---: | :---: | :---: |"

  jq -r --arg CHECK_MARK "$check_mark" --arg WARN_MARK "$warn_mark" --arg CROSS_MARK "$cross_mark" \
    'def parseresult(result): if result == "success" then $CHECK_MARK elif result == "incompatible" then $WARN_MARK else $CROSS_MARK end;
    group_by(.tag) | sort_by(.[0].tag | sub("-[a-z]+$"; "") | tonumber) | reverse |
    .[] |
    {
      tag: .[0].tag,
      signer: (map(select(.node == "mithril-signer") | parseresult(.result)) | join("")),
      aggregator: (map(select(.node == "mithril-aggregator") | parseresult(.result)) | join("")),
      client: (map(select(.node == "mithril-client") | parseresult(.result)) | join(""))
    } |
    "| `\(.tag)` | \(.signer) | \(.aggregator) | \(.client) |"' "$summary_file"
}

report_table() {
  local summary_file="$1"
  local release_to_test="$2"
  local cardano_node_version="$3"
  local results_directory="$4"

  print_header "Backward compatibility report"
  echo "Release under test: $release_to_test"
  echo ""
  echo "Cardano node version: $cardano_node_version"
  echo ""

  local check_mark="OK"
  local warn_mark="WARN"
  local cross_mark="FAIL"

  printf "%-20s | %-16s | %-20s | %-16s\n" "Compatibility" "mithril-signer" "mithril-aggregator" "mithril-client"
  printf "%-20s-+-%-16s-+-%-20s-+-%-16s\n" "--------------------" "----------------" "--------------------" "----------------"

  jq -r --arg OK "$check_mark" --arg WARN "$warn_mark" --arg FAIL "$cross_mark" '
    def parseresult(result):
      if result == "success" then $OK
      elif result == "incompatible" then $WARN
      else $FAIL
      end;
    group_by(.tag)
    | sort_by(.[0].tag | sub("-[a-z]+$"; "") | tonumber)
    | reverse
    | .[]
    | {
        tag: .[0].tag,
        signer: (map(select(.node == "mithril-signer") | parseresult(.result)) | join(",")),
        aggregator: (map(select(.node == "mithril-aggregator") | parseresult(.result)) | join(",")),
        client: (map(select(.node == "mithril-client") | parseresult(.result)) | join(","))
      }
    | [.tag, .signer, .aggregator, .client]
    | @tsv
  ' "$summary_file" | while IFS=$'\t' read -r tag signer aggregator client; do
    local signer_colored="$signer"
    local aggregator_colored="$aggregator"
    local client_colored="$client"

    case "$signer" in
      "$check_mark") signer_colored="${GREEN}${signer}${RESET}" ;;
      "$warn_mark") signer_colored="${YELLOW}${signer}${RESET}" ;;
      "$cross_mark") signer_colored="${RED}${signer}${RESET}" ;;
    esac
    case "$aggregator" in
      "$check_mark") aggregator_colored="${GREEN}${aggregator}${RESET}" ;;
      "$warn_mark") aggregator_colored="${YELLOW}${aggregator}${RESET}" ;;
      "$cross_mark") aggregator_colored="${RED}${aggregator}${RESET}" ;;
    esac
    case "$client" in
      "$check_mark") client_colored="${GREEN}${client}${RESET}" ;;
      "$warn_mark") client_colored="${YELLOW}${client}${RESET}" ;;
      "$cross_mark") client_colored="${RED}${client}${RESET}" ;;
    esac

    printf "%-20s | %-25b | %-29b | %-25b\n" "$tag" "$signer_colored" "$aggregator_colored" "$client_colored"
  done

  echo ""
  echo -e "Legend: ${GREEN}OK${RESET} = compatible, ${YELLOW}WARN${RESET} = incompatible (known), ${RED}FAIL${RESET} = test failure"
  echo ""
  echo "Full results: $results_directory/summary.json"
}

# ---------------------------------------------------------------------------
# Main dispatch
# ---------------------------------------------------------------------------
usage_main() {
  cat <<EOF
Mithril Backward Compatibility - Local Testing Tool

Reproduces the CI backward compatibility tests locally, with support for
building custom binaries (e.g. with the future_snark feature).

Usage: $(basename "$0") <command> [options]

Commands:
  install   Download released binaries for given distributions
  build     Build binaries from the local workspace with optional features
  list      List available binary sets in the local store
  test      Run backward compatibility E2E tests
  report    Generate a compatibility report from test results

Run '$(basename "$0") <command> --help' for details on each command.

Quick start:
  # 1. Download the latest stable releases + unstable
  $(basename "$0") install -d unstable --total-releases 3 --store-directory ./store

  # 2. Build local binaries with future_snark
  $(basename "$0") build --label future-snark --features future_snark --build-e2e --store-directory ./store

  # 3. Run backward compatibility tests
  $(basename "$0") test --store-directory ./store --e2e-work-directory /tmp/backward-compat-work --release-to-test future-snark --e2e-label future-snark

  # 4. Generate a report
  $(basename "$0") report --store-directory ./store --release-to-test future-snark
EOF
  exit 0
}

main() {
  if [[ $# -eq 0 ]]; then
    usage_main
  fi

  local command="$1"
  shift

  case "$command" in
    install) command_install "$@" ;;
    build) command_build "$@" ;;
    list) command_list "$@" ;;
    test) command_test "$@" ;;
    report) command_report "$@" ;;
    -h|--help|help) usage_main ;;
    *) error_exit "Unknown command: $command. Run '$(basename "$0") --help' for usage." ;;
  esac
}

main "$@"
