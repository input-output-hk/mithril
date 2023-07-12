---
sidebar_position: 2
---

import NetworksMatrix from '../../networks-matrix.md';
import CompiledBinaries from '../../compiled-binaries.md'

# Bootstrap a Cardano Node

:::info

Thanks to a **Mithril Client** connected to a **Mithril Aggregator**, you will restore a full Cardano node in less than 2 hours!

:::

:::note Mithril Networks

<NetworksMatrix />

:::

# Video demonstration

In this video you will see how fast the bootstrapping of a Cardano node with Mithril is compared to classical bootstrapping (benchmark done on mainnet with Daedalus wallet).

<iframe style={{width: '100%', height: '480px'}} src="https://www.youtube.com/embed/5qjJNRgEzYo" title="Daedalus Bootstrap Benchmark on mainnet with/without Mithril" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen="true"></iframe>


## Pre-requisites

* Install a [correctly configured](https://www.rust-lang.org/learn/get-started) Rust toolchain (latest stable version).

* Install OpenSSL development libraries, for example on Ubuntu/Debian/Mint run `sudo apt install libssl-dev`

* Install other requirements
```bash
sudo apt-get install make build-essential m4 docker jq
```

## Download source

Download from GitHub (HTTPS)

```bash
git clone https://github.com/input-output-hk/mithril.git
```

Or (SSH)

```bash
git clone git@github.com:input-output-hk/mithril.git
```

## Build Mithril Client binary

Switch to build branch / tag

```bash
# **YOUR_BUILD_BRANCH_OR_TAG** depends on the Mithril network you target, 
# please refer to the **Build From** column of the above **Mithril Networks** table
git switch **YOUR_BUILD_BRANCH_OR_TAG**
```

Change directory

```bash
cd mithril/mithril-client
```

Run tests (Optional)

```bash
make test
```

Build executable

```bash
make build
```

## Verify binary

### Verify version

Check that the Mithril Signer binary is running the correct version by running

```bash
./mithril-client -V
```

You should see something like

```bash
mithril-client 0.2.0
```

:warning: Verify that the version displayed is the version described in the content of the Release / Pre-Release note (see the **Build From** column of the above **Mithril Networks** table)


### Verify build

Check that the Mithril Client binary is working fine by running its help

```bash
./mithril-client -h
```

You should see

```bash
This program shows, downloads and verifies certified blockchain artifacts.

Usage: mithril-client [OPTIONS] <COMMAND>

Commands:
  snapshot                    Snapshot commands
  mithril-stake-distribution  Mithril Stake Distribution management (alias: msd)
  help                        Print this message or the help of the given subcommand(s)

Options:
      --run-mode <RUN_MODE>
          Run Mode [env: RUN_MODE=] [default: dev]
  -v, --verbose...
          Verbosity level (-v=warning, -vv=info, -vvv=debug)
      --config-directory <CONFIG_DIRECTORY>
          Directory where configuration file is located [default: ./config]
      --aggregator-endpoint <AGGREGATOR_ENDPOINT>
          Override configuration Aggregator endpoint URL
  -h, --help
          Print help
  -V, --version
          Print version


```

:::tip

You can use the `--json` option in order to display results in `JSON` format for the `list` and `show` commands:

```bash
./mithril-client snapshot list --json
```

:::

:::tip

If you want to dig deeper, you can get access to several level of logs from the Mithril Client:

* Add `-v` for some logs (WARN)
* Add `-vv` for more logs (INFO)
* Add `-vvv` for even more logs (DEBUG)
* Add `-vvvv` for all logs (TRACE)

:::

## Download pre-built binary

<CompiledBinaries />

## Run Docker container

The list of available images on the registry is listed [here](https://github.com/input-output-hk/mithril/pkgs/container/mithril-client)

Prepare an environment variable with the selected Docker image

```bash
export MITHRIL_IMAGE_ID=**YOUR_MITHRIL_IMAGE_ID**
```

Here is an example configuration for the `latest` stable Docker image

```bash
export MITHRIL_IMAGE_ID=latest
```

Then create a shell function for the Mithril Client:
```bash
mithril_client () {
  docker run --rm -e NETWORK=$NETWORK -e GENESIS_VERIFICATION_KEY=$GENESIS_VERIFICATION_KEY -e AGGREGATOR_ENDPOINT=$AGGREGATOR_ENDPOINT --name='mithril-client' -v $(pwd):/app/data -u $(id -u) ghcr.io/input-output-hk/mithril-client:$MITHRIL_IMAGE_ID $@
}
```

Now you can use the `mithril_client` function:
```bash
# 1- Help
mithril_client help

# 2- List snapshots
mithril_client snapshot list
```

:::tip

In the following part of the document, you will need to replace the `./mithril-client` commands with `mithril_client` in order to use the above shell function.

:::

## Bootstrap a Cardano node from a testnet Mithril snapshot

### Step 1: Prepare some useful variables

```bash
# Cardano network
export NETWORK=**YOUR_CARDANO_NETWORK**

# Aggregator API endpoint URL
export AGGREGATOR_ENDPOINT=**YOUR_AGGREGATOR_ENDPOINT**

# Genesis verification key
export GENESIS_VERIFICATION_KEY=$(wget -q -O - **YOUR_GENESIS_VERIFICATION_KEY**)

# Digest of the latest produced snapshot for convenience of the demo
# You can also modify this variable and set it to the value of the digest of a snapshot that you can retrieve at step 2
export SNAPSHOT_DIGEST=$(curl -sL $AGGREGATOR_ENDPOINT/artifact/snapshots | jq -r '.[0].digest')
```

### Step 2: Select A Snapshot

List the available snapshots with which you can bootstrap a Cardano node

```bash
./mithril-client snapshot list
```

You will see a list of snapshots

```bash
+-------+-----------+------------------------------------------------------------------+------------+-----------+--------------------------------+
| Epoch | Immutable | Digest                                                           | Size       | Locations | Created                        |
+-------+-----------+------------------------------------------------------------------+------------+-----------+--------------------------------+
| 72    | 1379      | a305aa11b0e2ccf737d4f5def8b0a9f2245eded2b4ec4be876f7bd64deddcbbf | 1259745182 |         1 | 2023-05-31T14:02:40.150189810Z |
+-------+-----------+------------------------------------------------------------------+------------+-----------+--------------------------------+
| 72    | 1378      | 5c2214b0b3a00cccacc96b65f9741d4e818df0bc092bee30986e4d554396c6fd | 1258142105 |         1 | 2023-05-31T07:48:08.357263836Z |
+-------+-----------+------------------------------------------------------------------+------------+-----------+--------------------------------+
| 72    | 1377      | 7cea14e5742387ca770a74f3e3cfdd93fc38573e2babbf05df292888a528ab15 | 1256695921 |         1 | 2023-05-31T01:56:38.178640636Z |
+-------+-----------+------------------------------------------------------------------+------------+-----------+--------------------------------+
| 72    | 1376      | 4cb153b55f6cadf47bc48cc3112c16a037ee49416820190dc9e121d4aa49369f | 1255258102 |         1 | 2023-05-30T20:11:54.620669432Z |
+-------+-----------+------------------------------------------------------------------+------------+-----------+--------------------------------+
|       |           |                                                                  |            |           |                                |
…
```

### Step 3: Show Snapshot Details

Get some more details from a specific snapshot (Optional)

```bash
./mithril-client snapshot show $SNAPSHOT_DIGEST
```

You will see more information about a snapshot

```bash
+-----------------------+-------------------------------------------------------------------------------------------------------------------------------+
| Info                  | Value                                                                                                                         |
+-----------------------+-------------------------------------------------------------------------------------------------------------------------------+
| Epoch                 | 72                                                                                                                            |
+-----------------------+-------------------------------------------------------------------------------------------------------------------------------+
| Immutable File Number | 1379                                                                                                                          |
+-----------------------+-------------------------------------------------------------------------------------------------------------------------------+
| Digest                | a305aa11b0e2ccf737d4f5def8b0a9f2245eded2b4ec4be876f7bd64deddcbbf                                                              |
+-----------------------+-------------------------------------------------------------------------------------------------------------------------------+
| Size                  | 1259745182                                                                                                                    |
+-----------------------+-------------------------------------------------------------------------------------------------------------------------------+
| Location 1            | https://storage.googleapis.com/mithril-release-preprod-…aa11b0e2ccf737d4f5def8b0a9f2245eded2b4ec4be876f7bd64deddcbbf.tar.gz   |
+-----------------------+-------------------------------------------------------------------------------------------------------------------------------+
| Created               | 2023-05-31T14:02:40.150189810Z                                                                                                |
+-----------------------+-------------------------------------------------------------------------------------------------------------------------------+
```

### Step 4: Download Selected Snapshot

Download the selected snapshot from the remote location to your remote location

```bash
./mithril-client snapshot download $SNAPSHOT_DIGEST
```

You will see that the selected snapshot archive has been downloaded locally, is unpacked and that the associated certificate is valid.

```bash
Unpacking snapshot...
Unpack success cd587611b5ff2445c714bef083d9455ed3e42e9304ae0ad38b02432d03f9b068
to /home/mithril/data/testnet/cd587611b5ff2445c714bef083d9455ed3e42e9304ae0ad38b02432d03f9b068/db

Restore a Cardano Node with:

docker run -v cardano-node-ipc:/ipc -v cardano-node-data:/data --mount type=bind,source="/home/mithril/data/testnet/cd587611b5ff2445c714bef083d9455ed3e42e9304ae0ad38b02432d03f9b068/db",target=/data/db/ -e NETWORK=testnet inputoutput/cardano-node
```

### Step 5: Launch a Cardano Node From Restored Snapshot

Launch an empty Cardano node and make it live in minutes!

```bash
docker run -v cardano-node-ipc:/ipc -v cardano-node-data:/data --mount type=bind,source="$(pwd)/data/testnet/$SNAPSHOT_DIGEST/db",target=/data/db/ -e NETWORK=**YOUR_CARDANO_NETWORK** inputoutput/cardano-node
```

You will see the node start by validating the files injested from the snapshot archive

```bash
Starting: /nix/store/gps7n088g6xv3zqg281sng821471vq78-cardano-node-exe-cardano-node-1.35.1/bin/cardano-node run
--config /nix/store/5b6pry15w93fv0r0x9rc3r1ii5871lvr-config-0-0.json
--database-path /data/db
--topology /nix/store/dpajyi2vaychwps1x7d20c2ddls4kf62-topology.yaml
--host-addr 0.0.0.0
--port 3001
--socket-path /ipc/node.socket





+RTS
-N2
-I0
-A16m
-qg
-qb
--disable-delayed-os-memory-return
-RTS
..or, once again, in a single line:
/nix/store/gps7n088g6xv3zqg281sng821471vq78-cardano-node-exe-cardano-node-1.35.1/bin/cardano-node run --config /nix/store/5b6pry15w93fv0r0x9rc3r1ii5871lvr-config-0-0.json --database-path /data/db --topology /nix/store/dpajyi2vaychwps1x7d20c2ddls4kf62-topology.yaml --host-addr 0.0.0.0 --port 3001 --socket-path /ipc/node.socket      +RTS -N2 -I0 -A16m -qg -qb --disable-delayed-os-memory-return -RTS
Node configuration: NodeConfiguration {ncSocketConfig = SocketConfig {ncNodeIPv4Addr = Last {getLast = Just 0.0.0.0}, ncNodeIPv6Addr = Last {getLast = Nothing}, ncNodePortNumber = Last {getLast = Just 3001}, ncSocketPath = Last {getLast = Just "/ipc/node.socket"}}, ncConfigFile = "/nix/store/5b6pry15w93fv0r0x9rc3r1ii5871lvr-config-0-0.json", ncTopologyFile = "/nix/store/dpajyi2vaychwps1x7d20c2ddls4kf62-topology.yaml", ncDatabaseFile = "/data/db", ncProtocolFiles = ProtocolFilepaths {byronCertFile = Nothing, byronKeyFile = Nothing, shelleyKESFile = Nothing, shelleyVRFFile = Nothing, shelleyCertFile = Nothing, shelleyBulkCredsFile = Nothing}, ncValidateDB = False, ncShutdownConfig = ShutdownConfig {scIPC = Nothing, scOnSyncLimit = Just NoShutdown}, ncProtocolConfig = NodeProtocolConfigurationCardano (NodeByronProtocolConfiguration {npcByronGenesisFile = "/nix/store/kax0css4lx3ywihvsgrqjym0jpi20f99-byron-genesis.json", npcByronGenesisFileHash = Just "96fceff972c2c06bd3bb5243c39215333be6d56aaf4823073dca31afe5038471", npcByronReqNetworkMagic = RequiresMagic, npcByronPbftSignatureThresh = Nothing, npcByronApplicationName = ApplicationName {unApplicationName = "cardano-sl"}, npcByronApplicationVersion = 0, npcByronSupportedProtocolVersionMajor = 3, npcByronSupportedProtocolVersionMinor = 0, npcByronSupportedProtocolVersionAlt = 0}) (NodeShelleyProtocolConfiguration {npcShelleyGenesisFile = "/nix/store/2xhy92909anynqsvx1b1x153cxwnfmzx-shelley-genesis.json", npcShelleyGenesisFileHash = Just "849a1764f152e1b09c89c0dfdbcbdd38d711d1fec2db5dfa0f87cf2737a0eaf4"}) (NodeAlonzoProtocolConfiguration {npcAlonzoGenesisFile = "/nix/store/8qnphq6yvcjspiy3z0aijfd6cv64l3hl-alonzo-genesis.json", npcAlonzoGenesisFileHash = Just "7e94a15f55d1e82d10f09203fa1d40f8eede58fd8066542cf6566008068ed874"}) (NodeHardForkProtocolConfiguration {npcTestEnableDevelopmentHardForkEras = False, npcTestShelleyHardForkAtEpoch = Nothing, npcTestShelleyHardForkAtVersion = Nothing, npcTestAllegraHardForkAtEpoch = Nothing, npcTestAllegraHardForkAtVersion = Nothing, npcTestMaryHardForkAtEpoch = Nothing, npcTestMaryHardForkAtVersion = Nothing, npcTestAlonzoHardForkAtEpoch = Nothing, npcTestAlonzoHardForkAtVersion = Nothing, npcTestBabbageHardForkAtEpoch = Nothing, npcTestBabbageHardForkAtVersion = Nothing}), ncDiffusionMode = InitiatorAndResponderDiffusionMode, ncSnapshotInterval = DefaultSnapshotInterval, ncTestEnableDevelopmentNetworkProtocols = False, ncMaxConcurrencyBulkSync = Nothing, ncMaxConcurrencyDeadline = Nothing, ncLoggingSwitch = True, ncLogMetrics = True, ncTraceConfig = TracingOnLegacy (TraceSelection {traceVerbosity = NormalVerbosity, traceAcceptPolicy = OnOff {isOn = True}, traceBlockFetchClient = OnOff {isOn = False}, traceBlockFetchDecisions = OnOff {isOn = False}, traceBlockFetchProtocol = OnOff {isOn = False}, traceBlockFetchProtocolSerialised = OnOff {isOn = False}, traceBlockFetchServer = OnOff {isOn = False}, traceBlockchainTime = OnOff {isOn = False}, traceChainDB = OnOff {isOn = True}, traceChainSyncBlockServer = OnOff {isOn = False}, traceChainSyncClient = OnOff {isOn = False}, traceChainSyncHeaderServer = OnOff {isOn = False}, traceChainSyncProtocol = OnOff {isOn = False}, traceConnectionManager = OnOff {isOn = True}, traceConnectionManagerCounters = OnOff {isOn = True}, traceConnectionManagerTransitions = OnOff {isOn = False}, traceDebugPeerSelectionInitiatorTracer = OnOff {isOn = False}, traceDebugPeerSelectionInitiatorResponderTracer = OnOff {isOn = False}, traceDiffusionInitialization = OnOff {isOn = True}, traceDnsResolver = OnOff {isOn = False}, traceDnsSubscription = OnOff {isOn = True}, traceErrorPolicy = OnOff {isOn = True}, traceForge = OnOff {isOn = True}, traceForgeStateInfo = OnOff {isOn = True}, traceHandshake = OnOff {isOn = False}, traceInboundGovernor = OnOff {isOn = True}, traceInboundGovernorCounters = OnOff {isOn = True}, traceInboundGovernorTransitions = OnOff {isOn = True}, traceIpSubscription = OnOff {isOn = True}, traceKeepAliveClient = OnOff {isOn = False}, traceLedgerPeers = OnOff {isOn = True}, traceLocalChainSyncProtocol = OnOff {isOn = False}, traceLocalConnectionManager = OnOff {isOn = False}, traceLocalErrorPolicy = OnOff {isOn = True}, traceLocalHandshake = OnOff {isOn = False}, traceLocalInboundGovernor = OnOff {isOn = False}, traceLocalMux = OnOff {isOn = False}, traceLocalRootPeers = OnOff {isOn = True}, traceLocalServer = OnOff {isOn = False}, traceLocalStateQueryProtocol = OnOff {isOn = False}, traceLocalTxMonitorProtocol = OnOff {isOn = False}, traceLocalTxSubmissionProtocol = OnOff {isOn = False}, traceLocalTxSubmissionServer = OnOff {isOn = False}, traceMempool = OnOff {isOn = True}, traceMux = OnOff {isOn = False}, tracePeerSelection = OnOff {isOn = True}, tracePeerSelectionCounters = OnOff {isOn = True}, tracePeerSelectionActions = OnOff {isOn = True}, tracePublicRootPeers = OnOff {isOn = True}, traceServer = OnOff {isOn = True}, traceTxInbound = OnOff {isOn = False}, traceTxOutbound = OnOff {isOn = False}, traceTxSubmissionProtocol = OnOff {isOn = False}, traceTxSubmission2Protocol = OnOff {isOn = False}}), ncTraceForwardSocket = Nothing, ncMaybeMempoolCapacityOverride = Nothing, ncProtocolIdleTimeout = 5s, ncTimeWaitTimeout = 60s, ncAcceptedConnectionsLimit = AcceptedConnectionsLimit {acceptedConnectionsHardLimit = 512, acceptedConnectionsSoftLimit = 384, acceptedConnectionsDelay = 5s}, ncTargetNumberOfRootPeers = 100, ncTargetNumberOfKnownPeers = 100, ncTargetNumberOfEstablishedPeers = 50, ncTargetNumberOfActivePeers = 20, ncEnableP2P = DisabledP2PMode}
Listening on http://127.0.0.1:12798
[c995d1df:cardano.node.basicInfo.protocol:Notice:5] [2022-07-10 13:48:51.68 UTC] Byron; Shelley
[c995d1df:cardano.node.basicInfo.version:Notice:5] [2022-07-10 13:48:51.68 UTC] 1.35.1
[c995d1df:cardano.node.basicInfo.commit:Notice:5] [2022-07-10 13:48:51.68 UTC] 0000000000000000000000000000000000000000
[c995d1df:cardano.node.basicInfo.nodeStartTime:Notice:5] [2022-07-10 13:48:51.68 UTC] 2022-07-10 13:48:51.685957811 UTC
[c995d1df:cardano.node.basicInfo.systemStartTime:Notice:5] [2022-07-10 13:48:51.68 UTC] 2019-07-24 20:20:16 UTC
[c995d1df:cardano.node.basicInfo.slotLengthByron:Notice:5] [2022-07-10 13:48:51.68 UTC] 20s
[c995d1df:cardano.node.basicInfo.epochLengthByron:Notice:5] [2022-07-10 13:48:51.68 UTC] 21600
[c995d1df:cardano.node.basicInfo.slotLengthShelley:Notice:5] [2022-07-10 13:48:51.68 UTC] 1s
[c995d1df:cardano.node.basicInfo.epochLengthShelley:Notice:5] [2022-07-10 13:48:51.68 UTC] 432000
[c995d1df:cardano.node.basicInfo.slotsPerKESPeriodShelley:Notice:5] [2022-07-10 13:48:51.68 UTC] 129600
[c995d1df:cardano.node.basicInfo.slotLengthAllegra:Notice:5] [2022-07-10 13:48:51.68 UTC] 1s
[c995d1df:cardano.node.basicInfo.epochLengthAllegra:Notice:5] [2022-07-10 13:48:51.68 UTC] 432000
[c995d1df:cardano.node.basicInfo.slotsPerKESPeriodAllegra:Notice:5] [2022-07-10 13:48:51.68 UTC] 129600
[c995d1df:cardano.node.basicInfo.slotLengthMary:Notice:5] [2022-07-10 13:48:51.68 UTC] 1s
[c995d1df:cardano.node.basicInfo.epochLengthMary:Notice:5] [2022-07-10 13:48:51.68 UTC] 432000
[c995d1df:cardano.node.basicInfo.slotsPerKESPeriodMary:Notice:5] [2022-07-10 13:48:51.68 UTC] 129600
[c995d1df:cardano.node.basicInfo.slotLengthAlonzo:Notice:5] [2022-07-10 13:48:51.68 UTC] 1s
[c995d1df:cardano.node.basicInfo.epochLengthAlonzo:Notice:5] [2022-07-10 13:48:51.68 UTC] 432000
[c995d1df:cardano.node.basicInfo.slotsPerKESPeriodAlonzo:Notice:5] [2022-07-10 13:48:51.68 UTC] 129600
[c995d1df:cardano.node.basicInfo.slotLengthBabbage:Notice:5] [2022-07-10 13:48:51.68 UTC] 1s
[c995d1df:cardano.node.basicInfo.epochLengthBabbage:Notice:5] [2022-07-10 13:48:51.68 UTC] 432000
[c995d1df:cardano.node.basicInfo.slotsPerKESPeriodBabbage:Notice:5] [2022-07-10 13:48:51.68 UTC] 129600
[c995d1df:cardano.node.startup:Info:5] [2022-07-10 13:48:51.69 UTC] Config path /nix/store/5b6pry15w93fv0r0x9rc3r1ii5871lvr-config-0-0.json, Network magic NetworkMagic {unNetworkMagic = 1097911063}, Protocol "Byron; Shelley", Version "1.35.1", Commit "0000000000000000000000000000000000000000", Node start time 2022-07-10 13:48:51.694962147 UTC
[c995d1df:cardano.node.startup:Info:5] [2022-07-10 13:48:51.69 UTC] Era Byron, Slot length 20s, Epoch length 21600
[c995d1df:cardano.node.startup:Info:5] [2022-07-10 13:48:51.69 UTC] Era Shelley, Slot length 1s, Epoch length 432000, Slots per KESPeriod 129600
[c995d1df:cardano.node.startup:Info:5] [2022-07-10 13:48:51.69 UTC] Era Allegra, Slot length 1s, Epoch length 432000, Slots per KESPeriod 129600
[c995d1df:cardano.node.startup:Info:5] [2022-07-10 13:48:51.69 UTC] Era Mary, Slot length 1s, Epoch length 432000, Slots per KESPeriod 129600
[c995d1df:cardano.node.startup:Info:5] [2022-07-10 13:48:51.69 UTC] Era Alonzo, Slot length 1s, Epoch length 432000, Slots per KESPeriod 129600
[c995d1df:cardano.node.startup:Info:5] [2022-07-10 13:48:51.69 UTC] Era Babbage, Slot length 1s, Epoch length 432000, Slots per KESPeriod 129600
[c995d1df:cardano.node.startup:Info:5] [2022-07-10 13:48:51.69 UTC] startup time: 1657460932
[c995d1df:cardano.node.startup:Info:5] [2022-07-10 13:48:51.70 UTC] 
node addresses:          0.0.0.0:3001
local socket:            /ipc/node.socket
node-to-node versions:
NodeToNodeV_7 HardForkNodeToNodeEnabled HardForkSpecificNodeToNodeVersion1 (EraNodeToNodeEnabled ByronNodeToNodeVersion2 :* EraNodeToNodeEnabled ShelleyNodeToNodeVersion1 :* EraNodeToNodeEnabled ShelleyNodeToNodeVersion1 :* EraNodeToNodeEnabled ShelleyNodeToNodeVersion1 :* EraNodeToNodeEnabled ShelleyNodeToNodeVersion1 :* EraNodeToNodeDisabled :* Nil)
NodeToNodeV_8 HardForkNodeToNodeEnabled HardForkSpecificNodeToNodeVersion1 (EraNodeToNodeEnabled ByronNodeToNodeVersion2 :* EraNodeToNodeEnabled ShelleyNodeToNodeVersion1 :* EraNodeToNodeEnabled ShelleyNodeToNodeVersion1 :* EraNodeToNodeEnabled ShelleyNodeToNodeVersion1 :* EraNodeToNodeEnabled ShelleyNodeToNodeVersion1 :* EraNodeToNodeDisabled :* Nil)
NodeToNodeV_9 HardForkNodeToNodeEnabled HardForkSpecificNodeToNodeVersion1 (EraNodeToNodeEnabled ByronNodeToNodeVersion2 :* EraNodeToNodeEnabled ShelleyNodeToNodeVersion1 :* EraNodeToNodeEnabled ShelleyNodeToNodeVersion1 :* EraNodeToNodeEnabled ShelleyNodeToNodeVersion1 :* EraNodeToNodeEnabled ShelleyNodeToNodeVersion1 :* EraNodeToNodeEnabled ShelleyNodeToNodeVersion1 :* Nil)
node-to-client versions:
NodeToClientV_9 HardForkNodeToClientEnabled HardForkSpecificNodeToClientVersion2 (EraNodeToClientEnabled ByronNodeToClientVersion1 :* EraNodeToClientEnabled ShelleyNodeToClientVersion4 :* EraNodeToClientEnabled ShelleyNodeToClientVersion4 :* EraNodeToClientEnabled ShelleyNodeToClientVersion4 :* EraNodeToClientEnabled ShelleyNodeToClientVersion4 :* EraNodeToClientDisabled :* Nil)
NodeToClientV_10 HardForkNodeToClientEnabled HardForkSpecificNodeToClientVersion2 (EraNodeToClientEnabled ByronNodeToClientVersion1 :* EraNodeToClientEnabled ShelleyNodeToClientVersion4 :* EraNodeToClientEnabled ShelleyNodeToClientVersion4 :* EraNodeToClientEnabled ShelleyNodeToClientVersion4 :* EraNodeToClientEnabled ShelleyNodeToClientVersion4 :* EraNodeToClientDisabled :* Nil)
NodeToClientV_11 HardForkNodeToClientEnabled HardForkSpecificNodeToClientVersion2 (EraNodeToClientEnabled ByronNodeToClientVersion1 :* EraNodeToClientEnabled ShelleyNodeToClientVersion5 :* EraNodeToClientEnabled ShelleyNodeToClientVersion5 :* EraNodeToClientEnabled ShelleyNodeToClientVersion5 :* EraNodeToClientEnabled ShelleyNodeToClientVersion5 :* EraNodeToClientDisabled :* Nil)
NodeToClientV_12 HardForkNodeToClientEnabled HardForkSpecificNodeToClientVersion2 (EraNodeToClientEnabled ByronNodeToClientVersion1 :* EraNodeToClientEnabled ShelleyNodeToClientVersion5 :* EraNodeToClientEnabled ShelleyNodeToClientVersion5 :* EraNodeToClientEnabled ShelleyNodeToClientVersion5 :* EraNodeToClientEnabled ShelleyNodeToClientVersion5 :* EraNodeToClientDisabled :* Nil)
NodeToClientV_13 HardForkNodeToClientEnabled HardForkSpecificNodeToClientVersion2 (EraNodeToClientEnabled ByronNodeToClientVersion1 :* EraNodeToClientEnabled ShelleyNodeToClientVersion5 :* EraNodeToClientEnabled ShelleyNodeToClientVersion5 :* EraNodeToClientEnabled ShelleyNodeToClientVersion5 :* EraNodeToClientEnabled ShelleyNodeToClientVersion5 :* EraNodeToClientEnabled ShelleyNodeToClientVersion5 :* Nil)
[c995d1df:cardano.node.ChainDB:Info:5] [2022-07-10 13:48:51.70 UTC] Started opening Chain DB
[c995d1df:cardano.node.ChainDB:Info:5] [2022-07-10 13:48:51.70 UTC] Started opening Immutable DB
[c995d1df:cardano.node.ChainDB:Info:5] [2022-07-10 13:48:51.79 UTC] Validating chunk no. 0 out of 2917. Progress: 0.00%
[c995d1df:cardano.node.ChainDB:Info:5] [2022-07-10 13:48:52.16 UTC] Validated chunk no. 0 out of 2917. Progress: 0.00%
[c995d1df:cardano.node.ChainDB:Info:5] [2022-07-10 13:48:52.16 UTC] Validating chunk no. 1 out of 2917. Progress: 0.00%
[c995d1df:cardano.node.ChainDB:Info:5] [2022-07-10 13:48:52.56 UTC] Validated chunk no. 1 out of 2917. Progress: 0.03%
[c995d1df:cardano.node.ChainDB:Info:5] [2022-07-10 13:48:52.56 UTC] Validating chunk no. 2 out of 2917. Progress: 0.03%
[c995d1df:cardano.node.ChainDB:Info:5] [2022-07-10 13:48:52.96 UTC] Validated chunk no. 2 out of 2917. Progress: 0.07%
[c995d1df:cardano.node.ChainDB:Info:5] [2022-07-10 13:48:52.96 UTC] Validating chunk no. 3 out of 2917. Progress: 0.07%
[c995d1df:cardano.node.ChainDB:Info:5] [2022-07-10 13:48:53.35 UTC] Validated chunk no. 3 out of 2917. Progress: 0.10%
[c995d1df:cardano.node.ChainDB:Info:5] [2022-07-10 13:48:53.35 UTC] Validating chunk no. 4 out of 2917. Progress: 0.10%
[c995d1df:cardano.node.ChainDB:Info:5] [2022-07-10 13:48:53.75 UTC] Validated chunk no. 4 out of 2917. Progress: 0.14%
[c995d1df:cardano.node.ChainDB:Info:5] [2022-07-10 13:48:53.75 UTC] Validating chunk no. 5 out of 2917. Progress: 0.14%
[c995d1df:cardano.node.ChainDB:Info:5] [2022-07-10 13:48:54.14 UTC] Validated chunk no. 5 out of 2917. 

... (Cut for readability)

[c995d1df:cardano.node.ChainDB:Info:5] [2022-07-10 13:53:08.33 UTC] Validated chunk no. 2911 out of 2917. Progress: 99.79%
[c995d1df:cardano.node.ChainDB:Info:5] [2022-07-10 13:53:08.33 UTC] Validating chunk no. 2912 out of 2917. Progress: 99.79%
[c995d1df:cardano.node.ChainDB:Info:5] [2022-07-10 13:53:08.40 UTC] Validated chunk no. 2912 out of 2917. Progress: 99.83%
[c995d1df:cardano.node.ChainDB:Info:5] [2022-07-10 13:53:08.40 UTC] Validating chunk no. 2913 out of 2917. Progress: 99.83%
[c995d1df:cardano.node.ChainDB:Info:5] [2022-07-10 13:53:08.48 UTC] Validated chunk no. 2913 out of 2917. Progress: 99.86%
[c995d1df:cardano.node.ChainDB:Info:5] [2022-07-10 13:53:08.48 UTC] Validating chunk no. 2914 out of 2917. Progress: 99.86%
[c995d1df:cardano.node.ChainDB:Info:5] [2022-07-10 13:53:08.54 UTC] Validated chunk no. 2914 out of 2917. Progress: 99.90%
[c995d1df:cardano.node.ChainDB:Info:5] [2022-07-10 13:53:08.54 UTC] Validating chunk no. 2915 out of 2917. Progress: 99.90%
[c995d1df:cardano.node.ChainDB:Info:5] [2022-07-10 13:53:08.60 UTC] Validated chunk no. 2915 out of 2917. Progress: 99.93%
[c995d1df:cardano.node.ChainDB:Info:5] [2022-07-10 13:53:08.60 UTC] Validating chunk no. 2916 out of 2917. Progress: 99.93%
[c995d1df:cardano.node.ChainDB:Info:5] [2022-07-10 13:53:08.66 UTC] Validated chunk no. 2916 out of 2917. Progress: 99.97%
[c995d1df:cardano.node.ChainDB:Info:5] [2022-07-10 13:53:08.66 UTC] Validating chunk no. 2917 out of 2917. Progress: 99.97%
[c995d1df:cardano.node.ChainDB:Warning:5] [2022-07-10 13:53:08.67 UTC] Rewriting the secondary index for the chunk file with number 2917.
[c995d1df:cardano.node.ChainDB:Warning:5] [2022-07-10 13:53:08.67 UTC] Rewriting the primary index for the chunk file with number 2917.
[c995d1df:cardano.node.ChainDB:Info:5] [2022-07-10 13:53:08.72 UTC] Found a valid last location at chunk 2917 with tip 07c1891b7394c92f50ddfabb84b9bd6be5944c5a201796190b5f69a69dcbc432@63009237.
[c995d1df:cardano.node.ChainDB:Info:5] [2022-07-10 13:53:08.72 UTC] Opened imm db with immutable tip at 07c1891b7394c92f50ddfabb84b9bd6be5944c5a201796190b5f69a69dcbc432 at slot 63009237 and chunk 2917
[c995d1df:cardano.node.ChainDB:Info:5] [2022-07-10 13:53:08.72 UTC] Started opening Volatile DB
[c995d1df:cardano.node.ChainDB:Info:5] [2022-07-10 13:53:09.28 UTC] Opened vol db
[c995d1df:cardano.node.ChainDB:Info:5] [2022-07-10 13:53:09.28 UTC] Started opening Ledger DB
[c995d1df:cardano.node.ChainDB:Info:5] [2022-07-10 13:53:37.19 UTC] Replaying ledger from snapshot at 16ce3e707388b8c033f87b440ac89095b2d2fc5fa2b42f768c38da286312a31a at slot 63005524
[c995d1df:cardano.node.ChainDB:Info:5] [2022-07-10 13:53:37.19 UTC] Replayed block: slot 63005533 out of 63009237. Progress: 0.24%
[c995d1df:cardano.node.ChainDB:Info:5] [2022-07-10 13:53:37.20 UTC] Replayed block: slot 63007165 out of 63009237. Progress: 44.20%
[c995d1df:cardano.node.ChainDB:Info:5] [2022-07-10 13:53:37.24 UTC] Opened lgr db
[c995d1df:cardano.node.ChainDB:Info:5] [2022-07-10 13:53:37.24 UTC] Started initial chain selection
[c995d1df:cardano.node.ChainDB:Info:5] [2022-07-10 13:53:37.45 UTC] Pushing ledger state for block 110b618d3359fb6d8bddf8cb1e98823dc995083a2af2607f1c179928a26d03c3 at slot 63009421. Progress: 0.00%
[c995d1df:cardano.node.ChainDB:Info:5] [2022-07-10 13:53:37.82 UTC] before next, messages elided = 63009476
[c995d1df:cardano.node.ChainDB:Info:5] [2022-07-10 13:53:37.82 UTC] Pushing ledger state for block 5e59df354b64634bd27fc1b3ed08991c6de909ea085625da2ca78ec5a6a3e37c at slot 63019450. Progress: 13.85%
[c995d1df:cardano.node.ChainDB:Info:5] [2022-07-10 13:53:37.82 UTC] Pushing ledger state for block e0610e4f03ef640ead31a729f6f18aa820f3906f6414b179d3c553f0011162ef at slot 63019479. Progress: 13.89%
[c995d1df:cardano.node.ChainDB:Info:5] [2022-07-10 13:53:37.82 UTC] Pushing ledger state for block 8edc84f1f289475ca5c2afd9cae3c704ed7bcdf3e489fd1842e3904bd2d6d8c5 at slot 63019510. Progress: 13.93%
[c995d1df:cardano.node.ChainDB:Info:5] [2022-07-10 13:53:38.21 UTC] before next, messages elided = 63019575
[c995d1df:cardano.node.ChainDB:Info:5] [2022-07-10 13:53:38.21 UTC] Pushing ledger state for block fc426a1c3a8c5ae61100e5ccfcaa47ccfb6cc144ed17fcd47a2b55f48890790a at slot 63029515. Progress: 27.75%
[c995d1df:cardano.node.ChainDB:Info:5] [2022-07-10 13:53:38.21 UTC] Pushing ledger state for block 68059200505d3ec65d3ddce9441efb0607adab002066c00acc050f5bb0c53deb at slot 63029578. Progress: 27.84%
[c995d1df:cardano.node.ChainDB:Info:5] [2022-07-10 13:53:38.21 UTC] Pushing ledger state for block 25f5cc004ad90dc554e085699f1386dc8f69cec4012a7b35735b83da6fa68a0b at slot 63029602. Progress: 27.87%
[c995d1df:cardano.node.ChainDB:Info:5] [2022-07-10 13:53:38.61 UTC] before next, messages elided = 63029636
[c995d1df:cardano.node.ChainDB:Info:5] [2022-07-10 13:53:38.61 UTC] Pushing ledger state for block cfb3dd669fd1a85aaf005ddd0e4a8b90cbb7b2c66dc8ff6f41e2e7aa2a0f0c57 at slot 63039634. Progress: 41.72%
[c995d1df:cardano.node.ChainDB:Info:5] [2022-07-10 13:53:38.61 UTC] Pushing ledger state for block d7f057436e749e4468cfd8b17a04d447c4ce628b8dc33ea7ebdc4d3ad4659dec at slot 63039638. Progress: 41.73%
[c995d1df:cardano.node.ChainDB:Info:5] [2022-07-10 13:53:38.61 UTC] Pushing ledger state for block 73fcb92346714585e1dbb484b1678bb15dab7304a8071cd95ec85195ca7a46be at slot 63039655. Progress: 41.75%
[c995d1df:cardano.node.ChainDB:Info:5] [2022-07-10 13:53:38.93 UTC] before next, messages elided = 63039729
[c995d1df:cardano.node.ChainDB:Info:5] [2022-07-10 13:53:38.93 UTC] Pushing ledger state for block a7256fa99f7bf50d0e764cb80122222b1d2b80ffef4bee30e42c0bb80c5f1168 at slot 63049716. Progress: 55.64%
[c995d1df:cardano.node.ChainDB:Info:5] [2022-07-10 13:53:38.93 UTC] Pushing ledger state for block 75e2064629d6a017bfe324a3d31723ed7bfa017eceda1549bd6c9f4d63653394 at slot 63049861. Progress: 55.84%
[c995d1df:cardano.node.ChainDB:Info:5] [2022-07-10 13:53:38.94 UTC] Pushing ledger state for block 71aeaae8f83ee6b94a7feb521c16f0d889a9b95be6da85d540ac0435b412fb2c at slot 63049904. Progress: 55.90%
[c995d1df:cardano.node.ChainDB:Info:5] [2022-07-10 13:53:39.19 UTC] before next, messages elided = 63049909
[c995d1df:cardano.node.ChainDB:Info:5] [2022-07-10 13:53:39.19 UTC] Pushing ledger state for block 15ad241c6908d0748cb80d17035d4001f7f86ee22fe71197cbe9bc7489d8e71b at slot 63059862. Progress: 69.65%
[c995d1df:cardano.node.ChainDB:Info:5] [2022-07-10 13:53:39.19 UTC] Pushing ledger state for block a5e38b0875d44e10a3cba9c420ad4c9708e913e3458f1003a8236a831762069a at slot 63059915. Progress: 69.73%
[c995d1df:cardano.node.ChainDB:Info:5] [2022-07-10 13:53:39.19 UTC] Pushing ledger state for block ca6dc89bd705476bcd056d6ffc3dd43499e51d0e5ecfe56a439fadacac50fc1b at slot 63059939. Progress: 69.76%
[c995d1df:cardano.node.ChainDB:Info:5] [2022-07-10 13:53:39.48 UTC] before next, messages elided = 63059957
[c995d1df:cardano.node.ChainDB:Info:5] [2022-07-10 13:53:39.48 UTC] Pushing ledger state for block 3c8593b6859b02abaa7f3b69d2114b1f402c65eef6904020e5ed37356aea4461 at slot 63069941. Progress: 83.57%
[c995d1df:cardano.node.ChainDB:Info:5] [2022-07-10 13:53:39.48 UTC] Pushing ledger state for block 50a06570febcf89ee00c386979d3a616561996b6c5cf6cbff44c20aa8454c375 at slot 63069971. Progress: 83.61%
[c995d1df:cardano.node.ChainDB:Info:5] [2022-07-10 13:53:39.48 UTC] Pushing ledger state for block 6265f3c30f149ba0a797a1d2c6998b918f089e7455ab665e3369456105901a5a at slot 63069972. Progress: 83.62%
[c995d1df:cardano.node.ChainDB:Info:5] [2022-07-10 13:53:39.79 UTC] before next, messages elided = 63069985
[c995d1df:cardano.node.ChainDB:Info:5] [2022-07-10 13:53:39.79 UTC] Pushing ledger state for block 0be786a1d9272083e9e184da3bc3bfcb71b82ad10fbf52f4959dc5dc6d1b7b3c at slot 63079898. Progress: 97.32%
[c995d1df:cardano.node.ChainDB:Info:5] [2022-07-10 13:53:39.79 UTC] Pushing ledger state for block 2862bd67a50bf9f6f31ab11d9153663d3e657f88c24ba8e71174f8f5216c2a99 at slot 63079992. Progress: 97.45%
[c995d1df:cardano.node.ChainDB:Info:5] [2022-07-10 13:53:39.79 UTC] Pushing ledger state for block cc9036c6c4076ca28ac5d03522001c233a9202cb5b1d9484c3e406fbb3406fd5 at slot 63079994. Progress: 97.45%
[c995d1df:cardano.node.ChainDB:Info:5] [2022-07-10 13:53:39.86 UTC] before next, messages elided = 63080024
[c995d1df:cardano.node.ChainDB:Info:5] [2022-07-10 13:53:39.86 UTC] Pushing ledger state for block c9bb7635b3e55175af30d1c9ca943d5df438ed838e814956d304cbb638c664aa at slot 63081837. Progress: 100.00%
[c995d1df:cardano.node.ChainDB:Info:5] [2022-07-10 13:53:39.86 UTC] Valid candidate at tip 110b618d3359fb6d8bddf8cb1e98823dc995083a2af2607f1c179928a26d03c3 at slot 63009421
[c995d1df:cardano.node.ChainDB:Info:5] [2022-07-10 13:53:39.86 UTC] Initial chain selected
[c995d1df:cardano.node.ChainDB:Info:5] [2022-07-10 13:53:39.86 UTC] Opened db with immutable tip at 07c1891b7394c92f50ddfabb84b9bd6be5944c5a201796190b5f69a69dcbc432 at slot 63009237 and tip c9bb7635b3e55175af30d1c9ca943d5df438ed838e814956d304cbb638c664aa at slot 63081837
[c995d1df:cardano.node.shutdown:Warning:5] [2022-07-10 13:53:39.87 UTC] Will terminate upon reaching NoShutdown
[c995d1df:cardano.node.DiffusionInitializationTracer:Info:346] [2022-07-10 13:53:39.87 UTC] CreatingServerSocket 0.0.0.0:3001
[c995d1df:cardano.node.DiffusionInitializationTracer:Info:346] [2022-07-10 13:53:39.87 UTC] ConfiguringServerSocket 0.0.0.0:3001
[c995d1df:cardano.node.DiffusionInitializationTracer:Info:346] [2022-07-10 13:53:39.87 UTC] ListeningServerSocket 0.0.0.0:3001
[c995d1df:cardano.node.DiffusionInitializationTracer:Info:346] [2022-07-10 13:53:39.87 UTC] ServerSocketUp 0.0.0.0:3001
[c995d1df:cardano.node.DiffusionInitializationTracer:Info:346] [2022-07-10 13:53:39.87 UTC] RunServer (0.0.0.0:3001 :| [])
[c995d1df:cardano.node.DiffusionInitializationTracer:Info:349] [2022-07-10 13:53:39.87 UTC] CreateSystemdSocketForSnocketPath (LocalAddress "/ipc/node.socket")
[c995d1df:cardano.node.DiffusionInitializationTracer:Info:349] [2022-07-10 13:53:39.87 UTC] CreatedLocalSocket (LocalAddress "/ipc/node.socket")
[c995d1df:cardano.node.DiffusionInitializationTracer:Info:349] [2022-07-10 13:53:39.87 UTC] ConfiguringLocalSocket (LocalAddress "/ipc/node.socket") (FileDescriptor 26)
[c995d1df:cardano.node.DiffusionInitializationTracer:Info:349] [2022-07-10 13:53:39.87 UTC] ListeningLocalSocket (LocalAddress "/ipc/node.socket") (FileDescriptor 26)
[c995d1df:cardano.node.DiffusionInitializationTracer:Info:349] [2022-07-10 13:53:39.87 UTC] LocalSocketUp (LocalAddress "/ipc/node.socket") (FileDescriptor 26)
[c995d1df:cardano.node.DiffusionInitializationTracer:Info:349] [2022-07-10 13:53:39.87 UTC] RunLocalServer (LocalAddress "/ipc/node.socket")
[c995d1df:cardano.node.DnsSubscription:Warning:345] [2022-07-10 13:53:39.91 UTC] Domain: "relays-new.cardano-testnet.iohkdev.io" Unsupported remote target address [2a05:d014:e00:a200:0:1:0:1]:3001
[c995d1df:cardano.node.DnsSubscription:Notice:358] [2022-07-10 13:53:39.91 UTC] Domain: "relays-new.cardano-testnet.iohkdev.io" Connection Attempt Start, destination 3.131.32.242:3001
[c995d1df:cardano.node.DnsSubscription:Warning:345] [2022-07-10 13:53:39.94 UTC] Domain: "relays-new.cardano-testnet.iohkdev.io" Unsupported remote target address [2a05:d01c:321:2101:0:1:0:2]:3001
[c995d1df:cardano.node.DnsSubscription:Notice:359] [2022-07-10 13:53:39.94 UTC] Domain: "relays-new.cardano-testnet.iohkdev.io" Connection Attempt Start, destination 13.41.9.54:3001
[c995d1df:cardano.node.DnsSubscription:Notice:359] [2022-07-10 13:53:39.95 UTC] Domain: "relays-new.cardano-testnet.iohkdev.io" Connection Attempt End, destination 13.41.9.54:3001 outcome: ConnectSuccessLast
[c995d1df:cardano.node.ErrorPolicy:Notice:343] [2022-07-10 13:53:39.95 UTC] IP 3.131.32.242:3001 ErrorPolicySuspendConsumer (Just (ConnectionExceptionTrace (SubscriberError {seType = SubscriberParallelConnectionCancelled, seMessage = "Parallel connection cancelled", seStack = []}))) 1s
```

Then the Cardano node will synchronize with the other nodes of the network and start adding blockss

```bash
[c995d1df:cardano.node.ChainDB:Notice:322] [2022-07-10 13:53:40.06 UTC] Chain extended, new tip: 7ae33b2f4bc8b84e77dfd539f0f6e7f59b293e96f62fdcfdb17cbd7a006fe5c0 at slot 63081906
[c995d1df:cardano.node.ChainDB:Notice:322] [2022-07-10 13:55:08.30 UTC] Chain extended, new tip: 6b4ccd2bec5e3862b23ea0f7c2f342a3659cecdcfdaf04551179df3839be6213 at slot 63092090
[c995d1df:cardano.node.ChainDB:Notice:322] [2022-07-10 13:55:21.36 UTC] Chain extended, new tip: 6e95eb82da5a38544e6ef430a2733f6014c3c10527003b9d3bdc534f6a2ce81f at slot 63092103
[c995d1df:cardano.node.ChainDB:Notice:322] [2022-07-10 13:55:39.04 UTC] Chain extended, new tip: a662672ec4b988022e135cb0b7e440f5fbffe8e205771d13a566a418f7021ba7 at slot 63092121
[c995d1df:cardano.node.ChainDB:Notice:322] [2022-07-10 13:55:45.18 UTC] Chain extended, new tip: 2a0f2e6f218a08f4e0bc4668285d8e792fd7ec62f05880bd5b2d23d6bce20dfb at slot 63092127
[c995d1df:cardano.node.ChainDB:Notice:322] [2022-07-10 13:56:18.05 UTC] Chain extended, new tip: ab9ef8af92ec062ec59a10da588e238ba8840705c095ebd5cd5da7ab9ea9c8e1 at slot 63092160
[c995d1df:cardano.node.ChainDB:Notice:322] [2022-07-10 13:56:45.15 UTC] Chain extended, new tip: e59bcbf34172eb6a934e2580f6b20ebe1ea32fba7420feec096744fab8ffce76 at slot 63092189
[c995d1df:cardano.node.ChainDB:Notice:322] [2022-07-10 13:57:59.06 UTC] Chain extended, new tip: ea0e76fe069a0bcb831cf6b342530adf2a28de4922a752cff734fe81a1842dff at slot 63092263
[c995d1df:cardano.node.ChainDB:Notice:322] [2022-07-10 13:58:02.27 UTC] Chain extended, new tip: 1b68a85a10dd7bd840a69ece2807fbca61a1de07c30080bf94fbbfd1de8d1446 at slot 63092266
[c995d1df:cardano.node.ChainDB:Notice:322] [2022-07-10 13:58:28.21 UTC] Chain extended, new tip: 07e1252c5614b9bb6e6cff95066c134eb251aea6bef33dc34c0dd064934f2cea at slot 63092292
[c995d1df:cardano.node.ChainDB:Notice:322] [2022-07-10 13:58:33.17 UTC] Chain extended, new tip: 0d3712fd7852b0aff00d660f4e7dc609808018dc271fe460bb9ff8270dd50d8f at slot 63092297
[c995d1df:cardano.node.ChainDB:Notice:322] [2022-07-10 13:58:48.53 UTC] Chain extended, new tip: c8bb96ea392e41dce6fc157e164dbe19ba894d268f69985e20a2d65daeb4488c at slot 63092312
[c995d1df:cardano.node.ChainDB:Notice:322] [2022-07-10 13:58:58.08 UTC] Chain extended, new tip: 35ea30e175abaae7983db9f65e7a00ba45312ecb151e53a651b3a6058add4c4f at slot 63092322
[c995d1df:cardano.node.ChainDB:Notice:322] [2022-07-10 13:59:02.13 UTC] Chain extended, new tip: 5320de43ea30e46515f48263211212833474ea87ff046f36f0d83059fad6e9ff at slot 63092326
[c995d1df:cardano.node.ChainDB:Notice:322] [2022-07-10 13:59:06.29 UTC] Chain extended, new tip: 58e9cb29bd771e0625689a0d591a7a8309f9e859ce4a3b70a9ab2e28173ce78b at slot 63092330
[c995d1df:cardano.node.ChainDB:Notice:322] [2022-07-10 13:59:36.31 UTC] Chain extended, new tip: 1af2411f492e1895231cc910bdb1a68a45596157bcbfae5bdfd5c5ed81b03054 at slot 63092360
[c995d1df:cardano.node.ChainDB:Notice:322] [2022-07-10 14:00:04.51 UTC] Chain extended, new tip: 270d47c5a277ef7efe028b6eb4764ab328db8e509ad962370bb383b1832bb260 at slot 63092388
[c995d1df:cardano.node.ChainDB:Notice:322] [2022-07-10 14:00:29.52 UTC] Chain extended, new tip: 6fbe009ac36363cbebc607c733964878dfce6054c8785d1294f202acb475c861 at slot 63092413
[c995d1df:cardano.node.ChainDB:Notice:322] [2022-07-10 14:01:12.63 UTC] Chain extended, new tip: f764596ece0b75d5d0054f22f3f0a0846f1647ff980d053015065099ec279839 at slot 63092456
```
