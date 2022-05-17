module CardanoCluster where

import qualified Cardano.Crypto.DSIGN as Crypto
import Cardano.Ledger.Crypto (DSIGN)
import Cardano.Ledger.Keys (VKey (VKey))
import CardanoNode
  ( CardanoNodeArgs (..),
    CardanoNodeConfig (..),
    Port,
    PortsConfig (..),
    RunningNode (..),
    defaultCardanoNodeArgs,
    withCardanoNode,
  )
import qualified Codec.Archive.Tar as Tar
import qualified Codec.Compression.GZip as GZip
import Control.Tracer (Tracer, traceWith)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Hydra.Cardano.Api
import Hydra.Prelude
import Logging (ClusterLog (MsgFromNode, MsgNodeStarting))
import qualified Paths_mithril_end_to_end as Pkg
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath ((<.>), (</>))
import System.Posix.Files
  ( ownerReadMode,
    setFileMode,
  )
import Test.Network.Ports (randomUnusedTCPPort, randomUnusedTCPPorts)

-- | TODO: This is hard-coded and must match what's in the genesis file, so
-- ideally, we want to either:
--
-- - overwrite the genesis configuration with the `ClusterConfig`
-- - pull the network id from the genesis configuration
defaultNetworkId :: NetworkId
defaultNetworkId = Testnet (NetworkMagic 42)

-- NOTE: This is hard-coded and needs to correspond to the initial funds set in
-- the genesis-shelley.json file.
availableInitialFunds :: Num a => a
availableInitialFunds = 900_000_000_000

-- | Enumeration of known actors for which we can get the 'keysFor' and 'writeKeysFor'.
data Actor
  = Alice
  | Bob
  | Carol
  | Faucet

actorName :: Actor -> String
actorName = \case
  Alice -> "alice"
  Bob -> "bob"
  Carol -> "carol"
  Faucet -> "faucet"

-- | Get the "well-known" keys for given actor.
keysFor :: Actor -> IO (VerificationKey PaymentKey, SigningKey PaymentKey)
keysFor actor = do
  bs <- readConfigFile ("credentials" </> actorName actor <.> "sk")
  let res =
        first TextEnvelopeAesonDecodeError (Aeson.eitherDecodeStrict bs)
          >>= deserialiseFromTextEnvelope asSigningKey
  case res of
    Left err ->
      fail $ "cannot decode text envelope from '" <> show bs <> "', error: " <> show err
    Right sk -> pure (getVerificationKey sk, sk)

type RawVerificationKey = Crypto.VerKeyDSIGN (DSIGN StandardCrypto)

fromRawVKey :: RawVerificationKey -> VerificationKey PaymentKey
fromRawVKey = PaymentVerificationKey . VKey

-- | Write the "well-known" keys for given actor into a target directory.
writeKeysFor ::
  -- | Target directory
  FilePath ->
  Actor ->
  -- | Paths of written keys in the form of (verification key, signing key)
  IO (FilePath, FilePath)
writeKeysFor targetDir actor = do
  readConfigFile ("credentials" </> skName) >>= writeFileBS skTarget
  readConfigFile ("credentials" </> vkName) >>= writeFileBS vkTarget
  pure (vkTarget, skTarget)
  where
    skTarget = targetDir </> skName
    vkTarget = targetDir </> vkName

    skName = actorName actor <.> ".sk"
    vkName = actorName actor <.> ".vk"

-- * Starting a cluster or single nodes

data RunningCluster = RunningCluster
  { clusterConfig :: ClusterConfig,
    clusterNodes :: [RunningNode]
  }

-- | Configuration parameters for the cluster.
data ClusterConfig = ClusterConfig
  { parentStateDirectory :: FilePath,
    networkId :: NetworkId,
    primedDB :: Maybe FilePath
  }

asSigningKey :: AsType (SigningKey PaymentKey)
asSigningKey = AsSigningKey AsPaymentKey

withCluster ::
  Tracer IO ClusterLog -> ClusterConfig -> (RunningCluster -> IO ()) -> IO ()
withCluster tr cfg@ClusterConfig {parentStateDirectory, primedDB} action = do
  systemStart <- initSystemStart
  (cfgA, cfgB, cfgC) <-
    makeNodesConfig parentStateDirectory systemStart
      <$> randomUnusedTCPPorts 3

  primeDB [cfgA, cfgB, cfgC] primedDB

  withBFTNode tr cfgA $ \nodeA -> do
    withBFTNode tr cfgB $ \nodeB -> do
      withBFTNode tr cfgC $ \nodeC -> do
        let nodes = [nodeA, nodeB, nodeC]
        action (RunningCluster cfg nodes)
  where
    primeDB nodesConfig = \case
      Nothing -> pure ()
      Just file -> forM_ nodesConfig $ \(CardanoNodeConfig _ dir _ _) -> do
        dump <- readConfigFile file
        Tar.unpack (dir </> "db") . Tar.read . GZip.decompress . LBS.fromStrict $ dump

-- | Start a cardano-node in BFT mode using the config from config/ and
-- credentials from config/credentials/ using given 'nodeId'. NOTE: This means
-- that nodeId should only be 1,2 or 3 and that only the faucet receives
-- 'initialFunds'. Use 'seedFromFaucet' to distribute funds other wallets.
withBFTNode ::
  Tracer IO ClusterLog ->
  CardanoNodeConfig ->
  (RunningNode -> IO ()) ->
  IO ()
withBFTNode clusterTracer cfg action = do
  createDirectoryIfMissing False (stateDirectory cfg)

  [dlgCert, signKey, vrfKey, kesKey, opCert] <-
    forM
      [ dlgCertFilename nid,
        signKeyFilename nid,
        vrfKeyFilename nid,
        kesKeyFilename nid,
        opCertFilename nid
      ]
      (copyCredential (stateDirectory cfg))

  let args =
        defaultCardanoNodeArgs
          { nodeDlgCertFile = Just dlgCert,
            nodeSignKeyFile = Just signKey,
            nodeVrfKeyFile = Just vrfKey,
            nodeKesKeyFile = Just kesKey,
            nodeOpCertFile = Just opCert,
            nodePort = Just (ours (ports cfg))
          }

  readConfigFile "cardano-node.json"
    >>= writeFileBS
      (stateDirectory cfg </> nodeConfigFile args)

  readConfigFile "genesis-byron.json"
    >>= writeFileBS
      (stateDirectory cfg </> nodeByronGenesisFile args)

  readConfigFile "genesis-shelley.json"
    >>= writeFileBS
      (stateDirectory cfg </> nodeShelleyGenesisFile args)

  readConfigFile "genesis-alonzo.json"
    >>= writeFileBS
      (stateDirectory cfg </> nodeAlonzoGenesisFile args)

  withCardanoNode nodeTracer cfg args $ \rn -> do
    traceWith clusterTracer $ MsgNodeStarting cfg
    waitForSocket rn
    action rn
  where
    dlgCertFilename i = "delegation-cert.00" <> show (i - 1) <> ".json"
    signKeyFilename i = "delegate-keys.00" <> show (i - 1) <> ".key"
    vrfKeyFilename i = "delegate" <> show i <> ".vrf.skey"
    kesKeyFilename i = "delegate" <> show i <> ".kes.skey"
    opCertFilename i = "opcert" <> show i <> ".cert"

    copyCredential parentDir file = do
      bs <- readConfigFile ("credentials" </> file)
      let destination = parentDir </> file
      unlessM (doesFileExist destination) $
        writeFileBS destination bs
      setFileMode destination ownerReadMode
      pure destination

    nid = nodeId cfg

    nodeTracer = contramap (MsgFromNode nid) clusterTracer

    waitForSocket :: RunningNode -> IO ()
    waitForSocket node@(RunningNode _ socket) = do
      unlessM (doesFileExist socket) $ do
        threadDelay 0.1
        waitForSocket node

-- | Initialize the system start time to now (modulo a small offset needed to
-- give time to the system to bootstrap correctly).
initSystemStart :: IO UTCTime
initSystemStart = do
  addUTCTime 1 <$> getCurrentTime

makeNodesConfig ::
  FilePath ->
  UTCTime ->
  [Port] ->
  (CardanoNodeConfig, CardanoNodeConfig, CardanoNodeConfig)
makeNodesConfig stateDirectory systemStart [a, b, c] =
  ( CardanoNodeConfig 1 (stateDirectory </> "node-1") systemStart (PortsConfig a [b, c]),
    CardanoNodeConfig 2 (stateDirectory </> "node-2") systemStart (PortsConfig b [a, c]),
    CardanoNodeConfig 3 (stateDirectory </> "node-3") systemStart (PortsConfig c [a, b])
  )
makeNodesConfig _ _ _ = error "we only support topology for 3 nodes"

newNodeConfig ::
  FilePath ->
  IO CardanoNodeConfig
newNodeConfig stateDirectory = do
  nodePort <- randomUnusedTCPPort
  systemStart <- initSystemStart
  pure $
    CardanoNodeConfig
      { nodeId = 1,
        stateDirectory,
        systemStart,
        ports = PortsConfig nodePort []
      }

-- | Lookup a config file similar reading a file from disk.
readConfigFile :: FilePath -> IO ByteString
readConfigFile source = do
  filename <- Pkg.getDataFileName ("config" </> source)
  BS.readFile filename
