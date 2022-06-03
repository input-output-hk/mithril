{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}

-- | Controls Mithril aggregator service.
module Mithril.Aggregator where

import Control.Tracer (Tracer, traceWith)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as Hex
import Hydra.Prelude
import qualified Paths_mithril_end_to_end as Pkg
import System.Directory (doesFileExist)
import System.Environment (getEnvironment)
import System.FilePath ((</>))
import System.Process (CreateProcess (..), StdStream (UseHandle), proc, withCreateProcess)
import Test.Hydra.Prelude (checkProcessHasNotDied, failure)
import Test.Network.Ports (randomUnusedTCPPort)

data ProtocolParameters = ProtocolParameters
  { k :: Word64,
    m :: Word64,
    phi_f :: Float
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

data SignerWithStake = SignerWithStake
  { party_id :: Word64,
    verification_key :: Text,
    stake :: Word64
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

data Certificate = Certificate
  { hash :: Text,
    previous_hash :: Text,
    beacon :: Beacon,
    protocol :: ProtocolParameters,
    digest :: Text,
    started_at :: UTCTime,
    completed_at :: UTCTime,
    signers :: [SignerWithStake],
    aggregate_verification_key :: Text,
    multisignature :: Text
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

data CertificatePending = CertificatePending
  { beacon :: Beacon,
    protocol :: ProtocolParameters,
    previous_hash :: Text,
    signers :: [SignerWithStake]
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

data Snapshot = Snapshot
  { digest :: Text,
    certificate_hash :: Text,
    size :: Word64,
    created_at :: UTCTime,
    locations :: [Text]
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

data Beacon = Beacon
  { network :: Text,
    epoch :: Word64,
    immutable_file_number :: Word64
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

digestOf :: Beacon -> Text
digestOf Beacon {network, epoch, immutable_file_number} =
  Hex.encodeBase16 $ encodeUtf8 $ "digest-" <> network <> "-" <> show epoch <> "-" <> show immutable_file_number

data Aggregator = Aggregator {aggregatorPort :: Int}

data AggregatorLog
  = AggregatorStarted Int
  | StartingAggregator {workDirectory :: FilePath}
  | AggregatorLog {logEntry :: Text}
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

withAggregator :: FilePath -> Tracer IO AggregatorLog -> (Aggregator -> IO a) -> IO a
withAggregator workDir tracer action = do
  port <- randomUnusedTCPPort
  process <- aggregatorProcess (Just workDir) port
  let logFile = workDir </> "aggregator.log"
  traceWith tracer (StartingAggregator workDir)
  withFile logFile WriteMode $ \out ->
    withCreateProcess process {std_out = UseHandle out, std_err = UseHandle out} $ \_stdin _stdout _stderr processHandle ->
      ( race
          (checkProcessHasNotDied "mithril-aggregator" processHandle)
          (traceWith tracer (AggregatorStarted port) >> action (Aggregator port))
          >>= \case
            Left _ -> error "should never happen"
            Right a -> pure a
      )
        `onException` (BS.readFile logFile >>= BS.putStr)

aggregatorProcess :: Maybe FilePath -> Int -> IO CreateProcess
aggregatorProcess cwd port = do
  binDir <- Pkg.getBinDir
  baseEnv <- getEnvironment
  let aggregator = binDir </> "mithril-aggregator"
      env =
        Just $
          [ ("NETWORK", "testnet"),
            ("URL_SNAPSHOT_MANIFEST", "https://storage.googleapis.com/cardano-testnet/snapshots.json"),
            ("SNAPSHOT_STORE_TYPE", "local"),
            ("SNAPSHOT_UPLOADER_TYPE", "local"),
            ("PENDING_CERTIFICATE_STORE_DIRECTORY", "./store/pending-certs"),
            ("CERTIFICATE_STORE_DIRECTORY", "./store/certs"),
            ("VERIFICATION_KEY_STORE_DIRECTORY", "./store/certs")
          ]
            <> baseEnv
  unlessM (doesFileExist aggregator) $ failure $ "cannot find mithril-aggregator executable in expected location (" <> binDir <> ")"
  pure $ (proc aggregator ["--db-directory", "db", "--server-port", show port, "--runtime-interval", "5", "-vvv"]) {cwd, env}
