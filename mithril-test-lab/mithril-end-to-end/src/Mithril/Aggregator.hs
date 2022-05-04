{-# LANGUAGE DeriveAnyClass #-}

-- | Controls Mithril aggregator service.
module Mithril.Aggregator where

import Control.Tracer (Tracer, traceWith)
import qualified Data.ByteString as BS
import Hydra.Prelude
import qualified Paths_mithril_end_to_end as Pkg
import System.Directory (doesFileExist)
import System.Environment (getEnvironment)
import System.FilePath ((</>))
import System.Process (CreateProcess (..), StdStream (UseHandle), proc, withCreateProcess)
import Test.Hydra.Prelude (checkProcessHasNotDied, failure)
import Test.Network.Ports (randomUnusedTCPPort)

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
            ("URL_SNAPSHOT_MANIFEST", "https://storage.googleapis.com/cardano-testnet/snapshots.json")
          ]
            <> baseEnv
  unlessM (doesFileExist aggregator) $ failure $ "cannot find mithril-aggregator executable in expected location (" <> binDir <> ")"
  pure $ (proc aggregator ["--server-port", show port]) {cwd, env}
