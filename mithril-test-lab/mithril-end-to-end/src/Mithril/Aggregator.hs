{-# LANGUAGE DeriveAnyClass #-}

-- | Controls Mithril aggregator service.
module Mithril.Aggregator where

import Control.Tracer (Tracer, traceWith)
import Hydra.Prelude
import System.FilePath ((</>))
import System.Process (CreateProcess (..), StdStream (UseHandle), proc, withCreateProcess)
import Test.Hydra.Prelude (checkProcessHasNotDied)
import Test.Network.Ports (randomUnusedTCPPort)

data Aggregator = Aggregator {aggregatorPort :: Int}

data AggregatorLog
  = AggregatorStarted Int
  | StartingAggregator {workDirectory :: FilePath}
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- TODO: start an aggregator server on some default configuration that allocates random
-- port and wait for 'action' to terminate before closing the server.
withAggregator :: FilePath -> Tracer IO AggregatorLog -> (Aggregator -> IO a) -> IO a
withAggregator workDir tracer action = do
  port <- randomUnusedTCPPort
  let process = aggregatorProcess (Just workDir) port
      logFile = workDir </> "aggregator.log"
  traceWith tracer (StartingAggregator workDir)
  withFile logFile WriteMode $ \out ->
    withCreateProcess process {std_out = UseHandle out, std_err = UseHandle out} $ \_stdin _stdout _stderr processHandle ->
      race
        (checkProcessHasNotDied "mithril-aggregator" processHandle)
        (traceWith tracer (AggregatorStarted port) >> action (Aggregator port))
        >>= \case
          Left _ -> error "should never happen"
          Right a -> pure a

aggregatorProcess :: Maybe FilePath -> Int -> CreateProcess
aggregatorProcess cwd port = (proc "mithril-aggregator" ["--server-port", show port]) {cwd}
