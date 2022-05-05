{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}

-- | Runs a mithril-client synchronously.
module Mithril.Client where

import Hydra.Prelude
import Mithril.Aggregator (Aggregator (..))
import qualified Paths_mithril_end_to_end as Pkg
import System.Directory (doesFileExist)
import System.Environment (getEnvironment)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.Process (CreateProcess (..), proc, waitForProcess, withCreateProcess)
import Test.Hydra.Prelude (failure)

runClient :: FilePath -> Aggregator -> [Text] -> IO ()
runClient workDir Aggregator {aggregatorPort} arguments = do
  process <- clientProcess (Just workDir) aggregatorEndpoint (toString <$> arguments)
  withCreateProcess process $ \_stdin _stdout _stderr processHandle -> do
    waitForProcess processHandle >>= \case
      ExitSuccess -> pure ()
      ExitFailure code -> failure $ "mithril-client failed with code: " <> show code
  where
    aggregatorEndpoint = "http://localhost:" <> show aggregatorPort <> "/aggregator"

clientProcess :: Maybe FilePath -> String -> [String] -> IO CreateProcess
clientProcess cwd aggregatorEndpoint args = do
  binDir <- Pkg.getBinDir
  baseEnv <- getEnvironment
  let client = binDir </> "mithril-client"
      env =
        Just $
          [ ("AGGREGATOR_ENDPOINT", toString aggregatorEndpoint),
            ("NETWORK", "testnet")
          ]
            <> baseEnv
  unlessM (doesFileExist client) $ failure $ "cannot find mithril-client executable in expected location (" <> binDir <> ")"
  pure $ (proc client args) {cwd, env}
