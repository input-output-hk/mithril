{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}

-- | Runs a mithril-end-to-end test runner synchronously.
module Mithril.TestRunner where

import Hydra.Prelude
import qualified Paths_mithril_end_to_end_legacy as Pkg
import System.Directory (doesFileExist)
import System.Environment (getEnvironment)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.Process (CreateProcess (..), proc, waitForProcess, withCreateProcess)
import Test.Hydra.Prelude (failure)

runTestRunner :: FilePath -> IO ()
runTestRunner workDir = do
  process <- testRunnerProcess (Just workDir)
  withCreateProcess process $ \_stdin _stdout _stderr processHandle -> do
    waitForProcess processHandle >>= \case
      ExitSuccess -> pure ()
      ExitFailure code -> failure $ "mithril-end-to-end test runner failed with code: " <> show code

testRunnerProcess :: Maybe FilePath -> IO CreateProcess
testRunnerProcess cwd = do
  binDir <- Pkg.getBinDir
  baseEnv <- getEnvironment
  let testRunner = binDir </> "mithril-end-to-end"
      env =
        Just $ []
            <> baseEnv
  unlessM (doesFileExist testRunner) $ failure $ "cannot find 'mithril-end-to-end' executable in expected location (" <> binDir <> ")"
  pure $ (proc testRunner ["--db-directory", "db", "--bin-directory", binDir]) {cwd, env}
