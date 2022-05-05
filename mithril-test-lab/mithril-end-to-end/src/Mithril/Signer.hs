{-# LANGUAGE DeriveAnyClass #-}

-- | Controls a Mithril signer service.
module Mithril.Signer where

import CardanoNode (RunningNode)
import Control.Tracer (Tracer, traceWith)
import qualified Data.ByteString as BS
import Hydra.Prelude
import qualified Paths_mithril_end_to_end as Pkg
import System.Directory (doesFileExist)
import System.Environment (getEnvironment)
import System.FilePath ((</>))
import System.Process (CreateProcess (..), StdStream (UseHandle), proc, withCreateProcess)
import Test.Hydra.Prelude (checkProcessHasNotDied, failure)

data SignerLog
  = StartingSigner FilePath
  | SignerStarted
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

data Signer = Signer
  { workDirectory :: FilePath,
    aggregatorEndpoint :: Text
  }
  deriving stock (Eq, Show, Generic)

-- TODO: starts a signer daemon that checks snapshots, sign them and send the signature
-- to the 'Aggregator' server listening at 'aggregatorPort', for the given running node
withSigner :: FilePath -> Tracer IO SignerLog -> Int -> RunningNode -> (Signer -> IO a) -> IO a
withSigner workDirectory tracer aggregatorPort _cardanoNode action = do
  process <- signerProcess (Just workDirectory) aggregatorEndpoint
  traceWith tracer (StartingSigner workDirectory)
  withFile logFile WriteMode $ \out ->
    withCreateProcess process {std_out = UseHandle out, std_err = UseHandle out} $ \_stdin _stdout _stderr processHandle ->
      ( race
          (checkProcessHasNotDied "mithril-signer" processHandle)
          (traceWith tracer SignerStarted >> action (Signer {workDirectory, aggregatorEndpoint}))
          >>= \case
            Left _ -> error "should never happen"
            Right a -> pure a
      )
        `onException` (BS.readFile logFile >>= BS.putStr)
  where
    aggregatorEndpoint = "http://localhost:" <> show aggregatorPort <> "/aggregator"
    logFile = workDirectory </> "signer.log"

signerProcess :: Maybe FilePath -> Text -> IO CreateProcess
signerProcess cwd aggregatorEndpoint = do
  binDir <- Pkg.getBinDir
  baseEnv <- getEnvironment
  let signer = binDir </> "mithril-signer"
      env =
        Just $
          [ ("AGGREGATOR_ENDPOINT", toString aggregatorEndpoint),
            ("NETWORK", "testnet"),
            ("PARTY_ID", "0"),
            ("RUN_INTERVAL", "5")
          ]
            <> baseEnv

  unlessM (doesFileExist signer) $ failure $ "cannot find mithril-signer executable in expected location (" <> binDir <> ")"
  pure $ (proc signer ["-vvv"]) {cwd, env}
