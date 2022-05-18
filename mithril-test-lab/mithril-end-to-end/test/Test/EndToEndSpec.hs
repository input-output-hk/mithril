{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Test.EndToEndSpec where

import CardanoCluster
  ( ClusterConfig (..),
    RunningCluster (..),
    defaultNetworkId,
    withCluster,
  )
import CardanoNode (ChainTip (..), RunningNode (..), cliQueryTip)
import Control.Monad.Class.MonadSTM (modifyTVar, newTVarIO, readTVarIO)
import Control.Monad.Class.MonadSay (MonadSay, say)
import Control.Tracer (Tracer (Tracer))
import Data.Aeson (Value, eitherDecode)
import qualified Data.Text as Text
import Hydra.Prelude
import Logging (ClusterLog (..))
import Mithril.Aggregator
  ( Aggregator (..),
    Certificate (Certificate, participants),
    CertificatePending (CertificatePending, beacon),
    digestOf,
    withAggregator,
  )
import Mithril.Client (runClient)
import Mithril.Signer (Signer, withSigner)
import Network.HTTP.Simple (getResponseBody, getResponseStatusCode, httpJSON, httpLBS, parseRequest)
import System.Directory (listDirectory)
import System.FilePath (takeDirectory, takeExtension, (</>))
import Test.Hydra.Prelude

spec :: Spec
spec =
  it "should produce and verify snapshot" $ do
    showLogsOnFailure $ \tr ->
      withTempDir "hydra-cluster" $ \tmp -> do
        let config =
              ClusterConfig
                { parentStateDirectory = tmp,
                  networkId = defaultNetworkId,
                  primedDB = Just "test-db.tar.gz"
                }
        -- Start cardano nodes cluster
        withCluster tr config $
          \cluster -> do
            -- basic verification, we check the nodes are producing blocks
            assertNetworkIsProducingBlock tr cluster
            case cluster of
              RunningCluster {clusterNodes = node@(RunningNode _ nodeSocket) : _} -> do
                waitForDBToBePopulated (takeDirectory nodeSocket)
                -- Start aggregator service on some random port
                withAggregator (takeDirectory nodeSocket) (contramap AggregatorLog tr) $ \aggregator@Aggregator {aggregatorPort} -> do
                  threadDelay 2
                  digest <- assertNodeIsProducingSnapshot tr node aggregatorPort
                  withSigner tmp (contramap SignerLog tr) aggregatorPort node $ \signer -> do
                    assertSignerIsSigningSnapshot signer aggregatorPort digest
                    assertClientCanVerifySnapshot tmp aggregator digest
              _ -> failure "No nodes in the cluster"

newtype Snapshots = Snapshots [Value]
  deriving newtype (FromJSON)

assertSignerIsSigningSnapshot :: Signer -> Int -> Text -> IO ()
assertSignerIsSigningSnapshot _signer aggregatorPort digest = go 10
  where
    go :: Int -> IO ()
    go 0 = failure "Timeout exhausted"
    go n = do
      request <- parseRequest $ "http://localhost:" <> show aggregatorPort <> "/aggregator/certificate/" <> Text.unpack digest
      response <- httpLBS request
      case getResponseStatusCode response of
        404 -> threadDelay 1 >> go (n -1)
        200 -> do
          let body = getResponseBody response
          case eitherDecode body of
            Right Certificate {participants} ->
              length participants `shouldBe` 5 -- FIXME: should be the number of registered signers but currently hardcoded
            Left err -> failure $ "invalid certificate body : " <> show err <> ", raw body: '" <> show body <> "'"
        other -> failure $ "unexpected status code: " <> show other

assertClientCanVerifySnapshot :: FilePath -> Aggregator -> Text -> IO ()
assertClientCanVerifySnapshot workDir aggregator digest =
  -- we only check the client exits with success
  void $ runClient workDir aggregator ["restore", digest]

assertNodeIsProducingSnapshot :: Tracer IO ClusterLog -> RunningNode -> Int -> IO Text
assertNodeIsProducingSnapshot _tracer _cardanoNode aggregatorPort = go 10
  where
    go :: Int -> IO Text
    go 0 = failure "Timeout exhausted"
    go n = do
      request <- parseRequest $ "http://localhost:" <> show aggregatorPort <> "/aggregator/certificate-pending"
      response <- httpLBS request
      case getResponseStatusCode response of
        204 -> threadDelay 1 >> go (n -1)
        200 -> do
          CertificatePending {beacon} <- getResponseBody <$> httpJSON request
          let digest = digestOf beacon
          putStrLn $ "Got beacon : " <> show beacon <> ", computed digest : " <> show digest
          pure $ digest
        other -> failure $ "unexpected status code: " <> show other

assertNetworkIsProducingBlock :: Tracer IO ClusterLog -> RunningCluster -> IO ()
assertNetworkIsProducingBlock tracer = failAfter 30 . go (-1)
  where
    go blk cluster = case cluster of
      RunningCluster _ (RunningNode nodeId socket : _) -> do
        waitForNewBlock
        tip <- cliQueryTip (contramap (MsgFromNode nodeId) tracer) socket
        if block tip > blk
          then pure ()
          else go (block tip) cluster
      _ ->
        error "empty cluster?"

waitForNewBlock :: IO ()
waitForNewBlock = threadDelay (2 * slotLength)

waitForDBToBePopulated :: FilePath -> IO ()
waitForDBToBePopulated nodeDirectory = do
  let immutableDir = nodeDirectory </> "db" </> "immutable"
      isChunk = ((== ".chunk") . takeExtension)
  immutableFiles <- filter isChunk <$> listDirectory immutableDir
  when (length immutableFiles < 2) $ do
    putStrLn $ "waiting for immutable directory " <> immutableDir <> " to have at least 2 chunk files"
    threadDelay 1 >> waitForDBToBePopulated nodeDirectory

slotLength :: DiffTime
slotLength = 1 -- FIXME this should be found in the genesis file

sshow :: (IsString s, Show a) => a -> s
sshow = fromString . show

showLogsOnFailure ::
  (MonadSTM m, MonadCatch m, Show msg, MonadSay m) =>
  (Tracer m msg -> m a) ->
  m a
showLogsOnFailure action = do
  tvar <- newTVarIO []
  action (traceInTVar tvar)
    `onException` (readTVarIO tvar >>= mapM_ (say . show) . reverse)

traceInTVar :: MonadSTM m => TVar m [a] -> Tracer m a
traceInTVar tvar = Tracer $ \a ->
  atomically $ modifyTVar tvar (a :)
