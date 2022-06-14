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
import Hydra.Prelude
import Logging (ClusterLog (..))
import Mithril.TestRunner (runTestRunner)
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
              RunningCluster {clusterNodes = _node@(RunningNode _ nodeSocket) : _} -> do
                waitForDBToBePopulated (takeDirectory nodeSocket)
                void $ runTestRunner (takeDirectory nodeSocket) (takeDirectory nodeSocket </> "db")
              _ -> failure "No nodes in the cluster"

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
      isChunk = (== ".chunk") . takeExtension
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
