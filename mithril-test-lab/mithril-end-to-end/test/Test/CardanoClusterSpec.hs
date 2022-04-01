{-# LANGUAGE NoImplicitPrelude #-}

module Test.CardanoClusterSpec where

import CardanoCluster
  ( Actor (Alice),
    ClusterConfig (..),
    ClusterLog (..),
    RunningCluster (..),
    defaultNetworkId,
    keysFor,
    withCluster,
  )
import CardanoNode (ChainTip (..), RunningNode (..), cliQueryTip)
import Control.Monad.Class.MonadSTM (modifyTVar, newTVarIO, readTVarIO)
import Control.Monad.Class.MonadSay (MonadSay, say)
import Control.Tracer (Tracer (Tracer))
import Hydra.Prelude
import Test.Hydra.Prelude

spec :: Spec
spec =
  it "should produce blocks, provide funds, and send Hydra OCV transactions" $ do
    showLogsOnFailure $ \tr ->
      withTempDir "hydra-cluster" $ \tmp -> do
        let config =
              ClusterConfig
                { parentStateDirectory = tmp,
                  networkId = defaultNetworkId
                }
        withCluster tr config $ \cluster -> do
          failAfter 30 $ assertNetworkIsProducingBlock tr cluster

assertNetworkIsProducingBlock :: Tracer IO ClusterLog -> RunningCluster -> IO ()
assertNetworkIsProducingBlock tracer = go (-1)
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
