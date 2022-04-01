{-# LANGUAGE DeriveAnyClass #-}

-- | Controls a Mithril signer service.
module Mithril.Signer where

import CardanoNode (RunningNode)
import Control.Tracer (Tracer, traceWith)
import Hydra.Prelude

data SignerLog = SignerStarted
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

data Signer = Signer

withSigner :: Tracer IO SignerLog -> Int -> RunningNode -> (Signer -> IO a) -> IO a
withSigner tracer _aggregatorPort _cardanoNode action = do
  traceWith tracer SignerStarted
  action Signer
