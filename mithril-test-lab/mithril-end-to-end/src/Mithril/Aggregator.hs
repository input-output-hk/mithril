{-# LANGUAGE DeriveAnyClass #-}

-- | Controls Mithril aggregator service.
module Mithril.Aggregator where

import Control.Tracer (Tracer, traceWith)
import Hydra.Prelude

data Aggregator = Aggregator {aggregatorPort :: Int}

data AggregatorLog = AggregatorStarted Int
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- TODO: start an aggregator server on some default configuration that allocates random
-- port and wait for 'action' to terminate before closing the server.
withAggregator :: Tracer IO AggregatorLog -> (Aggregator -> IO a) -> IO a
withAggregator tracer action = do
  traceWith tracer (AggregatorStarted 0)
  action (Aggregator 0)
