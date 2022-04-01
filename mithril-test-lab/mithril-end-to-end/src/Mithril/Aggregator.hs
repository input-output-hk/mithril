{-# LANGUAGE DeriveAnyClass #-}

-- | Controls Mithril aggregator service.
module Mithril.Aggregator where

import Control.Tracer (Tracer, traceWith)
import Hydra.Prelude

data Aggregator = Aggregator {aggregatorPort :: Int}

data AggregatorLog = AggregatorStarted Int
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

withAggregator :: Tracer IO AggregatorLog -> (Aggregator -> IO a) -> IO a
withAggregator tracer action = do
  traceWith tracer (AggregatorStarted 0)
  action (Aggregator 0)
