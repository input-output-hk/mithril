{-# LANGUAGE DeriveAnyClass #-}

module Logging where

import CardanoNode (CardanoNodeConfig, NodeId, NodeLog)
import Hydra.Prelude
import Mithril.Aggregator (AggregatorLog)
import Mithril.Signer (SignerLog)

--
-- Logging
--

data ClusterLog
  = AggregatorLog AggregatorLog
  | SignerLog SignerLog
  | MsgFromNode NodeId NodeLog
  | MsgNodeStarting CardanoNodeConfig
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)
