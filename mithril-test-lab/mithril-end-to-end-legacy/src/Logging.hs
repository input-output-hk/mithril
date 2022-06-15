{-# LANGUAGE DeriveAnyClass #-}

module Logging where

import CardanoNode (CardanoNodeConfig, NodeId, NodeLog)
import Hydra.Prelude

--
-- Logging
--

data ClusterLog
  = MsgFromNode NodeId NodeLog
  | MsgNodeStarting CardanoNodeConfig
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)
