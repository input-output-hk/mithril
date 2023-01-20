{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module MithrilEra.Contract (validator, wrapped, serialized, hash, MithrilEraDatum (..), MithrilEraRedeemer (..)) where

import Cardano.Api
import Cardano.Api.Shelley (PlutusScript (PlutusScriptSerialised))
import Codec.Serialise (serialise)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Short as BSS
import MithrilEra.Shared (validatorHash, wrap)
import qualified Plutus.V2.Ledger.Api as PlutusV2
import PlutusTx
import PlutusTx.Prelude

newtype MithrilEraDatum = MithrilEraDatum Integer

PlutusTx.unstableMakeIsData ''MithrilEraDatum

newtype MithrilEraRedeemer = MithrilEraRedeemer Integer

PlutusTx.unstableMakeIsData ''MithrilEraRedeemer

-- Validator will return True if and only if redeemer value provided is strictly lower than the datum value, for demonstration only. 
-- The real usage will be verifying that the secret key used to sign the transaction is the one registered in the datum
run :: MithrilEraDatum -> MithrilEraRedeemer -> PlutusV2.ScriptContext -> Bool
run (MithrilEraDatum datum) (MithrilEraRedeemer redeemer) _ = redeemer < datum

-- Entry

wrapped :: BuiltinData -> BuiltinData -> BuiltinData -> ()
wrapped = wrap run

validator :: PlutusV2.Validator
validator = PlutusV2.mkValidatorScript $$(PlutusTx.compile [||wrapped||])

serialized :: PlutusScript PlutusScriptV2
serialized = PlutusScriptSerialised . BSS.toShort . BSL.toStrict . serialise $ validator

hash :: PlutusV2.ValidatorHash
hash = validatorHash validator
