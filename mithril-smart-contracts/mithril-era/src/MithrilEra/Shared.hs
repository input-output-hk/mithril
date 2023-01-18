{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module MithrilEra.Shared (wrap, validatorHash) where

import qualified Cardano.Api.Shelley as Shelly
import Codec.Serialise (serialise)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Short as BSS
import qualified Plutus.V1.Ledger.Scripts as Scripts
import PlutusTx
import PlutusTx.Prelude

wrap ::
  forall a b c.
  ( UnsafeFromData a,
    UnsafeFromData b,
    UnsafeFromData c
  ) =>
  (a -> b -> c -> Bool) ->
  BuiltinData ->
  BuiltinData ->
  BuiltinData ->
  ()
wrap f a b c =
  check
    ( f
        (unsafeFromBuiltinData a)
        (unsafeFromBuiltinData b)
        (unsafeFromBuiltinData c)
    )

validatorHash :: Scripts.Validator -> Scripts.ValidatorHash
validatorHash = Scripts.ValidatorHash . Scripts.getScriptHash . scriptHash . Scripts.getValidator

scriptHash :: Scripts.Script -> Scripts.ScriptHash
scriptHash =
  Scripts.ScriptHash
    . toBuiltin
    . Shelly.serialiseToRawBytes
    . Shelly.hashScript
    . toCardanoApiScript

toCardanoApiScript :: Scripts.Script -> Shelly.Script Shelly.PlutusScriptV2
toCardanoApiScript =
  Shelly.PlutusScript Shelly.PlutusScriptV2
    . Shelly.PlutusScriptSerialised
    . BSS.toShort
    . BSL.toStrict
    . serialise