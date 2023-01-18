{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- {-# LANGUAGE NoImplicitPrelude #-}

module MithrilEra.Utils (writePlutusFile, encodePlutusData) where

import qualified Cardano.Api as Api
-- Contracts
import qualified MithrilEra.Contract as Contract
--
import PlutusTx.Prelude (Either (..), Maybe (Nothing), ($), (++), (>>=))
import System.FilePath ( FilePath )
import Prelude (IO, print, putStrLn)
import Data.Aeson ( encode )
import Cardano.Api (scriptDataToJson, ScriptDataJsonSchema (..))
import Cardano.Api.Shelley (fromPlutusData)
import PlutusTx (builtinDataToData, ToData (toBuiltinData))
import qualified Data.ByteString.Lazy as BSL

writePlutusFile :: FilePath -> IO ()
writePlutusFile filePath =
  Api.writeFileTextEnvelope filePath Nothing Contract.serialized >>= \case
    Left err -> print $ Api.displayError err
    Right () -> putStrLn $ "wrote validator to file " ++ filePath

encodePlutusData :: forall a. ToData a => a -> BSL.ByteString
encodePlutusData a = Data.Aeson.encode $ scriptDataToJson ScriptDataJsonDetailedSchema $ fromPlutusData (builtinDataToData $ toBuiltinData a)
