{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}

module Mithril.Filter where

import Codec.Serialise (Serialise (..), serialise)
import Data.BloomFilter.Hash (Hashable)
import Data.ByteString (ByteString, pack)
import Data.ByteString.Lazy (toStrict)
import qualified Data.Map as Map
import Data.Tree.MerklePatricia (Blake2b_224, fromList, MerklePatriciaTree)
import GHC.Generics (Generic)
import GHC.Natural (Natural)
import Generic.Random (genericArbitrary, uniform)
import Test.QuickCheck
  ( Arbitrary (..),
    Gen,
    NonEmptyList (getNonEmpty),
    choose,
    vectorOf,
  )

-- * Types

-- | A reasonably accurate representation of a UTXO.
-- This type is meant for simulation and measurements purpose, we are only interested in
-- having a structure with the same order of magnitude of size and relevant elements for
-- indexing and searching.
data Utxo = Utxo
  { txRef :: TxRef,
    address :: Address,
    datum :: DatumHash,
    value :: Value
  }
  deriving (Eq, Show, Generic, Serialise)

newtype Address = Address ByteString
  deriving newtype (Eq, Show, Serialise, Hashable)

instance Arbitrary Address where
  arbitrary = Address <$> genHash

newtype DatumHash = DatumHash ByteString
  deriving newtype (Eq, Show, Serialise)

instance Arbitrary DatumHash where
  arbitrary = DatumHash <$> genHash

newtype MonetaryPolicy = MonetaryPolicy ByteString
  deriving newtype (Eq, Show, Ord, Serialise)

instance Arbitrary MonetaryPolicy where
  arbitrary = MonetaryPolicy <$> genHash

newtype AssetName = AssetName ByteString
  deriving newtype (Eq, Show, Serialise)

instance Arbitrary AssetName where
  arbitrary = do
    len <- choose (0, 128)
    AssetName . pack <$> vectorOf len arbitrary

data Value = Value
  { coins :: Integer,
    assets :: Map.Map MonetaryPolicy [(AssetName, Integer)]
  }
  deriving (Eq, Show, Generic, Serialise)

instance Arbitrary Value where
  arbitrary = genericArbitrary uniform

newtype TxId = TxId {txId :: ByteString}
  deriving newtype (Eq, Show, Serialise)

instance Arbitrary TxId where
  arbitrary = TxId <$> genHash

newtype TxIx = TxIx {txIx :: Natural}
  deriving newtype (Eq, Show, Serialise)

instance Arbitrary TxIx where
  arbitrary = TxIx . fromInteger <$> choose (1, 1024)

newtype TxRef = TxRef (TxId, TxIx)
  deriving newtype (Eq, Show, Serialise)

instance Arbitrary TxRef where
  arbitrary = TxRef <$> arbitrary


-- * Functions

-- | Helper function to build a MPT for all given UTXO.
-- The key is the serialised 'txRef' and the value is the UTXO itself.
makeTree :: [Utxo] -> MerklePatriciaTree Blake2b_224 Utxo
makeTree utxos =
  fromList $ zip (toStrict . serialise . txRef <$> utxos) utxos

-- * Generators

-- | Some UTXO set along with one UTXO included in the set and one not included.
-- This is used for tests and properties verification.
data Utxos = Utxos
  { utxoSet :: [Utxo],
    inUtxo :: Utxo,
    notInUtxo :: Utxo
  }
  deriving (Eq, Show)

instance Arbitrary Utxo where
  arbitrary = genericArbitrary uniform

instance Arbitrary Utxos where
  arbitrary = do
    utxoSet <- getNonEmpty <$> arbitrary
    -- TODO(AB): There's a non-null probability the generated Utxo is actually
    -- included, check that
    otherUtxo <- arbitrary
    pure $
      Utxos
        { utxoSet,
          inUtxo = head utxoSet,
          notInUtxo = otherUtxo
        }

genHash :: Gen ByteString
genHash = pack <$> vectorOf 28 arbitrary
