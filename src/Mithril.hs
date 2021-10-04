{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}

module Mithril where

import Codec.Serialise (Serialise (..), serialise)
import Data.ByteString (ByteString, pack)
import Data.ByteString.Lazy (toStrict)
import qualified Data.Map as Map
import Data.Maybe (isJust, isNothing)
import Data.Tree.MerklePatricia (Blake2b_224, MerklePatriciaTree (..), fromList, mkProof)
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

genHash :: Gen ByteString
genHash = pack <$> vectorOf 28 arbitrary

newtype Address = Address ByteString
  deriving newtype (Eq, Show, Serialise)

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

data Utxo = Utxo
  { txRef :: TxRef,
    address :: Address,
    datum :: DatumHash,
    value :: Value
  }
  deriving (Eq, Show, Generic, Serialise)

data Utxos = Utxos
  { utxoSet :: [Utxo],
    inUtxo :: ByteString,
    notInUtxo :: ByteString
  }
  deriving (Eq, Show)

instance Arbitrary Utxo where
  arbitrary = genericArbitrary uniform

instance Arbitrary Utxos where
  arbitrary = do
    utxoSet <- getNonEmpty <$> arbitrary
    otherUtxo <- arbitrary
    pure $
      Utxos
        { utxoSet,
          inUtxo = toStrict $ serialise $ txRef $ head utxoSet,
          notInUtxo = toStrict $ serialise $ txRef otherUtxo
        }

makeTree :: [Utxo] -> MerklePatriciaTree Blake2b_224 Utxo
makeTree utxos =
  fromList $ zip (toStrict . serialise . txRef <$> utxos) utxos
