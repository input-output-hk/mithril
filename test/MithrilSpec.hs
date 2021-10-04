{-# LANGUAGE NamedFieldPuns #-}
module MithrilSpec where

import Test.Hspec
import Data.ByteString(ByteString)
import qualified Data.Map as Map
import Test.Hspec.QuickCheck (prop)
import Data.Maybe (isJust, isNothing)
import Data.Tree.MerklePatricia (mkProof, MerklePatriciaTree(..), Blake2b_224)
import Test.QuickCheck (Arbitrary(..))

newtype Address = Address ByteString
  deriving (Eq, Show)

newtype DatumHash = DatumHash ByteString
  deriving (Eq, Show)

data Value = Value { coins :: Integer,  assets :: Map.Map ByteString [(ByteString, Integer)]}
  deriving (Eq, Show)

data Utxo = Utxo
  { address :: Address,
    datum :: DatumHash,
    value :: Value
  }
  deriving (Eq, Show)

data Utxos = Utxos
  { utxoSet :: [Utxo],
    inUtxo :: ByteString,
    notInUtxo :: ByteString
  }
  deriving (Eq, Show)

instance Arbitrary Utxos where
  arbitrary = undefined
  
spec :: Spec
spec = parallel $ do
  prop "builds merkle-tree for UTXO and provide proofs" $ \Utxos {utxoSet, inUtxo, notInUtxo} ->
    let mpt = makeTree utxoSet
     in isJust (mkProof inUtxo mpt)
          && isNothing (mkProof notInUtxo mpt)

makeTree :: [Utxo] -> MerklePatriciaTree Blake2b_224 Utxo
makeTree = error "not implemented"
