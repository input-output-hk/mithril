{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}

module MithrilSpec where

import Mithril
import Codec.Serialise (Serialise (..), serialise)
import Data.ByteString (ByteString, pack)
import Data.ByteString.Lazy (toStrict)
import qualified Data.Map as Map
import Data.Maybe (isJust, isNothing)
import Data.Tree.MerklePatricia (Blake2b_224, MerklePatriciaTree (..), fromList, mkProof)
import GHC.Generics (Generic)
import GHC.Natural (Natural)
import Test.Hspec
import Generic.Random(genericArbitrary, uniform)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Arbitrary (..), NonEmptyList (getNonEmpty), vectorOf, choose, Gen)
import Generic.Random (genericArbitrary)

spec :: Spec
spec = parallel $ do
  prop "builds merkle-tree for UTXO and provide proofs" $ \Utxos {utxoSet, inUtxo, notInUtxo} ->
    let mpt = makeTree utxoSet
     in isJust (mkProof inUtxo mpt)
          && isNothing (mkProof notInUtxo mpt)
