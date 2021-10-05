{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}

module MithrilSpec where

import Codec.Serialise (Serialise (..), serialise)
import qualified Data.BloomFilter.Easy as Bloom
import Data.ByteString (ByteString, pack)
import Data.ByteString.Lazy (toStrict)
import Data.Function ((&))
import qualified Data.Map as Map
import Data.Maybe (isJust, isNothing)
import Data.Tree.MerklePatricia (Blake2b_224, MerklePatriciaTree (..), fromList, mkProof)
import GHC.Generics (Generic)
import GHC.Natural (Natural)
import Generic.Random (genericArbitrary, uniform)
import Mithril
import Test.Hspec
import Test.Hspec.QuickCheck (prop, modifyMaxSuccess)
import Test.QuickCheck
  ( Arbitrary (..),
    Gen,
    NonEmptyList (getNonEmpty),
    checkCoverage,
    choose,
    cover, 
    counterexample,
    vectorOf,
  )

spec :: Spec
spec = parallel $ do
  prop "builds merkle-tree for UTXO and provide proofs" $ \Utxos {utxoSet, inUtxo, notInUtxo} ->
    let mpt = makeTree utxoSet
     in isJust (mkProof (toStrict $ serialise $ txRef inUtxo) mpt)
          && isNothing (mkProof (toStrict $ serialise $ txRef notInUtxo) mpt)

  modifyMaxSuccess (const 1000) $
    prop "builds bloom filter for addresses" $ \Utxos {utxoSet, inUtxo, notInUtxo} ->
      let addresses = address <$> utxoSet
          addrFilter = Bloom.easyList 0.01 addresses
          isIn = Bloom.elem (address inUtxo) addrFilter
          isOut = not (Bloom.elem (address notInUtxo) addrFilter)
       in isIn
          & cover 99 isOut "true negative"
          & checkCoverage
          & counterexample ("\nShould be in Utxo, " <> show (address inUtxo) <> ", " <> show isIn)
          & counterexample ("\nShould not be in Utxo, " <> show (address notInUtxo) <> ", " <> show isOut)
