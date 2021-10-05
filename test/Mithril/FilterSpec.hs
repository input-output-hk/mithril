{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeApplications #-}

module Mithril.FilterSpec where

import Codec.Serialise (serialise)
import Data.Array.Unboxed (bounds)
import Data.BloomFilter (bitArray)
import qualified Data.BloomFilter.Easy as Bloom
import Data.ByteString.Lazy (toStrict)
import qualified Data.ByteString.Lazy as LBS
import Data.Function ((&))
import Data.Maybe (isJust, isNothing)
import Data.Tree.MerklePatricia (mkProof)
import Mithril.Filter
import Test.Hspec
import Test.Hspec.QuickCheck (modifyMaxSuccess, prop)
import Test.QuickCheck
  ( Arbitrary (..),
    checkCoverage,
    collect,
    counterexample,
    cover,
    generate,
    vectorOf,
  )

spec :: Spec
spec = parallel $ do
  describe "Merkle-Patricia Tree" $ do
    prop "builds merkle-tree for UTXO and provide proofs" $ \Utxos {utxoSet, inUtxo, notInUtxo} ->
      let mpt = makeTree utxoSet
          proofIsIn = mkProof (toStrict $ serialise $ txRef inUtxo) mpt
          proofIsNotIn = mkProof (toStrict $ serialise $ txRef notInUtxo) mpt
       in isJust proofIsIn
            && isNothing proofIsNotIn

    modifyMaxSuccess (const 70) $
      prop "proof of inclusion is significantly smaller than full UTXO set" $ \Utxos {utxoSet, inUtxo} ->
        let mpt = makeTree utxoSet
            utxoBytes = serialise utxoSet
            Just proofIsIn = mkProof (toStrict $ serialise $ txRef inUtxo) mpt
            proofSize = LBS.length (serialise proofIsIn)
         in proofSize < LBS.length utxoBytes
              & collect (show (proofSize * 100 `div` LBS.length utxoBytes) <> "%")
              & counterexample ("Proof: " <> show proofIsIn)
              & counterexample ("Proof size:  " <> show proofSize)

  describe "Addresses Bloom Filter" $ do
    modifyMaxSuccess (const 1000) $
      prop "builds bloom filter for addresses" $ \Utxos {utxoSet, inUtxo, notInUtxo} ->
        let addresses = address <$> utxoSet
            addrFilter = Bloom.easyList 0.01 addresses
            isIn = Bloom.elem (address inUtxo) addrFilter
            isOut = not (Bloom.elem (address notInUtxo) addrFilter)
         in isIn
              & cover 99 isOut "true negative"
              & checkCoverage
              & counterexample ("Should be in Utxo, " <> show (address inUtxo) <> ", " <> show isIn)
              & counterexample ("Should not be in Utxo, " <> show (address notInUtxo) <> ", " <> show isOut)

    modifyMaxSuccess (const 70) $
      prop "bloom filter is significantly smaller than serialised UTXO set" $ \Utxos {utxoSet} ->
        let utxoBytes = serialise utxoSet
            addresses = address <$> utxoSet
            addrFilter = bitArray $ Bloom.easyList 0.01 addresses
            (lb, ub) = bounds addrFilter
            sizeOfBloomFilter = 4 * (ub - lb)
            sizeOfUtxo = LBS.length utxoBytes
         in sizeOfBloomFilter < truncate (0.1 * fromIntegral @_ @Double sizeOfUtxo)
              & collect ("< " <> show (sizeOfBloomFilter * 100 `div` fromIntegral sizeOfUtxo + 1) <> "%")

    it "size for 10,000 UTXO" $ do
      utxoSet <- generate $ vectorOf 10_000 arbitrary
      let addrFilter = bitArray $ Bloom.easyList 0.001 $ address <$> utxoSet
          (lb, ub) = bounds addrFilter
          sizeOfBloomFilter = 4 * (ub - lb)

      putStrLn $ "Utxo Size: " <> show (LBS.length (serialise utxoSet) `div` (1024 * 1024)) <> "MB"
      putStrLn $ "Filter Size: " <> show sizeOfBloomFilter
