module MonitorTests where


import Data.Foldable(foldl')
import qualified Data.List as List
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as HUnit
import qualified Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit(testCase)
import Test.Tasty.HUnit((@?=))


import Mithril.Monitor


obsAll :: Foldable f => Monitor msg a -> f msg -> Monitor msg a
obsAll = foldl' observe'

tests :: Tasty.TestTree
tests =
  Tasty.testGroup "MonitorTests"
    [ QC.testProperty "next (==n) correct" nextNCorrect
    , QC.testProperty "next (==n1) >> next (==n2)" nextComposeNext
    ]

nextNCorrect :: Int -> [Int] -> Bool
nextNCorrect n ls =
  getResult (next (==n) `obsAll` ls) == List.find (==n) ls


nextComposeNext :: Int -> Int -> [Int] -> Bool
nextComposeNext n1 n2 ls =
  getResult ((next (==n1) >> next (==n2)) `obsAll` ls) ==
            List.find (==n2) (drop 1 . dropWhile (/= n1) $ ls)

expectIncomplete :: (Show a, Eq a) => Monitor msg a -> HUnit.Assertion
expectIncomplete mon =
  case mon of
    Result _ -> HUnit.assertFailure "Not expecting result from monitor"
    Continue _ -> pure ()

expectResult :: (Show a, Eq a) => Monitor msg a -> a -> HUnit.Assertion
expectResult mon a =
  case mon of
    Result a' -> a' @?= a
    Continue _ -> HUnit.assertFailure "Expected monitor to be complete, but it is not"