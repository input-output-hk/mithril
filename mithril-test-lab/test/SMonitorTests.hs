module SMonitorTests where

import qualified Test.Tasty as Tasty
import Mithril.SMonitor as Monitor
import qualified Test.Tasty.QuickCheck as QC
import qualified Test.Tasty.HUnit as HUnit
import Data.Void (Void)
import Data.Foldable (foldl')
import Test.Tasty.HUnit ((@?=))
import qualified Data.Sequence as Seq
import Data.Sequence(Seq, (<|))
import Data.Word (Word64)
import qualified Data.Maybe as Maybe
import qualified Data.List as List
import Control.Applicative (some)

tests :: Tasty.TestTree
tests =
  Tasty.testGroup "Monitor Tests"
    [ opTests
    , traceTests
    ]



data ReqResponse =
    Request Int
  | Response Int
  deriving Eq

data MonOut =
    Complete Int
  | ErrRequestNotUnique Int
  | ErrTimeout Int
  deriving (Eq, Show)

type RRMon a = Monitor ReqResponse MonOut a
type Svc a = Monitor ReqResponse ReqResponse a

request :: RRMon Int
request =
  do  Request i <- next
      pure i

response :: RRMon Int
response =
  do  Response i <- next
      pure i

type IntMon a = Monitor Int Void a

intmon :: IntMon a -> [Int] -> Maybe a
intmon m i =
  let (_, m') = m `observeMany` i
  in  case getResult m' of
        Nothing -> Nothing
        Just (Left _) -> Nothing
        Just (Right i) -> Just i

opTests :: Tasty.TestTree
opTests =
  Tasty.testGroup "Monitor Unit Tests"
    [ QC.testProperty "next"
        (\l -> next `intmon` l == Maybe.listToMaybe l)

    , QC.testProperty "fail"
        (\l -> Maybe.isNothing (fail "xxx" `intmon` l))

    , QC.testProperty "onFail"
        (\l n -> (fail "xxx" `onFail` pure (n :: Int)) `intmon` l == Just n)

    , QC.testProperty "on"
        (\l n -> on (eq n) `intmon` l == Maybe.listToMaybe [k | k <- l, k == n] )

    , QC.testProperty "any"
        (\l n -> Monitor.any (eq <$> l) `intmon` [n] == Maybe.listToMaybe [k | k <- l, k == n])

    , QC.testProperty "all"
        (\n1 n2 -> Monitor.all [eq n1, eq n2] `intmon` [n1] == (if n1 == n2 then Just [n1,n1] else Nothing))

    -- , QC.testProperty "evenoddeven"
    --     (\l -> evenoddeven `intmon` l == evenOddEvenList l)
    ]
  where
    eq i =
      do  j <- next
          require (i == j)
          pure i

evenOddEvenList :: Integral a => [a] -> Maybe [a]
evenOddEvenList l =
  let (evens, l') = span even l
      (odds, l'') = span odd l'
      (evens', l''') = span even l''
  in case l''' of
    [] | not $ Prelude.any null [evens, odds, evens'] -> Just l
    _ -> Nothing


evenoddeven :: Monitor Int o [Int]
evenoddeven =
  do  evens <- some evenm
      odds <- some oddm
      evens' <- some evenm
      pure (evens ++ odds ++ evens')


evenm :: Monitor Int o Int
evenm =
  do  i <- next
      require (even i)
      pure i

oddm :: Monitor Int o Int
oddm =
  do  i <- next
      require (odd i)
      pure i




traceTests :: Tasty.TestTree
traceTests =
  Tasty.testGroup "Monitor Trace Tests"
    [ HUnit.testCase "Request Response - simpleTrace"
        (monitorTraceOutput requestResponse simpleTrace @?= (Complete <$> [1,3,2,4]))

    , HUnit.testCase "Request ID Unique - simpleTrace"
        (monitorTraceOutput requestIdUnique simpleTrace @?= [])

    , HUnit.testCase "Request ID Unique - overlappingReq"
        (monitorTraceOutput requestIdUnique overlappingReq @?= [ErrRequestNotUnique 2])

    , HUnit.testCase "Request Timeout -  simpleTrace"
        (monitorTraceOutput requestTimeout simpleTrace @?= [])

    , HUnit.testCase "Request Timeout - delayed simpleTrace"
        (monitorWithPostDelay 1000 requestTimeout simpleTrace @?= [])

    , HUnit.testCase "Request Timeout - delayed incomplete5"
        (monitorWithPostDelay 1000 requestTimeout incomplete5 @?= [ErrTimeout 2])
    ]



monitorTraceOutput :: RRMon a -> [ReqResponse] -> [MonOut]
monitorTraceOutput m msgs =
  let (out, _) = m `observeMany` msgs
  in outValues out

monitorWithPostDelay :: Word64 -> RRMon a -> [ReqResponse] -> [MonOut]
monitorWithPostDelay delay m r =
  let (out, m') = m `observeMany` r
      (out', _) = m' `observeInput` ClockDelta delay
      in outValues (out <> out')

simpleTrace :: [ReqResponse]
simpleTrace =
  [ Request 1
  , Response 1
  , Request 2
  , Request 3
  , Request 4
  , Response 3
  , Response 2
  , Response 4
  ]

overlappingReq :: [ReqResponse]
overlappingReq =
  [ Request 1
  , Response 1
  , Request 2
  , Request 3
  , Request 4
  , Response 3
  , Request 2   -- <-- extra request 2
  , Response 2
  , Response 4
  ]

-- no response to 2
incomplete5 :: [ReqResponse]
incomplete5 =
  [ Request 1
  , Response 1
  , Request 2
  , Request 3
  , Request 4
  , Response 3
  , Response 4
  ]

responseFor :: Int -> RRMon ()
responseFor i =
  do  j <- on response
      require (i == j)

-- every request eventually gets a corresponding response
requestResponse :: RRMon Void
requestResponse =
  everywhere $
    do  Request i <- next
        on (responseFor i)
        output (Complete i)


-- request id should be unique until the associated response is seen
requestIdUnique :: RRMon Void
requestIdUnique =
  everywhere $
    do  Request i <- next
        Monitor.any
          [ on $ do j <- on request
                    require (i == j)
                    output (ErrRequestNotUnique i)

          , on (responseFor i)
          ]

-- response should be seen within a certain amount of time
requestTimeout :: RRMon Void
requestTimeout =
  everywhere $
    do  Request i <- next
        on (responseFor i)
          `timeoutWith` output (ErrTimeout i)

  where
    timeoutWith = onTimeout 100

traceGen :: Int -> QC.Gen (Seq.Seq ReqResponse)
traceGen n =
  QC.oneof [ pure Seq.empty
           , do  g <- traceGen (n + 1)
                 g' <- g `insertRandom` Response n
                 pure $ Request n <| g'
           ]
  where
    insertRandom ls m =
      do  i <- QC.chooseInt (0, Seq.length ls)
          pure $ Seq.insertAt i m ls
