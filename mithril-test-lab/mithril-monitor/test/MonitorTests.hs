module MonitorTests where

import qualified Test.Tasty as Tasty
import Mithril.Monitor.Monitor as Monitor
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
import Control.Monad(forever)

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

obsNoOut :: IntMon a -> [Int] -> IntMon a
obsNoOut m ins = snd $ m `observeMany` ins

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

    , QC.testProperty "allIntList" allIntList

    , QC.testProperty "bothIntList" bothIntList
    , QC.testProperty "watchIntList" watchIntList
    , QC.testProperty "raceIntList" raceIntList
    , QC.testProperty "eitherIntList" eitherIntList
    , QC.testProperty "guardIntList" guardIntList
    , QC.testProperty "alwaysIntList" alwaysIntList
    , QC.testProperty "neverIntList" neverIntList
    , QC.testProperty "eventuallyIntList" eventuallyIntList
    , QC.testProperty "onIntList" onIntList
    , QC.testProperty "accept" (\i -> accept (==i) `intmon` [i] == Just i)
    , QC.testProperty "accept i,j"
        (\i j -> accept (==i) `intmon` [j] == if i == j then Just i else Nothing)
    , QC.testProperty "parallelIntList" parallelIntList
    , QC.testProperty "collectUntilIntList" collectUntilIntList
    , QC.testProperty "traceIntList" traceIntList
    ]

eq :: Int -> IntMon Int
eq i =
  do  j <- next
      require (i == j)
      pure i

onEq :: Int -> IntMon Int
onEq = on . eq


oeoe :: Monitor Int o [Int]
oeoe =
  do  o1 <- accept odd
      e1 <- accept even
      o2 <- accept odd
      e2 <- accept even
      pure [o1, e1, o2, e2]

allIntList :: [Int] -> [Int] -> Bool
allIntList l1 l2 = Monitor.all (onEq <$> l1) `intmon` l2 == expected
  where
    expected =
      if Prelude.all (`elem` l2) l1
        then Just l1
        else Nothing

bothIntList :: [Int] -> Int -> Int -> Bool
bothIntList l n1 n2 = both (onEq n1) (onEq n2) `intmon` l == expected
  where
  expected =
    if n1 `elem` l && n2 `elem` l
      then Just (n1, n2)
      else Nothing

watchIntList :: [Int] -> [Int] -> Bool
watchIntList l1 = go (onEq <$> l1)
  where
    go ms ls =
      let r = watch ms `intmon` ls
      in case (ls, r) of
          ([], Nothing) -> True
          (h:l2', Just (as, ms')) | h `elem` l1 ->
                                    Prelude.all (==h) as &&  go ms' l2'
                                  | otherwise   -> null as && go ms' l2'
          _ -> False


raceIntList :: [Int] -> Int -> Int -> Bool
raceIntList l1 n1 n2 =
  race (onEq n1) (onEq n2) `intmon` l1 == expected
  where
    expected = List.find (`elem` [n1, n2]) l1

eitherIntList :: [Int] -> Bool
eitherIntList l = Monitor.either (accept even) (accept odd) `intmon` l == expected
  where
    expected =
      case l of
        [] -> Nothing
        h:_ | even h -> Just (Left h)
            | otherwise -> Just (Right h)

guardIntList :: [Int] -> Bool
guardIntList ls =
  Monitor.guard (forever $ accept even) (eq `traverse ` ls) `intmon` ls == expected
  where
    expected =
      if Prelude.all even ls
        then Just ls
        else Nothing

alwaysIntList :: [Int] -> Bool
alwaysIntList ls =
  Monitor.isFailed (Monitor.always (accept even) `obsNoOut` ls) /= Prelude.all even ls

neverIntList :: [Int] -> Bool
neverIntList ls =
  Monitor.isFailed (Monitor.never (accept even) `obsNoOut` ls) == Prelude.any even ls

eventuallyIntList :: [Int] -> Bool
eventuallyIntList ls = eventually oeoe `intmon` ls == expected ls
  where
    expected l =
      case l of
        o1:e1:o2:e2:t | odd o1 && odd o2 && even e1 && even e2 -> Just [o1,e1,o2,e2]
                      | otherwise -> expected (e1:o2:e2:t)
        _ -> Nothing

onIntList :: [Int] -> Bool
onIntList ls = on oeoe `intmon` ls == expected
  where
    expected = goEx ls scheme []
    scheme = [odd, even, odd, even]
    goEx l s acc =
      case (l,s) of
        (_, []) -> Just acc
        ([], _) -> Nothing
        (lh:l', sh:s') | sh lh -> goEx l' s' (acc ++ [lh])
                       | otherwise -> goEx l' scheme []

parallelIntList :: [Int] -> Bool
parallelIntList ls = ((,) <$> on (accept odd) <|*|> on (accept even)) `intmon` ls == expected
  where
    expected =
      do  i <- List.find odd ls
          j <- List.find even ls
          pure (i, j)

collectUntilIntList :: [Int] -> Bool
collectUntilIntList ls =
  [accept even, next >> accept even, next >> next >> accept even ] `collectUntil` on (accept odd) `intmon` ls == expected
  where
    expected =
      do  o <- List.find odd ls
          pure (take 3 $ takeWhile even ls, o)

traceIntList :: [Int] -> Bool
traceIntList ls = trace (on $ accept odd) `intmon` ls == expected
  where
    expected =
      do o <- List.find odd ls
         pure (o, List.takeWhile even ls ++ [o])

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
