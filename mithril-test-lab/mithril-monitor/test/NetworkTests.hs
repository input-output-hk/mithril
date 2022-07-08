module NetworkTests where

import Mithril.Process.Network
import Mithril.Process.Process
import qualified Test.Tasty.HUnit as HUnit
import qualified System.Random as Random
import qualified Test.Tasty as Tasty
import Data.Typeable(Typeable)

newtype GetIntReq = GetIntReq (SendPort GetIntResp)
newtype GetIntResp = GetIntResp Integer

tests :: Tasty.TestTree
tests =
  Tasty.testGroup "Network Tests"
    [ HUnit.testCase "hello"
        (pure () `assertOutput` const True)
    , HUnit.testCase "main"
        (main `assertOutput` all (inAscendingOrder))
    ]

inAscendingOrder :: Ord a => [a] -> Bool
inAscendingOrder [] = True
inAscendingOrder [_] = True
inAscendingOrder (e1:e2:r) | e1 < e2 = inAscendingOrder (e2:r)
                           | otherwise = False

intSvc :: RecvPort GetIntReq -> Integer -> Proc ()
intSvc reqPort = go
  where
    go n =
      do  logmsg ("intSvc: current value: " ++ show n)
          GetIntReq respPort <- recv reqPort
          send respPort (GetIntResp n)
          go (n + 1)


intClient :: SendPort GetIntReq -> SendPort (Integer, Integer, Integer) -> Proc ()
intClient server output =
  do  i1 <- getInt
      i2 <- getInt
      i3 <- getInt
      send output (i1, i2, i3)
  where
    getInt =
      do  (sendP, recvP) <- newPort
          send server (GetIntReq sendP)
          GetIntResp i <- recv recvP
          logmsg ("intClient: got response " ++ show i)
          pure i


main :: Proc [[Integer]]
main =
  do  (svcSend, svcRecv) <- newPort
      (clientSend, clientRecv) <- newPort

      logmsg "main: forked client 1"
      fork (intClient svcSend clientSend)

      logmsg "main: forked client 2"
      fork (intClient svcSend clientSend)

      logmsg "main: forked int svc"
      fork (intSvc svcRecv 0)

      logmsg "main: recv a"
      (a1,a2,a3) <- recv clientRecv

      logmsg "main: recv b"
      (b1,b2,b3) <- recv clientRecv

      let result = [[a1,a2,a3],[b1,b2,b3]]
      logmsg $ "process complete: " ++ show result
      pure result

assertOutput :: (Show a, Typeable a) => Proc a -> (a -> Bool) -> HUnit.Assertion
assertOutput process f =
  do  r <- Random.getStdGen
      case runProcess process r of
        (log, Left err) -> HUnit.assertFailure ("Failed to run process: " ++ err)
        (log, Right a) ->
          if f a
            then  pure ()
            else
              do  HUnit.assertFailure ("Test case failed - output: " ++ show a)


runMain :: Int -> IO ()
runMain i =
  do  let gen = Random.mkStdGen i
      let (log, Right r) = runProcess main gen
      putStrLn "Log:"
      putStrLn (unlines log)
      putStrLn ""
      putStrLn ("Result: " ++ show r)



