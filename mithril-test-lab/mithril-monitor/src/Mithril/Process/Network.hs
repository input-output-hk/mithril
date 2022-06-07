{-# LANGUAGE GADTs #-}
module Mithril.Process.Network where

import qualified Control.Monad.State as State
import Control.Monad ((>=>), ap, liftM, forever)
import Data.Typeable (cast, Typeable)
import qualified System.Random as Random
import qualified Data.List as List
import Data.Sequence (Seq ((:|>)) )
import qualified Data.Sequence as Seq
import qualified Data.Foldable as F

newtype SendPort a = SendPort Integer
  deriving (Eq, Ord)

newtype RecvPort a = RecvPort Integer
  deriving (Eq, Ord)

data NetworkSend where
  NetworkSend :: Typeable a => Integer -> SendPort a -> a -> NetworkSend

netSendId :: NetworkSend -> Integer
netSendId (NetworkSend i _ _) = i

netSendPort :: NetworkSend -> Integer
netSendPort (NetworkSend _ (SendPort i) _) = i

data NetworkRecv = NetworkRecv Integer [Recv ()]

netRecvId :: NetworkRecv -> Integer
netRecvId (NetworkRecv i _) = i

data NetworkState = NetworkState
  { netSends :: [NetworkSend]
  , netRecvs :: [NetworkRecv]
  , netIds :: Integer
  , netLog :: Seq String
  , netRandomGen :: Random.StdGen
  }

type Network = State.State NetworkState

data Recv a where
  Recv :: Typeable b => RecvPort b -> (b -> Proc a) -> Recv a

data Proc a where
  PRecv :: [Recv a] -> Proc a
  Fork :: Proc () -> Proc a -> Proc a
  Send :: Typeable b => SendPort b -> b -> Proc a -> Proc a
  MkPort :: ((SendPort b, RecvPort b) -> Proc a) -> Proc a
  Yield :: a -> Proc a
  Log :: String -> Proc a -> Proc a

instance Functor Proc where
  fmap = liftM

instance Applicative Proc where
  (<*>) = ap
  pure = Yield

instance Monad Proc where
  ma >>= fmb = case ma of
    PRecv recs -> PRecv (doRecv <$> recs)
    Fork forked main -> Fork forked (main >>= fmb)
    Send port msg p -> Send port msg (p >>= fmb)
    MkPort f -> MkPort (f >=> fmb)
    Yield a -> fmb a
    Log log p -> Log log (p >>= fmb)
    where
      doRecv (Recv p cont) = Recv p (cont >=> fmb)

interleavings :: NetworkState -> [(Proc (), NetworkState)]
interleavings ns =
  [ (p, deliver sendId recvId)
             | NetworkSend sendId (SendPort sPortId) msg <- netSends ns
             , NetworkRecv recvId recvs <- netRecvs ns
             , Recv (RecvPort rPortId) cont <- recvs
             , sPortId == rPortId
             , let (Just p) = cont <$> cast msg
             ]
  where
    deliver sendId recvId =
      ns { netSends = [s | s <- netSends ns, netSendId s /= sendId]
         , netRecvs = [r | r <- netRecvs ns, netRecvId r /= recvId]
         }

runScheduler :: Network ()
runScheduler =
  do  nextStates <- State.gets interleavings
      case nextStates of
        [] -> pure ()
        _ ->
          do  idx <- netRandom (Random.randomR (0, length nextStates - 1))
              let (prc, ns') = nextStates !! idx
              State.put ns'
              execute prc
              runScheduler

execute :: Proc () -> Network ()
execute p =
  case p of
    Yield a -> pure a
    Fork f m ->
      do  execute f
          execute m

    Send port msg p ->
      do  sendId <- netNewId
          execute p
          State.modify (\s -> s { netSends = NetworkSend sendId port msg : netSends s})

    MkPort f ->
      do  ports <- netMkPort
          execute (f ports)

    PRecv recs ->
      do  i <- netNewId
          let recs' = NetworkRecv i recs
          State.modify (\s -> s { netRecvs = recs' : netRecvs s})

    Log msg p ->
      do  State.modify (\s -> s { netLog = netLog s :|> msg  })
          execute p


netNewId :: Network Integer
netNewId =
  do  pid <- State.gets netIds
      State.modify (\s -> s { netIds = pid + 1})
      pure pid



netMkPort :: Network (SendPort a, RecvPort a)
netMkPort =
  do  pid <- netNewId
      pure (SendPort pid, RecvPort pid)

netRecvAll :: Typeable a => RecvPort a -> Network [a]
netRecvAll (RecvPort rid) =
  do  sends <- State.gets netSends
      let (recvs, rest) = List.partition (\s -> netSendPort s == rid) sends
      State.modify (\s -> s {netSends = rest})
      let as = [ castOrDie a | NetworkSend _ _ a <- recvs ]
      pure as
  where
    castOrDie a =
      case cast a of
        Just b -> b
        Nothing -> error "netRecvAll: cast failed!"

networkEmpty :: NetworkState
networkEmpty =
  NetworkState { netIds = 0
               , netRecvs = []
               , netSends = []
               , netLog = Seq.Empty
               , netRandomGen = Random.mkStdGen 0
               }

netSetRandomGen :: Random.StdGen -> Network ()
netSetRandomGen i = State.modify (\s -> s { netRandomGen = i })

netRandom :: (Random.StdGen -> (a, Random.StdGen)) -> Network a
netRandom r =
  do  (a, g') <- State.gets (r . netRandomGen)
      State.modify (\s -> s { netRandomGen = g' })
      pure a

runNetwork :: Network a -> NetworkState -> (a, NetworkState)
runNetwork = State.runState

runNetwork' :: Network a -> (a, NetworkState)
runNetwork' = (`runNetwork` networkEmpty)

eval :: Typeable a => Proc a -> Network (Maybe a)
eval p =
  do  (s, r) <- netMkPort
      execute (p >>= send s)
      runScheduler
      results <- netRecvAll r
      case results of
        [a] -> pure $ Just a
        _ -> pure Nothing


runProcessIO :: (Typeable a) => Proc a -> IO ([String], Either String a)
runProcessIO p = runProcess p <$> Random.getStdGen

runProcess :: (Typeable a) => Proc a -> Random.StdGen -> ([String], Either String a)
runProcess p g =
  let (result, ns) = runNetwork' net
  in (F.toList (netLog ns), result)
  where
    p' sendP = p >>= send sendP
    net =
      do  netSetRandomGen g
          (sendP, recvP) <- netMkPort
          execute (p' sendP)
          runScheduler
          recvs <- netRecvAll recvP
          case recvs of
            [] -> pure $ Left "Process did not complete"
            [r] -> pure $ Right r
            _ -> pure $ Left "[BUG] Process returned more than one result?"

----

send :: Typeable a => SendPort a -> a -> Proc ()
send port msg = Send port msg (pure ())

recv :: Typeable a => RecvPort a -> Proc a
recv port = PRecv [Recv port pure]

mkPort :: Typeable a => Proc (SendPort a, RecvPort a)
mkPort = MkPort pure

fork :: Proc () -> Proc ()
fork f = Fork f (pure ())

(===>) :: Typeable a => RecvPort a -> (a -> Proc b) -> Recv b
port ===> cont = Recv port cont

choose :: [Recv a] -> Proc a
choose = PRecv

async :: Typeable a => Proc a -> Proc (Proc a)
async p =
  do  (sendP, recvP) <- mkPort
      fork $ p >>= send sendP
      pure $ recv recvP

logmsg :: String -> Proc ()
logmsg msg = Log msg (pure ())

(<|*|>) :: (Typeable a, Typeable b) => Proc (a -> b) -> Proc a -> Proc b
pf <|*|> pa =
  do  pf' <- async pf
      pa' <- async pa
      pf' <*> pa'

tryGetResult :: Proc a -> Maybe a
tryGetResult p =
  case p of
    Yield a -> Just a
    _ -> Nothing

----

