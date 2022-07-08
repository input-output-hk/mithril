{-# LANGUAGE GADTs #-}
module Mithril.Process.Network
( Network
, NetworkState(..)
, RNG
, runScheduler
, execute
, mkPort
, recvAll
, empty
, setRandomGen
, withRandom
, runNetwork
, runNetwork'
, evalProcess
, runProcess
) where

-- TODO: the ideas here might be better expressed as a typeclass
--       implementing an interpreter of sorts for the actions
--       a Proc can take

import qualified Control.Monad.State as State
import Control.Monad ((>=>), ap, liftM, forever)
import Data.Typeable (cast, Typeable)
import qualified System.Random as Random
import qualified Data.List as List
import Data.Sequence (Seq ((:|>)) )
import qualified Data.Sequence as Seq
import qualified Data.Foldable as F
import Mithril.Process.Process
    ( send, Proc(..), Recv(..), RecvPort(..), SendPort(..) )

data NetworkSend where
  NetworkSend :: Typeable a => Integer -> SendPort a -> a -> NetworkSend

netSendId :: NetworkSend -> Integer
netSendId (NetworkSend i _ _) = i

netSendPort :: NetworkSend -> Integer
netSendPort (NetworkSend _ (SendPort i) _) = i

data NetworkRecv = NetworkRecv Integer [Recv ()]

netRecvId :: NetworkRecv -> Integer
netRecvId (NetworkRecv i _) = i

-- |The state of the network
data NetworkState = NetworkState
  { netSends :: [NetworkSend]     -- ^ Unrecieved messages
  , netRecvs :: [NetworkRecv]     -- ^ Processes awaiting messages
  , netIds :: Integer             -- ^ Seed for unique identifiers
  , netLog :: Seq String          -- ^ Network log
  , netRandomGen :: RNG           -- ^ RNG
  }

-- |Random number generator
data RNG where
  RNG :: Random.RandomGen g => g -> RNG

instance Random.RandomGen RNG where
  split (RNG f) =
    let (s1, s2) = Random.split f
    in (RNG s1, RNG s2)
  genWord64 (RNG f) = RNG <$> Random.genWord64 f


-- | A computation involving the network just stateful computation
--   on the 'NetworkState'
type Network = State.State NetworkState

-- |Generate the possible next states of the network
--  Each element is a new (unexecuted) process and network state
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

newId :: Network Integer
newId =
  do  pid <- State.gets netIds
      State.modify (\s -> s { netIds = pid + 1})
      pure pid

-- * Network API

-- |Run events in the network until no more remain
runScheduler :: Network ()
runScheduler =
  do  nextStates <- State.gets interleavings
      case nextStates of
        [] -> pure ()
        _ ->
          do  idx <- withRandom (Random.randomR (0, length nextStates - 1))
              let (prc, ns') = nextStates !! idx
              State.put ns'
              execute prc
              runScheduler

-- |Load a process into the network - the process will evolve until it requires input
-- in the form of `recv` or similar, but no further -- even if there are sends avaliable
-- that could allow it to make progress.  All such progress is deferred until a call
-- to 'runScheduler' to allow for processes to be interleaved.
execute :: Proc () -> Network ()
execute p =
  case p of
    Yield a -> pure a
    Fork f m ->
      do  execute f
          execute m

    Send port msg p ->
      do  sendId <- newId
          execute p
          State.modify (\s -> s { netSends = NetworkSend sendId port msg : netSends s})

    MkPort f ->
      do  ports <- mkPort
          execute (f ports)

    PRecv recs ->
      do  i <- newId
          let recs' = NetworkRecv i recs
          State.modify (\s -> s { netRecvs = recs' : netRecvs s})

    Log msg p ->
      do  State.modify (\s -> s { netLog = netLog s :|> msg  })
          execute p

-- | Create a pair of ports
mkPort :: Network (SendPort a, RecvPort a)
mkPort =
  do  pid <- newId
      pure (SendPort pid, RecvPort pid)

-- | Recieve all the messages currently on a port
recvAll :: Typeable a => RecvPort a -> Network [a]
recvAll (RecvPort rid) =
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

-- | An empty network
empty :: NetworkState
empty =
  NetworkState { netIds = 0
               , netRecvs = []
               , netSends = []
               , netLog = Seq.Empty
               , netRandomGen = RNG $ Random.mkStdGen 0
               }

-- | Set the RNG for the network
setRandomGen :: Random.RandomGen g => g -> Network ()
setRandomGen i = State.modify (\s -> s { netRandomGen = RNG i })

-- | Access the network's source of randomness
withRandom :: (RNG -> (a, RNG)) -> Network a
withRandom r =
  do  (a, g') <- State.gets (r . netRandomGen)
      State.modify (\s -> s { netRandomGen = g' })
      pure a

-- | Run a network computation
runNetwork :: Network a -> NetworkState -> (a, NetworkState)
runNetwork = State.runState

-- | Run a network computation on an initially empty network
runNetwork' :: Network a -> (a, NetworkState)
runNetwork' = (`runNetwork` empty)

-- | Execute and schedule a process immediately, returning a result if
--   the process terminates
evalProcess :: Typeable a => Proc a -> Network (Maybe a)
evalProcess p =
  do  (s, r) <- mkPort
      execute (p >>= send s)
      runScheduler
      results <- recvAll r
      case results of
        [a] -> pure $ Just a
        _ -> pure Nothing


-- | Run a process using Network, returning a result or an error
--   (most likely the error being that the process did not complete)
runProcess :: (Typeable a) => Proc a -> Random.StdGen -> ([String], Either String a)
runProcess p g =
  let (result, ns) = runNetwork' net
  in (F.toList (netLog ns), result)
  where
    p' sendP = p >>= send sendP
    net =
      do  setRandomGen g
          (sendP, recvP) <- mkPort
          execute (p' sendP)
          runScheduler
          recvs <- recvAll recvP
          case recvs of
            [] -> pure $ Left "Process did not complete"
            [r] -> pure $ Right r
            _ -> pure $ Left "[BUG] Process returned more than one result?"
