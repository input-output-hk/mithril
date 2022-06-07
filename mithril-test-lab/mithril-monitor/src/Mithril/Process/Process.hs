{-# LANGUAGE GADTs #-}
module Mithril.Process.Process where
import Data.Typeable (Typeable)
import Control.Monad (liftM, ap, (>=>))

-- * Overview

{- |
This module allows the modelling of asynchronous communicating processes
that can communicate via ports in a way that is abstract to any particular
realization on a network.

For some operations the 'Typeable' class is required to ease implementation
of networks that erase types - these constraints may be removed in the
future.
-}

-- * Process Types

-- |A port capable of sending values of type `a`
newtype SendPort a = SendPort Integer
  deriving (Eq, Ord)

-- |A port capable of recieving values of type 'a'
newtype RecvPort a = RecvPort Integer
  deriving (Eq, Ord)

data Recv a where
  Recv :: Typeable b => RecvPort b -> (b -> Proc a) -> Recv a

-- |An asynchronous networked process.
--
-- A process here is defined as a set of effects which are
-- entirely abstract to later be fulfilled through interpretation
-- (as in the "Mithril.Process.Network" module)
--
-- These constructors should not be used directly, instead processes should
-- be defined monadically using the API defined below.
data Proc a where
  -- |Await one of a set of possible recieve events
  PRecv :: [Recv a] -> Proc a

  -- |Create a new process
  Fork :: Proc () -> Proc a -> Proc a

  -- |Send a message to a port
  Send :: Typeable b => SendPort b -> b -> Proc a -> Proc a

  -- |Request the creation of a port
  MkPort :: ((SendPort b, RecvPort b) -> Proc a) -> Proc a

  -- |Halt and yield a value
  Yield :: a -> Proc a

  -- |Emit a log message
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

-- * Process API   #API#

-- |'Asynchronously' send a message to a port - execution may continue without waiting
--  for the sent message to be recieved.
send :: Typeable a => SendPort a -> a -> Proc ()
send port msg = Send port msg (pure ())

-- |Recieve a message from a port, suspending execution until the message arrives
recv :: Typeable a => RecvPort a -> Proc a
recv port = PRecv [Recv port pure]

-- |Create a new pair of ports
newPort :: Typeable a => Proc (SendPort a, RecvPort a)
newPort = MkPort pure

-- |Create a new process to be run in parallel with this one
fork :: Proc () -> Proc ()
fork f = Fork f (pure ())

-- |Defines an action to happen when a some port gets a message
--  - to be used with functions like `choose`
(===>) :: Typeable a => RecvPort a -> (a -> Proc b) -> Recv b
port ===> cont = Recv port cont
infixl 0 ===>

-- |Multi-way recv - allowing for execution to continue when any
--  of the possible next messages are recieved
--
-- Example:
--
-- @
-- chooseExample :: RecvPort Integer -> RecvPort String -> Proc String
-- chooseExample intPort strPort =
--   choose
--   [ intPort ===> pure . show
--   , strPort ===> pure
--   ]
-- @
choose :: [Recv a] -> Proc a
choose = PRecv

-- |Evaluate a process asynchronously, returning a handle that allows
-- waiting for the result - waiting multiple times on the same handle
-- will deadlock
async :: Typeable a => Proc a -> Proc (Proc a)
async p =
  do  (sendP, recvP) <- newPort
      fork $ p >>= send sendP
      pure $ recv recvP

-- |Output a string to the network's log
logmsg :: String -> Proc ()
logmsg msg = Log msg (pure ())

-- |Parallel version of `<*>`
(<|*|>) :: (Typeable a, Typeable b) => Proc (a -> b) -> Proc a -> Proc b
pf <|*|> pa =
  do  pf' <- async pf
      pa' <- async pa
      pf' <*> pa'

-- |Get the result of a process, if it is complete.
tryGetResult :: Proc a -> Maybe a
tryGetResult p =
  case p of
    Yield a -> Just a
    _ -> Nothing
