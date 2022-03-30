module Mithril.Free where

import Prelude hiding(read)
import qualified Control.Monad.Free as Free
import qualified Control.Monad.Trans.Free as FreeT
import qualified Data.Void(Void)

-- A handler is a function that given a message produces a result
newtype Handler m r = Handler { runHandler :: m -> r }

-- we can implement a fuctor that transforms the result
instance Functor (Handler m) where
  fmap f ha =
    Handler $ \m -> f (runHandler ha m)

-- A monitor is the Free monad on a Handler
type MonitorT msg m r = FreeT.FreeT (Handler msg) m r

observe :: Monad m => MonitorT msg m r -> msg -> m (MonitorT msg m r)
observe mon msg = run <$> FreeT.runFreeT mon
  where
    run m =
      case m of
        FreeT.Pure r -> pure r
        FreeT.Free handler -> runHandler handler msg


getResult :: Monad m => MonitorT msg m a -> m (Maybe a)
getResult mon = run <$> FreeT.runFreeT mon
  where
    run m =
      case m of
        FreeT.Pure r -> Just r
        FreeT.Free _ -> Nothing

on :: Applicative m => (msg -> MonitorT msg m a) -> MonitorT msg m a
on f =
  FreeT.FreeT $ pure $ FreeT.Free (Handler f)

next :: Monad m => MonitorT msg m msg
next = on pure

skip :: Monad m => MonitorT msg m ()
skip = next >> pure ()



