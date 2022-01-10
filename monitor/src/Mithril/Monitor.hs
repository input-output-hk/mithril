{-# LANGUAGE GADTs #-}
module Mithril.Monitor where

import qualified Control.Monad.Identity as Identity
import qualified Data.Foldable as Foldable
import qualified Control.Monad.Trans as Trans

data MonitorT msg m a where
  Result :: Monad m => a -> MonitorT msg m a
  Continue :: Monad m => (msg -> m (MonitorT msg m a)) -> MonitorT msg m a

type Monitor msg a = MonitorT msg Identity.Identity a

instance Monad m => Functor (MonitorT msg m) where
  fmap f ma =
    case ma of
      Result a -> Result (f a)
      Continue c ->
        Continue $ \msg ->
          do  ma' <- c msg
              pure $ fmap f ma'

instance Monad m => Applicative (MonitorT msg m) where
  mf <*> ma = mf >>= \f -> f <$> ma
  pure a = Result a

instance Monad m => Monad (MonitorT msg m) where
  ma >>= fmb =
    case ma of
      Result a -> fmb a
      Continue c -> Continue (cont c)
    where
      cont c msg =
        do  ma' <- c msg
            pure (ma' >>= fmb)

-- instance Trans.MonadTrans (MonitorT msg) where
--   -- lift :: Monad m => m a -> MonitorT msg m a
--   lift m =
--     fmap Result
-------------------------------------------------------------------------------
-- API

observe' :: Monitor msg a -> msg -> Monitor msg a
observe' mon msg =
  case mon of
    Result a -> Result a
    Continue c -> Identity.runIdentity (c msg)

observe :: Monad m => MonitorT msg m a -> msg -> m (MonitorT msg m a)
observe mt msg =
  case mt of
    Result a -> pure $ Result a
    Continue c -> c msg

getResult :: MonitorT msg m a -> Maybe a
getResult (Continue _) = Nothing
getResult (Result a) = Just a

hasResult :: MonitorT msg m a -> Bool
hasResult m =
  case m of
    Result _ -> True
    Continue _ -> False

-- | Gets the next message that satisfies a predicate
next :: Monad m => (msg -> Bool) -> MonitorT msg m msg
next p = Continue impl
  where
    impl msg =
      if p msg then pure (Result msg) else pure (Continue impl)

-- | Completes when the function argument returns Just
onJust :: Monad m => (msg -> Maybe a) -> MonitorT msg m a
onJust f = Continue impl
  where
    impl msg =
      case f msg of
        Just a -> pure $ Result a
        Nothing -> pure $ Continue impl

-- | Completes when the function argument returns a non empty list
onElems :: Monad m => (msg -> [a]) -> MonitorT msg m [a]
onElems f = Continue impl
  where
    impl msg =
      case f msg of
        [] -> pure $ Continue impl
        r -> pure $ Result r

mapInput :: Monad m => (msg -> b) -> MonitorT b m c -> MonitorT msg m c
mapInput f m0 =
  case m0 of
    Result c -> Result c
    Continue c -> Continue (impl c)
  where
    impl c msg = mapInput f <$> c (f msg)

filterInput :: Monad m => (msg -> Bool) -> MonitorT msg m a -> MonitorT msg m a
filterInput p m0 =
  case m0 of
    Result a -> Result a
    Continue c -> Continue (impl c)
  where
    impl c msg =
      if p msg
        then filterInput p <$> c msg
        else pure $ Continue c

-- | Run a container of monitors in parallel, completing when all complete
--   and producing results in the same structure
--   Note that this isn't `traverse` since that is sequential
parallel :: (Traversable f, Monad m) => f (MonitorT msg m a) -> MonitorT msg m (f a)
parallel ms =
    case complete ms of
        Just as -> Result as
        Nothing -> Continue impl
  where
    complete m = getResult `traverse` m
    impl msg =
      do  ms' <- (`observe` msg) `traverse` ms
          pure $ parallel ms'


parallel_ :: (Traversable f, Monad m) => f (MonitorT msg m a) -> MonitorT msg m ()
parallel_ ms = parallel ms >> pure ()

-- | Run a Traversable of monitors in parallel, completing when the first completes
parallelAny :: (Traversable f, Monad m) => f (MonitorT msg m a) -> MonitorT msg m a
parallelAny ms =
  case complete ms of
    Just as -> Result as
    Nothing -> Continue impl
  where
    complete m = Foldable.foldl' step Nothing m

    step (Just a) _ = Just a
    step Nothing (Result a) = Just a
    step Nothing _ = Nothing

    impl msg =
      do  ms' <- (`observe` msg) `traverse` ms
          pure $ parallelAny ms'

parallelAny_ :: (Traversable f, Monad m) => f (MonitorT msg m a) -> MonitorT msg m ()
parallelAny_ ms = parallelAny ms >> pure ()