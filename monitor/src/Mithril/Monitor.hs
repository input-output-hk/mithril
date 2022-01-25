module Mithril.Monitor where

import Control.Monad((>=>), ap)
import Control.Applicative((<|>))
import qualified Data.Foldable as Foldable

data Monitor i o a =
    Result a
  | Yield o (Monitor i o a)
  | Continue (i -> Monitor i o a)

log :: o -> Monitor i o ()
log l = Yield l (pure ())

instance Functor (Monitor i o) where
  fmap f m =
    case m of
      Result a -> Result (f a)
      Yield o c -> Yield o (fmap f c)
      Continue c -> Continue (fmap f . c)

instance Applicative (Monitor i o) where
  (<*>) = ap
  pure a = Result a

instance Monad (Monitor i o) where
  ma >>= fmb =
    case ma of
      Result a  -> fmb a
      Yield o c -> Yield o (c >>= fmb)
      Continue c -> Continue (c >=> fmb)

--

observe :: Monitor i o a -> i -> Monitor i o a
observe mon msg =
  case mon of
    Result a -> Result a
    Yield o m -> Yield o (m `observe` msg)
    Continue c  -> c msg

-- TODO: make tail recursive
output :: Monitor i o a -> (Monitor i o a, [o])
output ma =
  case ma of
    Yield o ma' ->
      let (ma'', os) = output ma'
      in (ma'', o:os)
    Continue _ -> (ma, [])
    Result _ -> (ma, [])

step :: Monitor i o a -> i -> (Monitor i o a, [o])
step m msg = output (m `observe` msg)

next :: Monitor i o i
next = Continue (pure <$> id)

on :: (i -> Monitor i o a) -> Monitor i o a
on = Continue

onJust :: (i -> Maybe a) -> Monitor i o a
onJust f = Continue impl
  where
    impl msg =
      case f msg of
        Nothing -> onJust f
        Just a -> pure a



nextWhen :: (i -> Bool) -> Monitor i o i
nextWhen p = Continue impl
  where
    impl msg =
      if p msg then pure msg else nextWhen p

getResult :: Monitor i o a -> Maybe a
getResult (Result a) = Just a
getResult (Continue _) = Nothing
getResult (Yield _ m) = getResult m

-- TODO: doesn't output
allInParallel :: Traversable t => t (Monitor i o a) -> Monitor i o (t a)
allInParallel ms =
  case getResult `traverse` ms of
    Just as -> pure as
    Nothing -> Continue impl
  where
    impl msg =
      let ms' = (`observe` msg) <$> ms
      in  allInParallel ms'

allInParallel_ :: (Traversable f) => f (Monitor msg i a) -> Monitor msg i ()
allInParallel_ ms = allInParallel ms >> pure ()

-- | Run a Traversable of monitors in parallel, completing when the first completes
anyInParallel :: (Foldable f, Functor f) => f (Monitor i o a) -> Monitor i o a
anyInParallel ms =
  case complete ms of
    Just as -> Result as
    Nothing -> Continue impl
  where
    complete m = Foldable.foldl' step Nothing m

    step a b = a <|> getResult b

    impl msg =
      do  let ms' = (`observe` msg) <$> ms
          anyInParallel ms'

anyInParallel_ :: (Traversable f) => f (Monitor i o a) -> Monitor i o ()
anyInParallel_ ms = anyInParallel ms >> pure ()

-- run a monitor continuously
forever :: Monitor i o a -> Monitor i o b
forever mon = mon >> forever mon

--
inParallelEither :: Monitor i o a -> Monitor i o b -> Monitor i o (Either a b)
inParallelEither ma mb =
  case (ma, mb) of
    (Yield o ma', _) -> Yield o (inParallelEither ma' mb)
    (_, Yield o mb') -> Yield o (inParallelEither ma mb')
    (Result a, _) -> Result (Left a)
    (_, Result b) -> Result (Right b)
    (Continue c1, Continue c2) -> Continue (impl c1 c2)
  where
    impl c1 c2 msg = inParallelEither (c1 msg) (c2 msg)

-- mapInput :: (i -> b) -> Monitor b o a -> Monitor i o a
-- mapInput f m0 =
--   case m0 of
--     Result a -> Result a
--     Yield o m -> Yield o (mapInput f m)
--     Continue c -> Continue (impl c)
--   where
--     impl c msg = mapInput f <$> c (f msg)

filterInput :: (i -> Bool) -> Monitor i o a -> Monitor i o a
filterInput p m0 =
  case m0 of
    Result a -> Result a
    Yield o m -> Yield o (filterInput p m)
    Continue c -> Continue (impl c)
  where
    impl c msg =
      if p msg
        then filterInput p (c msg)
        else filterInput p m0