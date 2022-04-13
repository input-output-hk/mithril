{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
module Mithril.Monitor.Monitor where
import Prelude hiding(either, all, any)

import Data.Word(Word, Word64)
import Control.Applicative ((<|>), Alternative (empty))
import Control.Monad (ap, unless, join)
import Data.Functor ((<&>))
import Data.Void (Void)
import qualified Data.Maybe as Maybe
import Data.Foldable (Foldable(foldl'))

type Duration = Word64

data Input i =
    ClockDelta Duration
  | Input i

data Output o = Output
  { outTimeout :: Maybe Duration
  , outValues  :: [o]
  }
  deriving Show

combineOutput :: (Duration -> Duration -> Duration) -> Output o -> Output o -> Output o
combineOutput f a b =
    Output { outTimeout = timeout'
           , outValues = concat (outValues <$> [a, b])
           }
    where
      timeout' =
        let t1 = outTimeout a
            t2 = outTimeout b
        in (f <$> t1 <*> t2) <|> t1 <|> t2

emptyOutput :: Output o
emptyOutput = Output { outTimeout = Nothing
                     , outValues = []
                     }

instance Monoid (Output o) where
  mempty = emptyOutput

instance Semigroup (Output o) where
  (<>) = combineOutput const

type MonitorStep i o a = Input i -> Monitor i o a

data Monitor i o a =
    Active (MonitorStep i o a)
  | Done a
  | Fail String
  | Out (Output o) (Monitor i o a)

instance Functor (Monitor i o) where
  fmap f ma =
    case ma of
        Active step -> Active $ \i -> fmap f (step i)
        Done a -> Done (f a)
        Fail s -> Fail s
        Out o ma' -> Out o (fmap f ma')

instance Applicative (Monitor i o) where
  pure a = Done a
  f <*> m = ap f m

instance Monad (Monitor i o) where
  ma >>= fmb =
    case ma of
      Fail s -> Fail s
      Done a -> fmb a
      Out o ma' -> Out o (ma' >>= fmb)
      Active step ->
        let step' i =
              let ma' = step i
              in  ma' >>= fmb
        in Active step'

instance MonadFail (Monitor i o) where
  fail =  Fail

instance Alternative (Monitor i o) where
  empty = fail "empty monitor"
  a <|> b = case a of
    Active f ->
      do  i <- next
          a' <- a `observeInner` i
          case a' of
            Fail s -> join (b `observeInner` i)
            _ -> a'

    Done a' -> Done a'
    Fail s -> b
    Out out mon -> Out out (mon <|> b)



-- fundamental operations -----------------------------------------------------

getResult :: Monitor i o a -> Maybe (Either String a)
getResult m =
  case m of
    Out _ m' -> getResult m
    Done a -> Just (Right a)
    Active _ -> Nothing
    Fail s -> Just (Left s)

getResults :: Traversable t => t (Monitor i o a) -> Maybe (Either String (t a))
getResults ms = fmap sequence (traverse getResult ms)

isComplete :: Monitor i o a -> Bool
isComplete m = Maybe.isJust (getResult m)

output :: o -> Monitor i o ()
output o = Out (Output Nothing [o]) (Done ())

output' :: Output o -> Monitor i o ()
output' o = Out o (Done ())

getOutput :: Monitor i o a -> (Output o, Monitor i o a)
getOutput m =
  case m of
    Out o m' ->
      let (o', m'') = getOutput m'
      in  (o <> o', m'')
    Fail _ -> (emptyOutput, m)
    Done _ -> (emptyOutput, m)
    Active _ -> (emptyOutput, m)


observe :: Monitor i o a -> i -> (Output o, Monitor i o a)
observe m i = m `observeInput` Input i

observeInner :: Monitor i o a -> i -> Monitor i o (Monitor i o a)
observeInner m i =
  do  let (o, m') = m `observe` i
      output' o
      pure m'

observeMany :: Foldable f => Monitor i o a -> f i -> (Output o, Monitor i o a)
observeMany m0 = foldl' step (emptyOutput, m0)
  where
    step (o, m) i =
      let (o', m') = m `observe` i
      in (o <> o', m')


observeInput :: Monitor i o a -> Input i -> (Output o, Monitor i o a)
observeInput m i =
  case m of
    Out o a' ->
      let (o', a'') = a' `observeInput` i
      in (o <> o', a'')
    Fail _ -> (emptyOutput, m)
    Done a -> (emptyOutput, m)
    Active step -> getOutput (step i)

next :: Monitor i o i
next = Active $ \case
  ClockDelta _ -> next
  Input i -> pure i

nextInput :: Monitor i o (Input i)
nextInput = Active pure

onFail :: Monitor i o a -> Monitor i o a -> Monitor i o a
onFail ma mb =
  case ma of
    Out o ma' -> Out o (onFail ma' mb)
    Fail _ -> mb
    Done a -> pure a
    Active step -> Active $ \i -> step i `onFail` mb

modStep :: Monitor i o a -> (MonitorStep i o a -> MonitorStep i o a) -> Monitor i o a
modStep ma f =
  case ma of
    Out o ma' -> Out o (modStep ma' f)
    Fail _ -> ma
    Done a -> Done a
    Active step -> Active (f step)


-- ignore all messages for a particular duration
wait :: Duration -> Monitor i o ()
wait t =
  do  output' emptyOutput { outTimeout = Just t }
      d <- onInput delta
      if t <= d
        then pure ()
        else wait (t - d)
  where
    delta =
      do  ClockDelta d <- nextInput
          pure d

onTimeout :: Duration -> Monitor i o a -> Monitor i o a -> Monitor i o a
onTimeout d m1 m2 =
  do  r <- either (wait d) m1
      case r of
        Left _ -> m2
        Right a -> pure a


withTimeout :: Duration -> Monitor i o a -> Monitor i o a
withTimeout d m = onTimeout d m (fail "timeout exceeded")

-- withTimeout :: Duration -> Monitor i o a -> Monitor i o a
-- withTimeout t ma =
--   case ma of
--     Done a -> Done a
--     Fail s -> Fail s
--     Out out mon -> Out out (withTimeout t mon)
--     Active _ ->
--       do  output' (emptyOutput { outTimeout = Just t })
--           i <- nextInput
--           case i of
--             ClockDelta d | d > t -> fail "timeout exceeded"
--                          | otherwise -> withTimeout (t - d) ma
--             Input i' ->
--               let (o', ma') = ma `observe` i'
--               in  output' o' >> withTimeout t ma'




on :: Monitor i o a -> Monitor i o a
on m = m `onFail` on m

onInput :: Monitor i o a -> Monitor i o a
onInput m = m `onFail` onInput m

any :: Foldable t => t (Monitor i o a) -> Monitor i o a
any = foldr race (fail "any failed")

watch :: [Monitor i o a] -> Monitor i o ([a], [Monitor i o a])
watch ms =
  case (results, ms') of
    -- stop if no monitors remain
    (a, []) -> pure (a, [])

    -- continue if no results have been produced
    ([], _) ->
      do  i <- next
          ms'' <- (`observeInner` i) `traverse` ms'
          watch ms''

    -- stop if any results have been produced
    (a, _) -> pure (a, ms')

  where
    results = concat resultss
    ms' = concat mss'
    (resultss, mss') = unzip (obs <$> ms)
    obs :: Monitor i o a -> ([a], [Monitor i o a])
    obs mon =
      case mon of
        Active f -> ([], [mon])
        Done a -> ([a], [])
        Fail s -> ([], [])
        Out o m' ->
          let (rs, ms) = obs m'
          in (rs, Out o <$> ms)




all :: (Traversable t) => t (Monitor i o a) -> Monitor i o (t a)
all ms =
  case getResults ms of
    Just (Right as) -> pure as
    Just (Left s) -> fail s
    Nothing ->
      do  i <- nextInput

          let obs = (`observeInput` i) <$> ms
              os = fst <$> obs
              ms' = snd <$> obs

          output' (foldr (combineOutput min) emptyOutput os)
          all ms'

all_ :: (Traversable t) => t (Monitor i o a) -> Monitor i o ()
all_ ms = all ms >> pure ()


race :: Monitor i o a -> Monitor i o a -> Monitor i o a
race a b = unEither <$> either a b
  where
    unEither (Left a) = a
    unEither (Right a) = a


either :: Monitor i o a -> Monitor i o b -> Monitor i o (Either a b)
either a b =
  case (a,b) of
    (Out o a', _) -> Out o (either a' b)
    (_, Out o b') -> Out o (either a b')
    (Fail _, _) -> Right <$> b
    (_, Fail _) -> Left <$> a
    (Done ra, _) -> pure (Left ra)
    (_, Done rb) -> pure (Right rb)
    (Active _, Active _) ->
      Active $ \i ->
        let (o1, a') = a `observeInput` i
            (o2, b') = b `observeInput` i
        in Out (combineOutput max o1 o2) (either a' b')

both :: Monitor i o a -> Monitor i o b -> Monitor i o (a,b)
both a b =
  case (a, b) of
    (Out o a', _) -> Out o (both a' b)
    (_, Out o b') -> Out o (both a b')
    (Fail s, _) -> Fail s
    (_, Fail s) -> Fail s
    (Done ra, _) -> (ra,) <$> b
    (_, Done rb) -> (,rb) <$> a
    (Active _, Active _) ->
      Active $ \i ->
        let (o1, a') = a `observeInput` i
            (o2, b') = b `observeInput` i
        in Out (combineOutput min o1 o2) (both a' b')

-- execute a and b in parellel, returning mb - but if ma fails, the overall result fails
guard :: Monitor i o a -> Monitor i o b -> Monitor i o b
guard a b =
  case (a, b) of
    (Out o a', _) -> Out o (guard a' b)
    (_, Out o b') -> Out o (guard a b')
    (Fail s, _) -> Fail s
    (_, Fail s) -> Fail s
    (Done ra, _) -> b
    (_, Done rb) -> Done rb
    (Active _, Active _) ->
      Active $ \i ->
        let (o1, a') = a `observeInput` i
            (o2, b') = b `observeInput` i
        in Out (combineOutput min o1 o2) (guard a' b')

try :: Monitor i o a -> Monitor i o (Maybe a)
try m = (Just <$> m) `onFail` pure Nothing


everywhere :: Monitor i o () -> Monitor i o Void
everywhere m =
  snd <$> both (m `onFail` pure ()) (next >> everywhere m)

always :: Monitor i o a -> Monitor i o Void
always m = fmap snd (both m (next >> always m))

never :: Monitor i o a -> Monitor i o Void
never m = any [m >> fail "never violated", next >> never m]

eventually :: Monitor i o a -> Monitor i o a
eventually m = any [m, next >> eventually m]

require :: Bool -> Monitor i o ()
require b = unless b (Fail "require failed")

accept :: (i -> Bool) -> Monitor i o i
accept f =
  do  m <- next
      require (f m)
      pure m

-- parallel version of <|*|>
infixl 4 <|*|>
(<|*|>) :: Monitor i o (t -> b) -> Monitor i o t -> Monitor i o b
mf <|*|> ma =
  do  (f, a) <- both mf ma
      pure (f a)

