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

-- | A Monitor is a simple machine that can look at a stream of inputs and produce
--   some set of outputs before resulting in a value summarizing the
--   stream or a failure.  Each constructor represents a state of the monitor.
data Monitor i o a =
    -- |The monitor is awaiting an event
    Active (MonitorStep i o a)
    -- |The monitor is complete with a resulting value
  | Done a
    -- |The monitor has failed
  | Fail String
    -- |The montior is outputting a value before continuing
  | Out (Output o) (Monitor i o a)

-- |A type alias describing a monitor's value in the Active state.
type MonitorStep i o a = Input i -> Monitor i o a

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
          b' <- b `observeInner` i
          a' <|> b'

    Done a' -> Done a'
    Fail s -> b
    Out out mon -> Out out (mon <|> b)

-- |An input for the monitor
data Input i =
    -- |Notification that a certian amount of time has passed (aka the clock signal)
    ClockDelta Duration
    -- |An event of interest to the monitor
  | Input i

-- |The output of a monitor
data Output o = Output
  { outTimeout :: Maybe Duration -- ^The next "interesting" timeout, if any
  , outValues  :: [o]            -- ^A list of values the monitor has output
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

-- |Human readable description of the monitor's state, useful for debugging.
describe :: (Show a, Show o) => Monitor i o a -> String
describe m =
  case m of
    Active _ -> "Active"
    Done a -> "Done " ++ show a
    Out o m' -> "Output " ++ show o ++ " -- " ++ describe m'
    Fail s -> "Fail " ++ show s

-- * Querying Monitors

-- |Get the result yielded by the monitor:
--
--    * If the monitor is complete with @value@ - returns @Just (Right value)@
--    * If the monitor has failed - returns @Just (Left err)@
--    * If the monitor is still active, returns @Nothing@
getResult :: Monitor i o a -> Maybe (Either String a)
getResult m =
  case m of
    Out _ m' -> getResult m
    Done a -> Just (Right a)
    Active _ -> Nothing
    Fail s -> Just (Left s)

-- | Convenience function to get results from many monitors.
getResults :: Traversable t => t (Monitor i o a) -> Maybe (Either String (t a))
getResults ms = fmap sequence (traverse getResult ms)

-- |Returns true if the monitor is done or has failed.
isComplete :: Monitor i o a -> Bool
isComplete m = Maybe.isJust (getResult m)

-- |Returns true if the monitor has failed
isFailed :: Monitor i o a -> Bool
isFailed m =
  case getResult m of
    Just (Left _) -> True
    _ -> False


-- |Gets any pending output on the monitor, returning the output and the
--  monitor with any output removed
getOutput :: Monitor i o a -> (Output o, Monitor i o a)
getOutput m =
  case m of
    Out o m' ->
      let (o', m'') = getOutput m'
      in  (o <> o', m'')
    Fail _ -> (emptyOutput, m)
    Done _ -> (emptyOutput, m)
    Active _ -> (emptyOutput, m)


-- * Observing (aka running)

-- | Observe an input with a monitor, returning any output as well as the next
--   state of the monitor
observe :: Monitor i o a -> i -> (Output o, Monitor i o a)
observe m i = m `observeInput` Input i

-- | Observe an input of a nested monitor, redirecting any output through the
--  "hosting" monitor and returning the next state of the monitor.
observeInner :: Monitor i o a -> i -> Monitor i o (Monitor i o a)
observeInner m i =
  do  let (o, m') = m `observe` i
      output' o
      pure m'

-- | Convenience function for observing many messages and collecting the output
observeMany :: Foldable f => Monitor i o a -> f i -> (Output o, Monitor i o a)
observeMany m0 = foldl' step (emptyOutput, m0)
  where
    step (o, m) i =
      let (o', m') = m `observe` i
      in (o <> o', m')

-- | General form of 'observe' allowing the ability to send clock signals
--   as well as input.
observeInput :: Monitor i o a -> Input i -> (Output o, Monitor i o a)
observeInput m i =
  case m of
    Out o a' ->
      let (o', a'') = a' `observeInput` i
      in (o <> o', a'')
    Fail _ -> (emptyOutput, m)
    Done a -> (emptyOutput, m)
    Active step -> getOutput (step i)

-- * Monitor API

-- ** Basic operations

-- | Produce an output object from a monitor
output :: o -> Monitor i o ()
output o = Out (Output Nothing [o]) (Done ())

output' :: Output o -> Monitor i o ()
output' o = Out o (Done ())

-- | Await the next input, ignoring any clock signals.
next :: Monitor i o i
next = Active $ \case
  ClockDelta _ -> next
  Input i -> pure i

-- | Await the next input
nextInput :: Monitor i o (Input i)
nextInput = Active pure

-- | Run @ma `onFail` mb@ runs @ma@ and if @ma@ fails, runs @mb@.
onFail :: Monitor i o a -> Monitor i o a -> Monitor i o a
onFail ma mb =
  case ma of
    Out o ma' -> Out o (onFail ma' mb)
    Fail _ -> mb
    Done a -> pure a
    Active step -> Active $ \i -> step i `onFail` mb

-- |@try ma@ runs @ma@ and yields the result wrapped in @Just@,
--  unless @ma@ fails - in which case the overall result is @Nothing@
try :: Monitor i o a -> Monitor i o (Maybe a)
try m = (Just <$> m) `onFail` pure Nothing

modStep :: Monitor i o a -> (MonitorStep i o a -> MonitorStep i o a) -> Monitor i o a
modStep ma f =
  case ma of
    Out o ma' -> Out o (modStep ma' f)
    Fail _ -> ma
    Done a -> Done a
    Active step -> Active (f step)


-- |Ignore all messages until a timeout happens
wait :: Duration -> Monitor i o ()
wait t =
  do  output' emptyOutput { outTimeout = Just t }
      d <- on delta
      if t <= d
        then pure ()
        else wait (t - d)
  where
    delta =
      do  ClockDelta d <- nextInput
          pure d

-- |@onTimeout d ma mb@ runs @ma@ but if at any point
--  duration @d@ elapses, behaves as @mb@
onTimeout :: Duration -> Monitor i o a -> Monitor i o a -> Monitor i o a
onTimeout d m1 m2 =
  do  r <- either (wait d) m1
      case r of
        Left _ -> m2
        Right a -> pure a

-- |Run a monitor with a timeout, failing if the timeout is exceeded
withTimeout :: Duration -> Monitor i o a -> Monitor i o a
withTimeout d m = onTimeout d m (fail "timeout exceeded")

-- |Run a monitor repeatedly, retrying on failure until it succeeds
on :: Monitor i o a -> Monitor i o a
on m = m `onFail` on m

-- |Require a boolean condition to be true or fail.
require :: Bool -> Monitor i o ()
require b = unless b (Fail "require failed")

-- |Same as 'next' but with a condition which causes failure if it is not met.
accept :: (i -> Bool) -> Monitor i o i
accept f =
  do  m <- next
      require (f m)
      pure m

-- ** Parallel Operations

-- |Run a set of monitors returning the result of the first one to succeed.
--  Failures are ignored unless the entire set of monitors fails.
--  If multiple succeed simultaneously then the result is the "leftmost" one
--  with respect to the 'Foldable' instance.
any :: Foldable t => t (Monitor i o a) -> Monitor i o a
any = foldr race (fail "any failed")

-- |Run a set of monitors until they all complete.
--  Any failure results in the failure of the overall computation.
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

-- |Like 'all' but ignoring results.
all_ :: (Traversable t) => t (Monitor i o a) -> Monitor i o ()
all_ ms = all ms >> pure ()

-- |Run a set of monitors until some subset completes, yielding
--  both the completed elements and the remaining monitors.
watch :: [Monitor i o a] -> Monitor i o ([a], [Monitor i o a])
watch ms =
  do  (as, n) <- ms `collectUntil` next
      ms' <- (`observeInner` n) `traverse` ms
      pure (as, [m | m <- ms', not (isComplete m)])

-- | @either ma mb@ runs @ma@ and @mb@ in parallel, with the result
--   being the result of the first monitor to complete or the result of @ma@
--   if they complete simultaneously.  Only fails if both @ma@ and @mb@ fail.
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

-- | Like @either ma mb@ but simpler for the case when @ma@ and @mb@
--   have the same type and it's unnecessary to distinguish which
--   completed, only the resulting value
race :: Monitor i o a -> Monitor i o a -> Monitor i o a
race a b = unEither <$> either a b
  where
    unEither (Left a) = a
    unEither (Right a) = a


-- | @both ma mb@ runs @ma@ and @mb@ in parallel, with the result
--   being a pair of both completed results.  Fails if either monitor fails.
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

-- |@guard ma mb@ executes @ma@ and @mb@ in parallel,
--  returning the result of @mb@ -- but if @ma@ fails, the overall result fails
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

-- |@everywhere ma@ runs @ma@ continuously on every subsequence of messages,
-- never returning (even on failure.)
everywhere :: Monitor i o () -> Monitor i o Void
everywhere m =
  snd <$> both (m `onFail` pure ()) (next >> everywhere m)

-- |@always ma@ runs @ma@ on every subsequence of messages until any failure happens
-- at which point @always ma@ fails.
always :: Monitor i o a -> Monitor i o Void
always m = fmap snd (both m (next >> always m))

-- **Global monitors

-- |@never ma@ runs @ma@ on every subsequence of messages until it completes successfully
-- at which point the call to @never@ fails.
never :: Monitor i o a -> Monitor i o Void
never m = snd <$> both doM (next >> never m)
  where
    doM =
      try m >>= \case
        Nothing -> pure ()
        Just _ -> fail "never violated"

-- |@eventually ma@ runs @ma@ on every subsequence of messages until any of them complete
--  yielding the result of the first completed instantiation of @ma@
eventually :: Monitor i o a -> Monitor i o a
eventually m = any [m, next >> eventually m]

-- | @collectUntil mas mb@ collects the results from @mas@ until @mb@ happens
--   yielding the list of results from the completed @mas@ and the value
--   yielded by @mb@
collectUntil :: [Monitor i o a] -> Monitor i o b -> Monitor i o ([a], b)
collectUntil mas mb = both (Maybe.catMaybes <$> all (mk <$> mas)) mb
  where
    mk m = race (mb >> pure Nothing) (Just <$> m) `onFail` pure Nothing

-- | Parallel version of '(<*>)' - monitors both arguments until they are complete
--   and then applies the first to the second.  Useful for building up structures
--   that can arrive in any order but require all pieces to show up a specific
--   number of times.
infixl 4 <|*|>
(<|*|>) :: Monitor i o (t -> b) -> Monitor i o t -> Monitor i o b
mf <|*|> ma =
  do  (f, a) <- both mf ma
      pure (f a)

-- ** Tracing/Lookahead

-- | Trace a monitor - on completion yielding both the result and the list of
--   accepted messages.
trace :: Monitor i o a -> Monitor i o (a, [i])
trace m0 = go m0 []
  where
    go m acc =
      case m of
        Done a -> pure (a, acc)
        Fail f -> Fail f
        Out o m' ->
          do  output' o
              go m' acc
        Active _ ->
          do  i <- next
              m' <- m `observeInner` i
              go m' (acc ++ [i])

-- | @lookahead ma mb@ uses @ma@ to observe messages until it completes, at which point
--   @mb@ observes those messages and continues executing if not complete.
lookahead :: Monitor i o a -> Monitor i o b -> Monitor i o b
lookahead la m =
  do  (_, is) <- trace la
      let (o, m') = m `observeMany` is
      output' o
      m'
