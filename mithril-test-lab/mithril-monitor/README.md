# Understanding `Monitor` (`Mithril.Monitor.Monitor`)

Suppose we are trying to determine if a safety property holds for a sequence of messages of type `m`.

One thing we could imagine doing is defining such a property in three cases:

1. The property holds
2. The property does not hold
3. Another `m` value is needed to make a further determination about whether the property holds.

In Haskell we might define the type of such properties as:

```haskell
data Property m =
    Holds
  | DoesNotHold
  | NeedAnotherMessage (m -> Property m)
```

We can generalize this idea to model any computation that derives a value from seeing some sequence
of messages:

```haskell
data StreamProcessor m a =
    Result a
  | Await (m -> StreamProcessor m a)
```

Notice that `Property m` becomes the specific case of `StreamProcessor m Bool`.

`StreamProcessor` also has an interpretation as a `Monad` where the stream processors 
are "sequenced" - messages are recieved by the first argument to `>>=` until it yields
a result which is used to to instantiate a second `StreamProcessor` which recieves
the subsequent messages:

```haskell
instance Monad (StreamProcessor m) where
  sa >>= fsb =
    case sa of
      Result a -> fsb a
      Await c -> Await (\m -> c m >>= fsb)
```

Essentially `Monitor` is a specialized `StreamProcessor` with the addition of failure and 
the ability to produce output values during computation instead of at the end.  Further, the 
notion of a virtual clock has been added to the input to allow for the modelling 
of timeouts and to allow for the testing of liveness properties:

```haskell
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
```


# Understanding `Process` (`Mithril.Process.Process`)

`Monitor` is flexible and it is tempting to think it can also be used to model
_communicating processes_ with a particular instantiation of `Monitor m m Void` -
which is to say that it is a monitor that both accepts and produces `m` values to
be externally routed by some network simulation.

In practice this is cumbersome and limiting.  For one thing, the specification of
the type `m` is often too broad and needs to include a lot of annoying routing information.

For another, the combinators that combine monitors generally do so in a way that
the component sub-monitors will witness the same stream of messages.  This can be very
cumbersom when we want to implement something like the creation of a new totally
independent process.

Rather than processing streams, then - we want an abstraction that allows us
to interact with ports - possibly of different types.  Note that this
requires the use of GADTs since the types of the ports are "hidden" with
respect to the overall type.

A simple example might be:

```haskell
data PortProcessor a where
  Yield :: a -> PortProcessor a
  Send :: SendPort b -> b -> PortProcessor a -> PortProcessor a
  Recv :: RecvPort b -> (b -> PortProcessor a) -> PortProcessor a
```

Note the similarity between `Send` and `Out` on `Monitor` and likewise
with `Recv` and `Active`/`Await`.  This is very similar to `Monitor` in many ways!
The actual definiton of `SendPort` and `RecvPort` isn't terribly important.

`PortProcessor` is a nice specification for communicating processes because
it specifies the communication machinery without specifying anything about
_how_ it happens - which can allow for a variety of interpretations by
a network simulation -- but there are some useful things missing.  Specifically,
how do we create processes and ports?  If we close over the set of constructors
to complete this abstraction, we arrive at something like the `Proc` type
as written in the `Mithril.Process.Process` module:

```haskell
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

data Recv a where
  Recv :: Typeable b => RecvPort b -> (b -> Proc a) -> Recv a
```

`Fork` and `MkPort` have been added to allow for the creation of
processes and port pairs.  `Recv` is a little more complex since it's been
extended to handle waiting for messages on multiple ports simultaneously.
And `Log` has been added for convenience, though it's not really necessary
for the overall functioning of the system.
