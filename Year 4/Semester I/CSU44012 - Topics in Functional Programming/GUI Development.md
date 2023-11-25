The Threepenny GUI is a library made for creating user interfaces, which is implemented as a Functional Reactive Programming Domain-Specific Language (FRP DSL). It creates a browser-based UI. Unlike Scotty, it is meant to be used with a tight interaction loop.

\[rest of [[9.01 - Reactive programming.pdf|9.01]]]
## The FRP Approach
[[9.02 - Reactive programming 2.pdf]]
The FRP style emphasises composing functions; specifically, *signal functions* that carry information about events and behaviours. The point is to get away from mutation, callbacks, etc.

**Important concepts:**
- Events: things that happen at certain moments (e.g., mouse click, key press, custom events, etc.)
- Behaviours: continuously occurring things (e.g., the content of an input field, position of mouse cursor, etc.)

### Behaviour
Behaviours are like signal functions. One may have the type:
```haskell
type Behaviour a = Time -> a
```
which is a type of Behaviour from Time to a value.

![[Pasted image 20231108120847.png]]

We'd like to be able to:
- apply functions to one or more Behaviours
- create constant behaviours
So Behaviour should be an instance of Functor and Applicative. 

We can use Functor and Applicative operations to modify behaviours. For example, if there exists a Behaviour that returns your balance:
```haskell
balance :: Behaviour Int
```
and another that tells you the price of something:
```haskell
price :: Behaviour Int
```
when you can produce a behaviour that tells you whether you can afford to buy something at any given time:
```haskell
affordable :: Behaviour Bool
affordable = (>=) <$> balance <*> price
```

### Events
Events are things that can be triggered at certain times. At any given moment, there can be at most one event taking place.

For example, we can have an event that tells the time:
```haskell
type Event a = [(Time, a)]
```

![[Pasted image 20231108122401.png]]

The only event from Threepenny is the one that never fires; all others come from the UI library.
```haskell
never :: Event a
never = []
```

There's a set of functions to combine events. First, there's the Functor instance:
```haskell
fmap :: (a -> b) -> Event a -> Event b
```
Event streams can be *joined*:
```haskell
unionWith :: (a -> a -> a) -> Event a -> Event a -> Event a
```
Only one event is permitted as a time; the first argument has the job of combining the latter two events in that case.
#### Utilities
Sometimes, we'll want to filter event streams to ignore some events.
```haskell
filterE :: (a -> Bool) -> Event a -> Event a
```
A common case is merging many event streams that contain functions:
```haskell
unions :: [Event (a -> a)] -> Event (a -> a)
```

#### Accumulation
Accumulation accumulates events (obviously).
```haskell
accumE :: MonadIO m => a -> Event (a -> a) -> m (Event a)
```
For example, say we have an input event that produces numbers:
```haskell
newItemsArrived :: Event Int -- gives number of new items
```
We can transform this into an event that fires when new items arrive, but which gives the *total* accumulated number of items instead:
```haskell
totalItems :: UI (Event Int) -- gives number of total items
totalItems = accumE 0 ((+) <$> newItemsArrived)
```

### Combining Events and Behaviours
One combination is to take a Behaviour that has a function which varies over time and apply it, similar to an extension of `fmap`.
```haskell
apply :: Behaviour (a -> b) -> Event a -> Event b
(<@>) = apply
```

Here's a related idea: make an Event that fires when the supplied event does, but that takes its value from the behaviour:
```haskell
(<@) :: Behaviour b -> Event a -> Event b
```

Behaviours can be involved in filtering events with these functions:
```haskell
filterApply :: Behaviour (a -> Bool) -> Event a -> Event a
whenE :: Behaviour Bool -> Event a -> Event a
```

And most usefully, a way to treat Events as Behaviours:
```haskell
stepper :: monadIO m => a -> Event a -> m (Behaviour a)
stepper x0 ex = return $ \time ->
		last (x0 : [x | (timex, x) <- ex, timex < time])
```
![[Pasted image 20231108123458.png]]

`stepper` is often combined with `accumE`. For convenience, this exists as:
```haskell
accumB :: MonadIO m => a -> Event (a -> a) -> m (Behaviour a)
```


