Starting in GHC 7.10, some extra abstractions were introduced. As a result, Monads are no longer defined by a single class.

Monads were refactored into 3 pieces:
- Functor
- Applicative
- Monad

This has completely changed how Monads work, as there were duplicate functions and some operations that should only require types to be instances of Applicative were requiring Monad.

## Functor
A Functor is something that can apply functions to values contained in the functor.

```haskell
class Functor f where
	fmap :: (a -> b) -> (f a -> f b)
```

Here is an instance of `fmap` for Maybe:
```haskell
 instance Functor Maybe where 
	 fmap f Nothing = Nothing 
	 fmap f (Just a) = Just (f a)
```

The Data.Functor library contains the synonym for `fmap`:
```haskell
(<$>) = fmap
```

There are also the following functions:
```haskell
(<$) :: Functor f => a => f b => f a 
($>) :: Functor f => f a => b => f b 
void :: Functor f => f a => f ()
```

`(<$)` is actually a member of the Functor class, but it has a default definition.

### Functor Laws
Functor instances are expected to obey two equational laws. The compiler can't enforce these, but users of the Functor expect them to be true.

Identity:
`fmap id = id`

Composition:
`fmap (f . g) = fmap f . fmap g`

## Applicative
An Applicative is a Functor with a bit more going on. We might want to allow an `fmap`-like operation with more arguments. If we had:
```haskell
fmap2 :: (a -> b -> c) -> f a -> f b -> f c
```
then this would do something useful:
```haskell
fmap2 (+) (Just 1) (Just 2)
```

But why stop at `fmap2`? Why not have `fmap3`, `fmap4`, and so on?

```haskell
class Functor f => Applicative f where
	pure :: a -> f a
	(<*>) :: f (a -> b) -> f a -> f b
```

Every Applicative must also be a Functor, but with two additional operations.
- `pure` lifts a value into the Applicative
- `(<*>)` is a sequential application

What is a sequential application?

To make `fmap2`, we could start with:
```haskell
fmap :: Maybe (Int -> Int)
fmap (+) (Just 2) 
```

But we run into trouble quickly. The partial function is inside a `Maybe`, so we can't simply apply it. We have to "run" the `Maybe` to get the value out before we can apply it again to the next `Maybe int`.

This is the job of `(<*>)`.
```haskell
pure (+) <*> (Just 1) <*> (Just 2)
```

`fmap2` can also be written like the following, but there's little reason to.
```haskell
fmap2 g x y = pure g <*> x <*> y
```
`pure` "lifts" a value into the Applicative. In other words, it turns an ordinary value into an Applicative one.
### Applicative Functor Laws
Similarly to Functor and Monad, there are laws about applicative instances.

Identity:
`pure id <*> v -> v`

Homomorphism:
`pure f <*> pure x -> pure (f x)`

Interchange:
`u <*> pure y -> pure ($ y) <*> u`

Composition:
`pure (.) <*> u <*> v <*> w -> u <*> (v <*> w)`

## Monad
The *real* Monad class in modern Haskell looks like the following:
```haskell
class Applicative m => Monad m where 
	(>>=) :: m a -> (a -> m b) -> m b 
	
	(>>) :: m a -> m b -> m b 
	m >> k = m >>= \_ -> k 
	
	return :: a -> m a 
	return = pure
```
(also in [[Function Examples#^monad-class]])


It's worth knowing this when writing instances:
Since every Monad is an Applicative, and every Applicative is a Functor, we can write the following (and Control.Monad already has):

```haskell
liftM :: (Monad f) => (a -> b) -> f a -> f b
liftM f m = do
			x <- m
			return f x
```
(also in [[Function Examples#^liftM]])

When writing a Functor instance, you can very often just say `fmap = liftM`, which basically means `fmap` is delegating to the definition of `return` (which itself is delegating to the definition of Applicative's `pure`).

A full definition for the State monad can be found at [[Function Examples#^state-monad]]