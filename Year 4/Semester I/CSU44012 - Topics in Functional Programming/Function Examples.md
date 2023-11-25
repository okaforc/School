## Fibonacci
### Concurrent
```haskell
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = par nf (fn + fib (n-2)) 
		where nf = fib (n-1)
```
^fibfunc-concurrent

Uses :
- [[#^parfunc|par]]

## Control.Parallel
### par
```haskell
par :: a -> b -> b
``` 
^parfunc

## Monad
```haskell
class Monad m where
	(>>=)  :: m a -> (a -> m b) -> m b
	return :: a -> m a
```
^monad-class-old

```haskell
class Applicative m =>= Monad m where 
	(>>=) :: m a -> (a -> m b) -> m b 
	
	(>>) :: m a -> m b -> m b 
	m >> k = m >>= \_ -> k 
	
	return :: a -> m a 
	return = pure
```
^monad-class

## Maybe
```haskell
data Maybe = Just a
		   | Nothing

instance Monad Maybe where
	(>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
	Nothing >>= k = Nothing
	(Just x) >>= k = k x

	return :: a -> Maybe a
	return x = Just x
```
^maybe-type

## liftM
```haskell
liftM :: (Monad f) => (a -> b) -> f a -> f b
liftM f m = do
			x <- m
			return f x
```
^liftM

## State Monad
```haskell
instance Functor (State s) where 
	fmap = liftM 

instance Applicative (State s) where 
	pure a = State (\s -> (a,s)) 

instance Monad (State s) where 
	m >>= k = State (\s -> let (a,s') = runState m s 
							in runState (k a) s')
```
^state-monad

## Scotty Example
```haskell
{-# LANGUAGE OverloadedStrings #-} 
import Web.Scotty 
main = scotty 3000 $ do 
	get "/" $ do 
		html "Hello World!"
```
^scotty-example

## Blaze Example
```haskell
import qualified Text.Blaze.Html5 as H 
import qualified Text.Blaze.Html5.Attributes as A 
import qualified Text.Blaze.Html.Renderer.Text as R

response :: Text -> Text
response n = do R.renderHtml $ do
	H.h1 ("Hello" >> H.toHtml n)

renderHTML :: Html -> Text 
h1 :: Html -> Html 
toHtml :: ToMarkup a => a -> Html

longresponse :: Text -> Text 
longresponse n = do R.renderHtml $ do 
	H.head $ H.title "Welcome page" H.body $ do 
		H.h1 "Welcome!" 
		H.p ("Welcome to my Scotty app" >> H.toHtml n)

myImage :: Html
myImage = img ! src "catPicture.jpg" ! alt "Awwww."
```
^blaze-example