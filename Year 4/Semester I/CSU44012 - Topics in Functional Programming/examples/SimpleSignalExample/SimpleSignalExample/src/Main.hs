module Main where
import qualified Control.Concurrent(threadDelay)

-- Thread.threadDelay is used to pause the program for a moment
-- so that the animation looks smoother.

-- A very simple library for manipulating continuous signals.
--

import Control.Monad (forM_)

-- Smart constructors


-- Shallow embedding

type Time = Double
newtype Signal a = Signal {at :: Time -> a}

-- Equivalent to the following two definitions
-- newtype Signal a = Signal (Time -> a)
-- at (Signal s) = s

-- Constant signal
--constant :: a -> Signal a
--constant x = Signal (const x)

-- Time varying signal
timeS  :: Signal Time
timeS = Signal id


instance Functor Signal where
  fmap f xs = pure f <*> xs

instance Applicative Signal where
  pure x = Signal $ const x
  fs <*> xs = Signal $ \t -> (fs `at` t)  (xs `at` t)



-- Combinators

-- Function application lifted to signals.
--applyS   :: Signal (a -> b) -> Signal a -> Signal b
--applyS fs xs = Signal (\t -> (fs `at` t)  (xs `at` t))

-- Transforming the time.
mapT   :: (Time -> Time)  -> Signal a -> Signal a
mapT f xs = Signal $ \t -> xs `at` f t
-- Equivalent to :
--  = Signal (at xs . f)
-- which I'm sure you'll agree is much neater!

-- Mapping a function over a signal.
-- We already provided this as 'fmap'
--mapS   :: (a -> b)        -> Signal a -> Signal b
--mapS f xs = pure f <*> xs



-- Sampling a signal at a given time point.
-- This is the /semantic function/ of our library.
sample :: Signal a -> Time -> a
sample = at


-- flipflopper
change a b t | odd (floor t) = a
             | otherwise     = b

--------------------------------------------
-- Example

-- sinusoidal of given frequency
sinS :: Double -> Signal Double
sinS freq = fmap sin timeS -- mapT (freq*) $ fmap sin timeS


scale :: Num a =>  Signal a -> Signal a
scale = fmap ((30*) . (1+))

-- Discretize a continuous signal
discretize :: Signal Double -> Signal Int
discretize = fmap round

-- convert to "analog"
toBars :: Signal Int -> Signal String
toBars = fmap (`replicate` '#')

displayLength = 500
-- display the signal at a number of points
-- threadDelay is used to pause the program momentarily so that
-- the resulting stages get displayed at a reasonable speed.
display :: Signal String -> IO ()
display ss = forM_ [0, 0.1 .. displayLength] $ \x ->
   putStrLn (sample ss x) >> Control.Concurrent.threadDelay 10000

-- Can't get enough? This definition runs forever instead of
-- up to the displayLength limit.
{-
display ss = forM_ [0, 0.1 .. ] $ \x ->
   putStrLn (sample ss x) >> Control.Concurrent.threadDelay 10000
-}


-- Alternative version, just to show another way to write it
-- in case you like this sort of thing
{-
clock = [0, 0.1 .. 500]
display ss = sequence_ [ frame x | x <- clock ]
   where frame x = do
                     putStrLn (sample ss x)
                     Control.Concurrent.threadDelay 10000
-}

-- The display magic.
-- Note how we take advantage of function composition,
-- types defined so far, etc.
magic :: Signal Double -> IO ()
magic = display . toBars . discretize . scale

main :: IO ()
main = magic $ sinS 0.1
