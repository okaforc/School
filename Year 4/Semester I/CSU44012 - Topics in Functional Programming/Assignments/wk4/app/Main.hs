-- Part 1
type Log = [String]

data Writer a = Writer Log a deriving Show

runWriter :: Writer a -> (Log, a)
runWriter (Writer l x) = (l, x)

instance Functor Writer where
    fmap f (Writer log val) = Writer log (f val)

instance Applicative Writer where
    pure = Writer []
    (Writer l1 f) <*> (Writer l2 a) = Writer (l1++l2) (f a)

{- 
The default definition of <*> is
<*> = liftA2 id
and liftA2 is
liftA2 f c = (<*>) (fmap f x)
so we'd get an infinite recursion trying to apply it if we don't define something sensible.
 -}

instance Monad Writer where
    return = pure
    m >>= k = let Writer l a = m
                  Writer l' a' = k a
                  in Writer (l++l') a'

tell :: String -> Writer()
tell logMessage = Writer [logMessage] ()

example :: Writer Int
example = do
  tell "entry 1"
  tell "entry 7"
  return (1 + 1)

main = do print example

-- Part 2
-- a
data Writer2 l a = Writer2 [l] a

runWriter2 :: Monoid L => Writer2 l a -> ([l], a)
runWriter2 (Writer2 l x) = (l, x)

instance Functor (Writer2 l) where
    fmap f (Writer2 log val) = Writer2 log (f val)

instance Monoid => Applicative (Writer2 l) where
    pure x = Writer2 mempty x
    (Writer2 l1 f) <*> (Writer2 l2 a) = Writer2 (l1++l2) (f a)

instance Monad Writer where
    return = pure
    m >>= k = let Writer2 l a = m
                  Writer2 l' a' = k a
                  in Writer2 (l++l') a'


tell2 :: a -> Writer2 a ()
tell2 logMessage = Writer2 [logMessage] ()

example :: Writer2 String Int
example = do
  tell2 "entry 1"
  tell2 "entry 7"
  return (1 + 1)

-- b


