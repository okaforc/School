-- Part 1
data List a = Nil | Cons a (List a)

instance Functor List where 
    fmap _ Nil = Nil
    fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Applicative List where 
    pure xs = Cons xs Nil -- Cons is the same as `:`, so this turns a list xs into xs:[] in Applicative form
    Nil <*> _ = Nil -- Applying nothing sequentially results in nothing at the end
    (Cons x xs) <*> ys = fmap x ys `appendList` (xs <*> ys)

instance Monad List where 
    return = pure

-- Extra function to append two Lists
appendList :: List a -> List a -> List a
appendList Nil xs = xs
appendList (Cons x xs) ys = Cons x (appendList xs ys)



-- Part 2
data Pair a b = Pair a b
instance Functor (Pair a) where 
    fmap f (Pair x y) = Pair x (f y)

instance Applicative (Pair a) where 
    pure x = Pair a x -- This will return an error because `a` is not in scope and cannot be used.


main :: IO ()
main = print 4


