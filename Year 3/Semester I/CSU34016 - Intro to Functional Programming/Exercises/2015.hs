hof [] _ _ = []
hof _ [] _ = []
hof (x:xs) (y:ys) op =
    x `op` y : hof xs ys op

f1 [] _ = []
f1 _ [] = []
f1 (x:xs) (y:ys) = (x * y) : f1 xs ys

f2 [] _ = []
f2 _ [] = []
f2 (x:xs) (y:ys) = (x * y) : f2 xs ys

f3 [] _ = []
f3 _ [] = []
f3 (x:xs) (y:ys) = x y : f3 xs ys

f4 [] _ = []
f4 _ [] = []
f4 (x:xs) (y:ys) = (y,x) : f4 xs ys

f5 [] _ = []
f5 _ [] = []
f5 (x:xs) (y:ys) = x : f5 xs ys


ff1 a b = hof a b (*)
ff2 a b = hof a b (+)
ff3 a b = hof a b ff3h
-- ff4 a b = hof a b (\x(\y) -> (y,x))
ff5 a b = hof a b id
-- ff4 a b = hof a b (\(x,y)->(y,x))


-- f5 :: (t1 -> t2) -> t1 -> t2
-- f5 x y = x y
-- f6 :: (t1 -> t2) -> t1 -> p -> t2
-- f6 x y = (\c -> x y)

-- f7 :: (a -> b) -> a -> b
-- ff3h :: (t->a) -> t -> a
ff3h x y = x y
ff4h x y = (y, x)
