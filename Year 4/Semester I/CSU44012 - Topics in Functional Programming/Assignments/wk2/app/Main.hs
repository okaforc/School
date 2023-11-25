-- q1
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
f1 :: [IO a] -> IO a
f1 [] = undefined -- case length == 0 (shouldn't ever get here)
f1 [x] = do x -- case length == 1
f1 (x:xs) = do x -- case length > 1
               f1 xs

-- q2
-- main =  f1 actions   
--         where actions = [putChar 'h', putChar 'e', putChar 'l', putChar 'l', putChar 'o']
-- The above prints "hello".

-- q3
main = do
        let a = f1 actions
            b = if True then putChar 'a' else putChar 'b'
        putStr "Hi there"
        where actions = [putChar 'h', putChar 'e', putChar 'l', putChar 'l', putChar 'o']
-- The above prints "Hi there". `a` and `b` are only assigned a value but never evaluated, whereas `putStr` is ran normally. 
-- As a result, only `putStr "Hi there"` is printed.

-- q4
while :: IO Bool -> IO()
while x = do
            y <- x
            if y
                then while x
                else return ()

-- q5
f2 :: [IO a] -> IO [a]
f2 [] = return []
f2 [x] = do
            y <- x
            return [y]
f2 (x:xs) = do 
            y <- x
            z <- f2 xs 
            return (y:z)


read10 :: IO String
read10 = f2 $ take 10 actions
          where actions = getChar : actions



