f1 :: [IO a] -> IO a
f1 (a:b:c:d:e:xss) = do a
                        b
                        c
                        d
                        e

-- main = do
--         let a = f1 actions
--             b = if True then putChar 'a' else putChar 'b'
--         putStr "Hi there"
--         where actions = [putChar 'h', putChar 'e', putChar 'l', putChar 'l', putChar 'o']

main =  f1 actions   
        where actions = [putChar 'h', putChar 'e', putChar 'l', putChar 'l', putChar 'o']
