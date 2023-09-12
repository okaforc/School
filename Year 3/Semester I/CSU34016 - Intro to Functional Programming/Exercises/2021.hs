import System.IO

hof x [] op = x
hof x (y:ys) op = hof (x `op` y) ys op

f1 a bs = hof a bs (*)
f2 a bs = hof a bs (||)
f3 a bs = hof a bs f3h
f4 a bs = hof a bs f4h
f5 a bs = hof a bs (-)

f3h a b = 2*a + b
f4h a b = b ++ a

-- Q2


-- Q3
-- fCopyChar :: FilePath -> FilePath -> FilePath -> IO ()
-- fCopyChar f1 f2 tof = 
--     do 
--         eof <- isEOF h
--         if eof then 
--             -- hClose ff1
--             -- hClose ff2
--             return cs
--         else do 
--             turn <- 0
--             ff1 <- openFile f1 ReadMode
--             ff2 <- openFile f2 ReadMode
--             l1 <- hGetLine ff1
--             l2 <- hGetLine ff2
--             tf <- openFile tof WriteMode
--             hPutChar tf c
--             hClose tf