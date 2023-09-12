hof [] iden op = iden
hof (x:xs) iden op = x `op` hof xs iden op

ff1 [] = 1
ff1 (x:xs) = x * ff1 xs

ff2 [] = 0
ff2(x:xs) = 1 + ff2 xs

ff3 [] = 0
ff3(x:xs) = x + ff3 xs

ff4 [] = []
ff4(x:xs) = x ++ ff4 xs

ff5 [] = 0
ff5(x:xs) = (x*x) + ff5 xs


f1 x = hof x 1 (*)
f2 x = hof x 0 f2h
f3 x = hof x 0 (+)
f4 x = hof x [] (++)
f5 x = hof x 0 f5h

f2h x y = 1 + y
f5h x y = (x*x) + y