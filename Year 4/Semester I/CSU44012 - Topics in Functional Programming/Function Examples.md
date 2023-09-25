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
