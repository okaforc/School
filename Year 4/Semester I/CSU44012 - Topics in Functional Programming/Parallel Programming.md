
Some terminology that we have to get right from the outset: 
- Parallelism is using multiple compute elements (e.g. more than one CPU core) to perform a computation. 
- Concurrency is the use of more than one thread of program control at a time (which may, or may not, involve more than one processor)

## Libraries
### `Control.Parallel`
The `Control.Parallel` library has several utilities to let us tell the runtime that there are sites of potential parallelism. For example,
```haskell
par :: a -> b -> b
``` 
(In [[Function Examples]])

This function must mean that this must be something equivalent to
``` haskell
par left right = right
```

Or even
``` haskell
par left right = par left right
```
(infinite recursion.)

#### Laziness
Take, for example, two expressions entered to the GHCi prompt.
``` haskell
Prelude> let x = 1 + 2 :: Int
Prelude> 3
```
These are very similar, but are different because of their evaluation. Testing them returns
```haskell
Prelude> x == 3
True
```

But `x` can be represented as either
- "3", or
- a lazily-unevaluated computation of "1+2"

#### Debugging
GHCi also has a debugging operation to see if something is evaluated or not/
```haskell
Prelude> let x = 1 + 2 :: Int
Prelude> :sprint x
x = _
Prelude> x
3
Prelude> :sprint x
x = 3
```

The **underscore** on line 3 means that `x` has not yet been evaluated and cannot be printed. When asking to display `x` on line 4, GCHi evaluates the value stored within "1 + 2" and stores that in `x`. Then, when asking to display `x` again on line 6, `:sprint x` returns the evaluated value now stored in `x`.

The unevaluated value in the underscore is sometimes called a ***Thunk***. 

More complex examples could also include the unevaluated "graphs".

```haskell
let x = 1 + 2
let y = x + x
```
#### Sharing
***Sharing*** refers to GHCi evaluating an unevaluated value once and using it in the next reference immediately instead of re-evaluating it every time.

{graph pic coming soon?????}

Going back to [[Function Examples#^parfunc|par]]: semantically, the expression `par x y` is equivalent to just `y`, but the runtime is allowed to use it as a hint that it would be a good idea to evaluate `x` in parallel with `y`.

{
stuff about sharing, concurrency, and faster fibonacci
}

#### Ordering
`pseq` orders computations in a strict order. It works similarly to `par` but is strict in its first argument.

```haskell
fib :: Integer -> Integer 
fib 0 = 0 
fib 1 = 1 
fib n = par nf1 (pseq nf2 (nf1 + nf2) 
		where nf1 = fib (n-1) 
			   nf2 = fib (n-2)
```

## The `Eval` Monad
(see [[Monads#The `Eval` Monad|The Eval Monad]])