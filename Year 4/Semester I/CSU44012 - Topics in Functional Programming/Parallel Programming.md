# Libraries
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

Going back to [[Function Examples#^parfunc|par]], 

{
stuff about sharing, concurrency, and faster fibonacci
}

#### Ordering
`pseq` orders 