## The `IO a` type and Monads
This is a type that represents **actions that have side-effects**. This abstraction is the definition of **monads**.

**Monads** are types that represent actions that have side-effects when run.


There are some issues surrounding `IO` types. For example, the following function gets primitive characters:
```haskell
primGetChar :: Char
primPutChar :: Char -> ()
```

For these functions to be meaningful they would have to be performing some side-effecting IO operations whenever they are evaluated, which violates the principle of [[Definitions#^referential-transparency|referential transparency]].

The following two functions *should* be equivalent:
```haskell
f1 = (primGetChar, primGetChar)
f2 = let x = primGetChar in (x, x)
```
![[IO_Side_Effects.png]]

On the right, there is only one IO action performed due to `primGetChar`'s value being shared, while on the left there will be two. As a result, we can say that these two functions are *not* the same.

A solution to this is proposed as follows.
In order to get both uses of `primGetChar` to be the same, we will add an argument to the function and use it to distinguish them.
```haskell
getChar :: World -> (Char, World)
```
`primGetChar` is unsafe, but gets wrapped in this safe function that takes the side effects into account.

`World` here is the state of the program from one moment to another (impractical to actually exist or create). It is assumed that it is not possible to duplicate, copy, or otherwise create a second `World`.

With this in mind, the function can be written differently:
```haskell
f3 :: World -> ((Char, Char), World)
f3 w0 = ((ch1, ch2), w2)
		where
			(ch1, w1) = getChar w0
			(ch2, w2) = getChar w1
```
Threading the `World` parameters forces the evaluation to be in an intended order as long as each reference to the `World` is unique (at different moments).

An example of misuse is as follows:
```haskell
f3 :: World -> ((Char, Char), World)
f3 w = ((ch1, ch2), w2)
		where
			(ch1, w1) = getChar w
			(ch2, w2) = getChar w
```

Regardless, it should be easier to write this style of function. Instead, let's assume the World-modifying functions look like this:
```haskell
f :: World -> (a, World)
```
We can declare a type that captures this:
```haskell
type IO a = World -> (a, World)
```

Then, we can rewrite the older functions as:
```haskell
getChar :: IO Char
putChar :: Char -> IO ()
```

We can also declare an infix function with this type:
```haskell
(>>) :: IO a -> IO b -> IO b
```
used like:
```haskell
f4 = (putChar 'a') >> (putChar 'b')
```
This can be read as:
> Do the first thing, throw away the result but keep the World, then do the second thing.

A possible implementation of `(>>)` is:
```haskell
(>>) l r = \w -> let (_, w1) = l w in r w1
```
where `\w` is an inline lambda expression.

### Lambda / Anonymous Functions
Lambda notation is used to create once-off inline functions without creating new single-use functions in code. The backslash character (`\`) is used to start the lambda function.

For example, if you wanted to increment a list of numbers, you would normally do:
```haskell
incrementEverything numList = 
	num incf numList
		where incf x = x + 1
```

With lambda functions, we can use:
```haskell
incrementEverything numList = 
	map (\x -> x + 1) numList
```