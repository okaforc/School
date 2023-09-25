## Refresher
```haskell
%% Tree example %%
data Tree a = Empty
				| Leaf a
				| Branch (Tree a) (Tree a)

f :: (a -> b) -> (Tree a) -> (Tree b)
tmap f Empty = Empty
tmap f (Leaf x) = Leaf (f x)
tmap f (Branch l r) = Branch (tmap f l) (tmap f r)
```

```haskell
%% Boundary example %%
threshold t v | v < t       = v
              | otherwise   = t
boundTr t = tmap (threshold 0.5) t
```


Haskell is a ***lazy*** language.
Haskell is a ***pure*** language.