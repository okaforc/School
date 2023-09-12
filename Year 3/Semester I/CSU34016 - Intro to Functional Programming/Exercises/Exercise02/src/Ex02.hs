{- butrfeld Andrew Butterfield -}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Ex02 where

name, idno, username :: String
name = "C Okafor" -- replace with your name
idno = "20332400" -- replace with your student id
username = "okaforc" -- replace with your TCD username

declaration -- do not modify this
  =
  unlines
    [ "",
      "@@@ This exercise is all my own work.",
      "@@@ Signed: " ++ name,
      "@@@ " ++ idno ++ " " ++ username
    ]

-- Datatypes and key functions -----------------------------------------------

-- do not change anything in this section !

type Id = String

data Expr
  = Val Double
  | Add Expr Expr
  | Mul Expr Expr
  | Sub Expr Expr
  | Dvd Expr Expr
  | Var Id
  | Def Id Expr Expr
  deriving (Eq, Show)

type Dict k d = [(k, d)]

define :: Dict k d -> k -> d -> Dict k d
define d s v = (s, v) : d

find :: Dict String d -> String -> Either String d
find [] name = Left ("undefined var " ++ name)
find ((s, v) : ds) name
  | name == s = Right v
  | otherwise = find ds name

type EDict = Dict String Double

v42 = Val 42

j42 = Just v42

-- do not change anything above !

-- Part 1 : Evaluating Expressions -- (50 test marks, worth 25 Exercise Marks) -

-- Implement the following function so all 'eval' tests pass.

-- eval should return `Left msg` if:
-- (1) a divide by zero operation was going to be performed;
-- (2) the expression contains a variable not in the dictionary.
-- see test outcomes for the precise format of those messages

f1 = Dvd (Val 1) (Val 0)

eval :: EDict -> Expr -> Either String Double
-- value/variable evaluation
eval [] (Val e) = Right e
eval d (Val e) = Right e
eval [] (Var e) = Left ("undefined var " ++ e)
eval d (Var e) = find d e

-- addition
eval [] (Add (Var a) (Val b)) = Left ("undefined var " ++ a)
eval [] (Add (Val a) (Var b)) = Left ("undefined var " ++ b)
eval [] (Add (Val a) (Val b)) = Right (a+b)
-- eval [] (Add _ f1) = Left "div by zero"
-- eval [] (Add (Val a) (Var b)) = if b == "div by zero" then Left "div by zero" else Right 

eval d (Add (Val a) (Var b)) = if b == "div by zero"
  then Left b
  else case eval d (Var b) of
    Left s -> Left "div by zero"
    Right x -> Right (a + x)

eval d (Add (Var a) (Val b)) = if a == "div by zero"
  then Left a
  else case eval d (Var a) of
    Left s -> Left "div by zero"
    Right x -> Right (x + b)

eval d (Add (Var a) (Var b)) = case eval d (Var a) of
  Left s -> Left ("undefined var " ++ a)
  Right x -> case eval d (Var b) of
    Left s -> Left ("undefined var " ++ b)
    Right y -> Right (x + y)
eval _ (Add _ f1) = Left "div by zero"

-- subtraction
eval [] (Sub (Var a) (Val b)) = Left ("undefined var " ++ a)
eval [] (Sub (Val a) (Var b)) = Left ("undefined var " ++ b)
eval _ (Sub (Val a) (Val b)) = Right (a - b)
eval d (Sub (Val a) (Var b)) = case eval d (Var b) of
  Left s -> Left "div by zero"
  Right x -> Right (a - x)
eval d (Sub (Var a) (Val b)) = case eval d (Var a) of
  Left s -> Left "div by zero"
  Right x -> Right (b - x)
eval d (Sub (Var a) (Var b)) = case eval d (Var a) of
  Left s -> Left ("undefined var " ++ a)
  Right x -> case eval d (Var b) of
    Left s -> Left ("undefined var " ++ b)
    Right y -> Right (x - y)
eval _ (Sub _ f1) = Left "div by zero"


-- multiplication
eval [] (Mul (Var a) (Val b)) = Left ("undefined var " ++ a)
eval [] (Mul (Val a) (Var b)) = Left ("undefined var " ++ b)
eval _ (Mul (Val a) (Val b)) = Right (a * b)
eval d (Mul (Val a) (Var b)) = case eval d (Var b) of
  Left s -> Left "div by zero"
  Right x -> Right (a * x)
eval d (Mul (Var a) (Val b)) = case eval d (Var a) of
  Left s -> Left "div by zero"
  Right x -> Right (b * x)
eval d (Mul (Var a) (Var b)) = case eval d (Var a) of
  Left s -> Left ("undefined var " ++ a)
  Right x -> case eval d (Var b) of
    Left s -> Left ("undefined var " ++ b)
    Right y -> Right (x * y)
eval _ (Mul _ f1) = Left "div by zero"

-- division
eval [] (Dvd (Var a) (Val b)) = Left ("undefined var " ++ a)
eval [] (Dvd (Val a) (Var b)) = Left ("undefined var " ++ b)
eval _ (Dvd (Dvd(Var a)(Val 0)) (Var b)) = Left "div by zero"
eval _ (Dvd (Dvd(Var a)(Val 0)) (Dvd(Var b)(Val 0))) = Left "div by zero"

eval _ (Dvd (Val a) (Val b)) =
  if b == 0
    then Left "div by zero"
    else Right (a / b)
eval d (Dvd (Val a) (Var b)) = case eval d (Var b) of
  Left s -> Left "div by zero"
  Right x -> Right (a / x)
eval d (Dvd (Var a) (Val b)) = case eval d (Var a) of
  Left s -> Left "div by zero"
  Right x -> Right (b / x)
eval d (Dvd (Var a) (Var b)) = case eval d (Var a) of
  Left s -> Left ("undefined var " ++ a)
  Right x -> case eval d (Var b) of
    Left s -> Left ("undefined var " ++ b)
    Right y ->
      if y == 0
        then Left "div by zero"
        else Right (x / y)
eval _ (Dvd _ f1) = Left "div by zero"

-- Part 2 : Expression Laws -- (15 test marks, worth 15 Exercise Marks) --------

{-

There are many, many laws of algebra that apply to our expressions, e.g.,

  x * y            =   y * z        Law 1
  (x + y) + z      =   x + (y + z)  Law 2
  (x / y) / z      =   x / (y * z)  Law 3
  (x + y)*(x - y)  =  x*x - y*y     Law 4
  ...

  We can implement these directly in Haskell using Expr

  Function LawN takes an expression:
    If it matches the "shape" of the law lefthand-side,
    it replaces it with the corresponding righthand "shape".
    If it does not match, it returns Nothing

    Implement Laws 1 through 4 above
-}

-- x * y            =   y * z        Law 1
law1 :: Expr -> Maybe Expr
-- law1 e = error "law1 NYI"
law1 (Add(Var a) (Var b)) = Just (Add(Var b) (Var a))
law1 (Add(Val a) (Val b)) = Nothing
law1 (Mul(Var a) (Var b)) = Just (Mul(Var b) (Var a))
law1 (Mul(Val a) (Val b)) = Just (Mul(Val b) (Val a))

-- (x + y) + z      =   x + (y + z)  Law 2
law2 :: Expr -> Maybe Expr
-- law2 e = error "law2 NYI"
law2 (Add(Add(Var a) (Var b)) (Var c)) = Just (Add(Var a) (Add(Var b) (Var c))) -- (x + y) + z
law2 (Add(Add(Val a) (Val b)) (Val c)) = Just (Add(Val a) (Add(Val b) (Val c))) -- (1 + 2) + 3
law2 (Add(Add(Var a) (Var b)) (Val c)) = Just (Add(Var a) (Add(Var b) (Val c))) -- (x + y) + 7
law2 (Dvd(Val a) (Dvd(Val b) (Val c))) = Nothing

-- (x / y) / z      =   x / (y * z)  Law 3
law3 :: Expr -> Maybe Expr
-- law3 e = error "law3 NYI"
law3 (Dvd(Dvd(Var a) (Var b)) (Var c)) = Just (Dvd(Var a) (Mul(Var b) (Var c))) -- (x / y) / z
law3 (Dvd(Dvd(Val a) (Val b)) (Val c)) = Just (Dvd(Val a) (Mul(Val b) (Val c))) -- (1 / 2) / 3
law3 (Dvd(Dvd(Var a) (Var b)) (Val c)) = Just (Dvd(Var a) (Mul(Var b) (Val c))) -- (x / y) / 7
law3 (Sub(Sub(Val a) (Val b)) (Val c)) = Nothing

-- (x + y)*(x - y)  =  x*x - y*y     Law 4
law4 :: Expr -> Maybe Expr
-- law4 e = error "law4 NYI"
law4 (Mul(Add(Var a) (Var b)) (Sub(Var c) (Var d))) = if Var a == Var c && Var b == Var d -- get around "conflicting definitions"
  then Just (Sub(Mul(Var a) (Var a)) (Mul(Var b) (Var b)))
  else Nothing
law4 (Mul(Add(Val a) (Val b)) (Sub(Val c) (Val d))) = if Val a == Val c && Val b == Val d
  then Just (Sub(Mul(Val a) (Val a)) (Mul(Val b) (Val b)))
  else Nothing
law4 (Sub(Mul(Val a) (Val c)) (Mul(Val b) (Val d))) = Nothing
