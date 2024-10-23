module Compilerbau where

-- 1.1 a

-- Define logical operators
myAnd :: (Bool, Bool) -> Bool
myAnd (x, y) = x && y

myOr :: (Bool, Bool) -> Bool
myOr (x, y) = x || y

myXor :: (Bool, Bool) -> Bool
myXor (x, y) = (x || y) && not (x && y)

myNeg :: Bool -> Bool
myNeg x = not x

-- 1.1 b
-- Half adder
halfAdder :: (Bool, Bool) -> (Bool, Bool)
halfAdder (a, b) = (myXor (a, b), myAnd (a, b))

-- Full adder
fullAdder :: (Bool, Bool, Bool) -> (Bool, Bool)
fullAdder (a, b, carry) =
  let sum1 = myXor (a, b)
      carry1 = myAnd (a, b)
      sum2 = myXor (sum1, carry)
      carry2 = myOr (carry1, myAnd (sum1, carry))
  in (sum2, carry2)

-- 1.2 a
-- Define the Term data type
data Term
  = Monom Int Int -- Monom with coefficient and variable
  | Add Term Term          -- Addition
  | Mul Term Term          -- Multiplication
  | Div Term Term          -- Division
  deriving (Show, Eq)

-- 1.2 b
-- Differentiate a Term
diff :: Term -> Term
diff (Monom x y) = Mul (Monom y 1) (Monom x (y-1)) -- Pattern Matching Haskell Style
diff (Add x y) = Add (diff x) (diff y) -- Rule of Sums for differentiation
diff (Mul x y) = Add (Mul (diff  x) y) (Mul x (diff y)) -- Chain rule for differentiating a product
diff (Div x y) = Div (Add (Mul (diff x) y) (Mul -1 (Mul x (diff y)) +) (Mul y y) -- Rule for division

-- 1.3 a 
-- Define the BBaum data type
data BBaum a = Empty | Node a (BBaum a) (BBaum a)
  deriving (Show, Eq)

-- 1.3 b 
-- Map function for BBaum
mapTree :: (BBaum a, (a -> b)) -> BBaum b
mapTree (Empty, _) = Empty
mapTree (Node val left right, f) = Node (f val) (mapTree (left, f)) (mapTree (right, f))

-- 1.4 a 
-- Convert a curried function to a tuple function
curry3 :: ((a, b, c) -> d) -> (a -> b -> c -> d)
curry3 f x y z = f (x, y, z)

-- Convert a tuple function to a curried function
uncurry3 :: (a -> b -> c -> d) -> ((a, b, c) -> d)
uncurry3 f (x, y, z) = f x y z

-- 1.5 
-- Convert each element of a list to a single-element list
toListofLists :: [a] -> [[a]]
toListofLists = map (\x -> [x])

-- 1.6
-- Concatenate a list of strings
listStringConcat :: [String] -> String
listStringConcat = foldr (++) ""
