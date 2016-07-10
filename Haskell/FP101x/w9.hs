module Main where
import Data.List
import Data.Char

data Nat = Zero | Succ Nat
  deriving Show

natToInteger :: Nat -> Integer
natToInteger =  \n -> genericLength [ c | c <- show n, c == 'S' ]

integerToNat   :: Integer -> Nat
integerToNat 0 =  Zero
integerToNat n =  Succ (integerToNat $ (-) n 1)

add            :: Nat -> Nat -> Nat
add Zero n     =  n
add (Succ m) n =  Succ (add n m)

mult            :: Nat -> Nat -> Nat
mult m Zero     =  Zero
mult m (Succ n) =  add m (mult m n)

data Tree = Leaf Integer | Node Tree Integer Tree

occurs                :: Integer -> Tree -> Bool
occurs m (Leaf n)     =  m == n
occurs m (Node l n r) =  case compare m n of
  LT -> occurs m l
  EQ -> True
  GT -> occurs m r

leaves              :: Tree -> Integer
leaves (Leaf _)     =  1
leaves (Node l n r) =  leaves l + leaves r

balanced              :: Tree -> Bool
balanced (Leaf _)     =  True
balanced (Node l n r) =  abs (leaves l - leaves r) <= 1 && balanced l && balanced r

halve    :: (Integral a) => [a] -> ([a], [a])
halve xs =  splitAt (length xs `div` 2) xs

balance     :: [Integer] -> Tree
balance [x] =  Leaf x
balance xs  =  Node (balance ys) 0 (balance zs)
  where (ys, zs) = halve xs

removeone          :: Eq a => a -> [a] -> [a]
removeone x []     = []
removeone x (y:ys)
  | x == y = ys
  | otherwise = y : removeone x ys

isChoice           :: Eq a => [a] -> [a] -> Bool
isChoice [] _      =  True
isChoice (x:xs) [] =  False
isChoice (x:xs) ys =  elem x ys && isChoice xs (removeone x ys)
