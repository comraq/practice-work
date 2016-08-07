module Sudoku where

{-
 - Being a fan of sudoku and Haskell, it is only natural to try a Haskell
 - Sudoku Implementation
 -
 - @link - http://www.cs.nott.ac.uk/~pszgmh/sudoku.lhs
 -}
import Data.List
import Grid


empty :: Value -> Bool
empty =  (== '.')

single     :: [a] -> Bool
single [_] =  True
single _   =  False

rows :: Matrix a -> [Row a]
rows =  id

cols :: Matrix a -> [Row a]
cols =  transpose

boxes :: Matrix a -> [Row a]
boxes = unpack . map cols . pack
  where pack   = split . map split
        split  = chop boxsize
        unpack = map concat . concat

chop      :: Int -> [a] -> [[a]]
chop n [] =  []
chop n xs =  take n xs : chop n (drop n xs)

valid   :: Grid -> Bool
valid g =  all nodups (rows g) &&
           all nodups (cols g) &&
           all nodups (boxs g)

nodups        :: (Eq a) => [a] -> Bool
nodups []     =  True
nodups (x:xs) =  not (elem x xs) && nodups xs

type Choices = [Value]

choices :: Grid -> Matrix Choices
choices =  map (map choice)
  where choice [] = values
        choice v  = [v]

cartProduct          :: [[a]] -> [[a]]
cartProduct []       =  [[]]
cartProduct (xs:xss) =  [ y:ys | y <- xs, ys <- cartProduct xss ]
