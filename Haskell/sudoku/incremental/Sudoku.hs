module Sudoku where

{-
 - Being a fan of sudoku and Haskell, it is only natural to try a Haskell
 - Sudoku Implementation
 -
 - @link - http://www.cs.nott.ac.uk/~pszgmh/sudoku.lhs
 -}
import Data.List
import Grid


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


-- Validity Checking
valid   :: Grid -> Bool
valid g =  all nodups (rows g) &&
           all nodups (cols g) &&
           all nodups (boxes g)

nodups        :: (Eq a) => [a] -> Bool
nodups []     =  True
nodups (x:xs) =  not (elem x xs) && nodups xs


-- Solver
type Choices = [Value]

choices :: Grid -> Matrix Choices
choices =  map (map choice)
  where choice v
          | empty v   = values
          | otherwise = [v]

cartProduct          :: [[a]] -> [[a]]
cartProduct []       =  [[]]
cartProduct (xs:xss) =  [ y:ys | y <- xs, ys <- cartProduct xss ]

collapse :: Matrix [a] -> [Matrix a]
collapse =  cartProduct . map cartProduct

solve :: Grid -> [Grid]
solve =  filter valid . collapse . choices


-- Pruning the Search Space
prune :: Matrix Choices -> Matrix Choices
prune =  pruneBy boxes . pruneBy cols . pruneBy rows
  where pruneBy f = f . map reduce . f

reduce     :: Row Choices -> Row Choices
reduce xss =  [xs `minus` singles | xs <- xss]
  where singles = concat (filter single xss)

minus         :: Choices -> Choices -> Choices
xs `minus` ys =  if single xs then xs else xs \\ ys

solve2 :: Grid -> [Grid]
solve2 =  filter valid . collapse . prune . choices


-- Repeated Prune until reaches 'Fix Point'
solve3 :: Grid -> [Grid]
solve3 =  filter valid . collapse . fix prune . choices

fix     :: Eq a => (a -> a) -> a -> a
fix f x =  if x == x' then x else fix f x'
  where x' = f x


-- Matrix Properties
complete :: Matrix Choices -> Bool
complete =  all (all single)

void :: Matrix Choices -> Bool
void =  any (any null)

safe    :: Matrix Choices -> Bool
safe cm =  all consistent (rows cm) &&
           all consistent (cols cm) &&
           all consistent (boxes cm)

consistent :: Row Choices -> Bool
consistent =  nodups . concat . filter single

blocked     :: Matrix Choices -> Bool
blocked mat =  void mat || not (safe mat)


-- Making Choices One at a Time
solve4 :: Grid -> [Grid]
solve4 =  search . prune . choices

search :: Matrix Choices -> [Grid]
search mat
  | blocked mat  = []
  | complete mat = collapse mat
  | otherwise    = [ g | mat' <- expand mat, g <- search (prune mat') ]

expand   :: Matrix Choices -> [Matrix Choices]
expand m =  [ rows1 ++ [row1 ++ [c] : row2] ++ rows2 | c <- cs ]
  where (rows1, row:rows2) = break (any (not . single)) m
        (row1, cs:row2)    = break (not . single) row


main :: IO ()
main =  putStrLn (unlines (head (solve4 diabolical)))
