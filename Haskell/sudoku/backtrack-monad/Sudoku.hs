module Sudoku where

{-# LANGUAGE FlexibleContexts #-}

import Control.Monad.State
import Data.Maybe (maybeToList)
import Data.List (delete)

type Value = Int
type Cell  = (Int, Int) -- Coordinates starting from 1

type Puzzle = [[ Maybe Value ]]
type Solution = [[ Value ]]

sqrtSize :: Int
sqrtSize =  3

size :: Int
size =  sqrtSize * sqrtSize

blocks :: [[Cell]]
blocks =  [ [ (x + i, y + j) | i <- [1..sqrtSize], j <- [1..sqrtSize] ]
                             | x <- [0, sqrtSize..(size - sqrtSize)], y <- [0, sqrtSize..(size - sqrtSize)] ]

blockNum            :: Cell -> Int
blockNum (row, col) =  row - (row - 1) `mod` sqrtSize + (col - 1) `div` sqrtSize

data Options = Options {
  cellOpts :: [[[ Value ]]],
  rowOpts  :: [[[ Cell ]]],
  colOpts  :: [[[ Cell ]]],
  blkOpts  :: [[[ Cell ]]]
} deriving Show

modifyCellOpts   :: MonadState Options m => ([[[Value]]] -> [[[Value]]]) -> m ()
modifyCellOpts f =  do opts <- get
                       put $ opts { cellOpts = f $ cellOpts opts }

modifyRowOpts   :: MonadState Options m => ([[[Cell]]] -> [[[Cell]]]) -> m ()
modifyRowOpts f =  do opts <- get
                      put $ opts { rowOpts = f $ rowOpts opts }

modifyColOpts   :: MonadState Options m => ([[[Cell]]] -> [[[Cell]]]) -> m ()
modifyColOpts f =  do opts <- get
                      put $ opts { colOpts = f $ colOpts opts }

modifyBlkOpts   :: MonadState Options m => ([[[Cell]]] -> [[[Cell]]]) -> m ()
modifyBlkOpts f =  do opts <- get
                      put $ opts { blkOpts = f $ blkOpts opts }

initOptions :: Options
initOptions =  Options {
  cellOpts = [[[ 1..size ] | _ <- [1..size]] | _ <- [1..size]],
  rowOpts  = [[[ (r, c)    | c <- [1..size]] | _ <- [1..size]] | r <- [1..size]],
  colOpts  = [[[ (r, c)    | r <- [1..size]] | _ <- [1..size]] | c <- [1..size]],
  blkOpts  = [[  b         | _ <- [1..size]] | b <- blocks]
}

{-
solve     :: Puzzle -> [Solution]
solve puz =  evalStateT (initPizzle >> solutions) initOptions
  where initPuzzle = sequence_ [ fixCell v (r, c) | (row, r) <- zip puz [1..],
                                                    (val, c) <- zip row [1..],
                                                    v <- maybeToList val ]
-}

solutions :: StateT Options [] Solution
solutions =  solveFromRow 1
  where solveFromRow r
          | r > size = return []
          | otherwise = do
            row <- solveRowFromCol r 1
            rows <- solveFromRow $ r + 1
            return $ row:rows
        solveRowFromCol r c
          | c > size = return []
          | otherwise = do
            vals <- gets $ (!! (c - 1)) . (!! (r - 1)) . cellOpts
            val <- lift vals
            fixCell val (r, c)
            row <- solveRowFromCol r (c + 1)
            return $ val:row

fixCell :: (MonadState Options m, MonadPlus m) => Value -> Cell -> m ()
fixCell val cell@(row, col) =
  do
     vals <- gets $(!! (col - 1)) . (!! (row - 1)) . cellOpts
     guard $ val `elem` vals
     modifyCellOpts $ replace2 row col [val]
     modifyRowOpts  $ replace2 row val [cell]
     modifyColOpts  $ replace2 col val [cell]
     modifyBlkOpts  $ replace2 blk val [cell]
     sequence_ [constrainCell v   cell     | v <- [1..size], v /= val]
     sequence_ [constrainCell val (row, c) | c <- [1..size], c /= col]
     sequence_ [constrainCell val (r, col) | r <- [1..size], r /= row]
     sequence_ [constrainCell val c        | c <- blocks !! (blk - 1), c /= cell]
  where blk = blockNum cell

constrainCell :: (MonadState Options m, MonadPlus m) => Value -> Cell -> m ()
constrainCell val cell@(row, col) =
  do
     constrainOpts row col val  cellOpts modifyCellOpts (flip fixCell cell)
     constrainOpts row val cell rowOpts  modifyRowOpts  (fixCell val)
     constrainOpts col val cell colOpts  modifyColOpts  (fixCell val)
     constrainOpts blk val cell blkOpts  modifyBlkOpts  (fixCell val)
  where blk = blockNum cell
        constrainOpts x y z getOpts modifyOpts fixOpts =
          do
             zs <- gets $ (!! (y - 1)) . (!! (x - 1)) . getOpts
             case zs of
               [z']  -> guard (z' /= z)
               [_,_] -> when (z `elem` zs) $ fixOpts (head $ delete z zs)
               (_:_) -> modifyOpts $ replace2 x y (delete z zs)
               _     -> mzero

replace :: Int -> a -> [a] -> [a]
replace i x (y:ys)
  | i > 1     = y : replace (i - 1) x ys
  | otherwise = x : ys
replace _ _ _ = []

replace2 :: Int -> Int -> a -> [[a]] -> [[a]]
replace2 i j x (y:ys)
  | i > 1        = y : replace2 (i - 1) j x ys
  | otherwise    = replace j x y : ys
replace2 _ _ _ _ = []
