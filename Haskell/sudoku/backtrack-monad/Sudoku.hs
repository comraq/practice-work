{-# LANGUAGE FlexibleContexts #-}

module Sudoku where

{-
 - My own adaptation of a Sudoku solver using StateT and [] for backtracking
 -
 - Inspired from: https://wiki.haskell.org/Sudoku#Backtrack_monad_solver
 -}

import Utils
import Data.List (delete)
import Control.Monad.Plus (mfromMaybe)
import Control.Monad.State
import Control.Monad.Trans

-- Each Sudoku Cell Contains a Char
type Value  = Char

{-
 - Each Cell is of coordinates where:
 - - (0, 0) being the top left
 - - (8, 8) being the bottom right
 -   in a typical 3 x 3 (size 3 sudoku)
 -}
type Cell  = (Int, Int)

-- 2D list of size (dimensions * dimensions)
type Puzzle   = [[Maybe Value]]
type Solution = [[Value]]


size :: Int
size =  3

dimensions :: Int
dimensions =  size * size

valuesRange :: [Value]
valuesRange =  ['0'..'9'] ++ ['A'..'Z']

getValues :: Int -> [Value]
getValues =  (`take` valuesRange) . square
  where square n = n * n

blocks :: [[Cell]]
blocks =  [[ (row + r * size, col + c * size )
             | row <- blkSize, col <- blkSize ]
             | r   <- blkSize, c   <- blkSize ]
  where blkSize = [0..size - 1]

blockNum :: Cell -> Int
blockNum (r, c) = (r * dimensions + c) `mod` dimensions

type CellOptions  = [[[Value]]]         -- list of possible values for each cell
type ValueOptions = [[(Value, [Cell])]]
type RowOptions   = ValueOptions        -- list of rows of values mapping to cells
type ColOptions   = ValueOptions        -- list of cols of values mapping to cells
type BlkOptions   = ValueOptions        -- list of blks of values mapping to cells

data Options = Options {
  cellOpts :: CellOptions
, rowOpts  :: RowOptions
, colOpts  :: ColOptions
, blkOpts  :: BlkOptions
} deriving Show

modifyCellOpts :: (MonadState Options m, MonadPlus m) => (CellOptions -> CellOptions) -> m ()
modifyCellOpts f = do
  options <- get
  put $ options { cellOpts = f $ cellOpts options }

modifyRowOpts :: (MonadState Options m, MonadPlus m) => (RowOptions -> RowOptions) -> m ()
modifyRowOpts f = do
  options <- get
  put $ options { rowOpts = f $ rowOpts options }

modifyColOpts :: (MonadState Options m, MonadPlus m) => (ColOptions -> ColOptions) -> m ()
modifyColOpts f = do
  options <- get
  put $ options { colOpts = f $ colOpts options }

modifyBlkOpts :: (MonadState Options m, MonadPlus m) => (BlkOptions -> BlkOptions) -> m ()
modifyBlkOpts f = do
  options <- get
  put $ options { blkOpts = f $ blkOpts options }

initOptions :: Options
initOptions = Options {
      cellOpts = [[ values       | _     <- cellsRange ]
                                 | _     <- cellsRange ]

    , rowOpts  = [[ (v, [ (r, c) | c     <- cellsRange ])
                                 | v     <- values     ]
                                 | r     <- cellsRange ]

    , colOpts  = [[ (v, [ (r, c) | r     <- cellsRange ])
                                 | v     <- values     ]
                                 | c     <- cellsRange ]

    , blkOpts  = [[ (v, block)   | v     <- values     ]
                                 | block <- blocks     ]
    }

  where
    -- The Valid 'Char' Values for Each Sudoku Cell
    values :: [Value]
    values =  getValues size

    cellsRange :: [Int]
    cellsRange = [0 .. dimensions - 1]

setCellValue :: (MonadState Options m, MonadPlus m) => Cell -> Value -> m ()
cell@(row, col) `setCellValue` value = do

    {-
     - Get previously remaining values and guard that the value to set is in
     - the previously remaining values
     -}
    remainingVals <- gets $ getCellValues cell
    guard $ value `elem` remainingVals

    -- Set the value in cell, row, col and blk options
    modifyCellOpts $ replaceCellOpts row col   [value]
    modifyRowOpts  $ replaceOpts     row value [cell]
    modifyColOpts  $ replaceOpts     col value [cell]
    modifyBlkOpts  $ replaceOpts     blk value [cell]

    -- Remove all other values from all occurrences of this 'cell'
    sequence_ [constrainCell v     cell     | v <- valuesRange, v /= value]

    -- Remove current 'value' from current 'row' with col /= this 'col'
    sequence_ [constrainCell value (row, c) | c <- cellsRange,  c /= col ]

    -- Remove current 'value' from current 'col' with row /= this 'row'
    sequence_ [constrainCell value (r, col) | r <- cellsRange,  r /= row ]

    {-
     - Remove current 'value' from cells in currentBlk where
     - the block cell /= this 'cell'
     -}
    sequence_ [constrainCell value blkCell  | blkCell <- currentBlk
                                            , blkCell /= cell ]

  where
    -- Gets the list of possible values of a given Cell from CellOptions
    getCellValues :: Cell -> Options -> [Value]
    getCellValues (row, col) = cellOpts >>> (!! row) >>> (!! col)

    blk :: Int
    blk = blockNum cell

    currentBlk :: [Cell]
    currentBlk = blocks !! blk

    cellsRange :: [Int]
    cellsRange = [0 .. dimensions - 1]

    valuesRange :: [Value]
    valuesRange =  getValues size

{-
 - The 'Alternative' and 'MonadPlus' implementations of []:
 -
 - instance Alternative [] where
 -   empty = []
 -   (<|>) = (++)
 -
 - instance MonadPlus [] where
 -   mzero = empty
 -   mplus = (<|>)
 -}

{-
 - Assert that the cell to constrain must have the 'val' as one of its
 - possible values.
 -
 -}
constrainCell :: (MonadState Options m, MonadPlus m)
              => Value
              -> Cell
              -> m ()
constrainCell val cell@(row, col) = do
    constrainCellOpts cell val
    constrainOpts     row  val cell modifyRowOpts rowOpts
    constrainOpts     col  val cell modifyColOpts colOpts
    constrainOpts     blk  val cell modifyBlkOpts blkOpts

  where
    constrainCellOpts :: (MonadState Options m, MonadPlus m)
                      => Cell -> Value -> m ()
    constrainCellOpts cell@(row, col) valToRemove = do
      valsRemain <- gets $ cellOpts >>> (!! row) >>> (!! col)
      case valsRemain of
        [val] -> guard (val /= valToRemove)
        [_,_] -> when (valToRemove `elem` valsRemain) $
                      cell `setCellValue` head (delete valToRemove valsRemain)
        (_:_) -> modifyCellOpts $ replaceCellOpts row col
                                  (delete valToRemove valsRemain)
        _     -> mzero

    constrainOpts :: (MonadState Options m, MonadPlus m)
                  => Int -> Value -> Cell
                  -> ((ValueOptions -> ValueOptions) -> m ())
                  -> (Options -> ValueOptions)
                  -> m ()
    constrainOpts index value cellToRemove modifyOpts getOpts = do
      cellsRemain <- gets $ getOpts >>> (!! index) >>> lookup value
                                    >>> mfromMaybe >>> join
      case cellsRemain of
        [cell] -> guard (cell /= cellToRemove)
        [_,_]  -> when (cellToRemove `elem` cellsRemain) $
                       head (delete cellToRemove cellsRemain) `setCellValue` value
        (_:_)  -> modifyOpts $ replaceOpts index value
                               (delete cellToRemove cellsRemain)
        _      -> mzero

    blk :: Int
    blk = blockNum cell

solutions :: StateT Options [] Solution
solutions = solveFromRow 0
  where
    solveFromRow :: Int -> StateT Options [] Solution
    solveFromRow row
      | row >= dimensions = return []
      | otherwise         = do
          solvedRow  <- solveRowFromCol row 0
          solvedRows <- solveFromRow $ row + 1
          return $ solvedRow : solvedRows

    solveRowFromCol :: Int -> Int -> StateT Options [] [Value]
    solveRowFromCol row col
      | col >= dimensions = return []
      | otherwise         = do
          possibleVals <- gets $ cellOpts >>> (!! row) >>> (!! col)
          value        <- lift possibleVals

          (row, col) `setCellValue` value
          solvedRow <- solveRowFromCol row $ col + 1
          return $ value : solvedRow



{-
 - Helper Setter Functions
 -
 - TODO: Consider using lens setters
 -}
replaceCellOpts :: Int -> Int -> [Value] -> CellOptions -> CellOptions
replaceCellOpts 0   col newVals (vs:vss) = replaceInList col newVals vs : vss
replaceCellOpts row col newVals (vs:vss) = vs : replaceCellOpts (row - 1) col newVals vss

replaceInList :: Int -> a -> [a] -> [a]
replaceInList 0 newX (_:xs) = newX:xs
replaceInList n newX (x:xs) = x : replaceInList (n - 1) newX xs

replaceOpts :: Int -> Value -> [Cell] -> ValueOptions -> ValueOptions
replaceOpts 0 value cells (pr:prs) = setAssocList (== value) cells pr : prs
replaceOpts n value cells (pr:prs) = pr : replaceOpts (n - 1) value cells prs

setAssocList :: (k -> Bool) -> v -> [(k, v)] -> [(k, v)]
setAssocList predicate newVal ((key, value):prs)
  | predicate key = (key, newVal) : prs
  | otherwise     = (key, value)  : setAssocList predicate newVal prs

main :: IO ()
main = print $ runStateT solutions initOptions
