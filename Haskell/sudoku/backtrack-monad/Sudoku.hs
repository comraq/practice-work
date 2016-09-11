module Sudoku where

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
getValues =  flip take valuesRange . square
  where square n = n * n

-- The Valid 'Char' Values for Each Sudoku Cell
values :: [Value]
values =  getValues size

blocks :: [[ Cell ]]
blocks =  [[ (row + r * size, col + c * size )
             | row <- blkSize, col <- blkSize ]
             | r   <- blkSize, c   <- blkSize ]
  where blkSize = [0..size - 1]

blockNum :: Cell -> Int
blockNum (r, c) = r * dimensions + c

data Options = Options { cells :: [[[ Value ]]]
                       , rows  :: [[[ Cell  ]]]
                       , cols  :: [[[ Cell  ]]]
                       , blks  :: [[[ Cell  ]]]
                       } deriving Show

-- initOptions :: Options
-- initOptions = Options {
--   cells = [[ values | _ <- [0..dimensions - 1] ] | _ <- [0..dimensions - 1] ],
--   rows  =
-- }
