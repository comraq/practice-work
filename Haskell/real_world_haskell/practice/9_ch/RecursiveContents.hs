module RecursiveContents
  ( getRecursiveContents
  , getRecursiveContents'
  ) where

import Utils

import Control.Monad    (forM)
import System.Directory (doesDirectoryExist, getDirectoryContents)
import System.FilePath  ((</>))

getRecursiveContents :: FilePath -> IO [FilePath]
getRecursiveContents topdir = do
  names <- getDirectoryContents topdir
  let properNames = filter (`notElem` [".", ".."]) names

  -- Since each iteration of the loop body (the 'do' block) has a return
  -- type IO [[FilePath]], need to 'return . concat' to flatten the 2D - list
  paths <- forM properNames $ \name -> do
    let path = topdir </> name
    isDirectory <- doesDirectoryExist path
    if isDirectory
    then getRecursiveContents path
    else return [path]

  return $ concat paths

getRecursiveContents' :: FilePath -> IO [FilePath]
getRecursiveContents' =
  let __getInnerContents = uncurry (<<?) . (getRecursiveContents' &&& (return . (:[])))
      _getInnerContents  = (doesDirectoryExist &&& __getInnerContents) >>> uncurry (>>=)
      getInnerContents   = _getInnerContents .* (</>)

      getProperNames     = filter (`notElem` [".", ".."])
      _getNamesThenLoop  = forM . getProperNames
      getNamesThenLoop   = curry $ (getInnerContents *** _getNamesThenLoop) >>> swap >>> app

      concatResults      = return . concat
      getResults         = getNamesThenLoop *. (>>= concatResults)

  in (getDirectoryContents &&& getResults) >>> uncurry (>>=)
