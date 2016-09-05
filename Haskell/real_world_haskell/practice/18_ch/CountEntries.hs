module CountEntries (listDirectory, countEntriesTrad) where

import Utils

import System.Directory (doesDirectoryExist, getDirectoryContents)
import System.FilePath ((</>))
import Control.Monad (forM, liftM)

listDirectory :: FilePath -> IO [String]
listDirectory = liftM (filter notDots) . getDirectoryContents
  where notDots p = p /= "." && p /= ".."
        notDots' = ((/= ".") &&& (/= "..")) >>> uncurry (&&)

countEntriesTrad :: FilePath -> IO [(FilePath, Int)]
countEntriesTrad path = do
  contents <- listDirectory path
  rest     <- forM contents $ \name -> do
                let newName = path </> name
                isDir <- doesDirectoryExist newName
                if isDir
                then countEntriesTrad newName
                else return []
  return $ (path, length contents) : concat rest

{-
 - Note that without Monad Transformers, we must explicitly build up the
 - resuling directory list within the 'IO' monad.
 -
 - However ideally, we should leverage the power of the 'Writer' monad to
 - record the filepaths and count. Unfortunately since we must traverse
 - directories within the 'IO' monad, we cannot use the 'Writer' monad
 - directly.
 -
 - Thus, we use 'WriterT' to add the recording capbility to a standard 'IO'
 - monad.
 -}
