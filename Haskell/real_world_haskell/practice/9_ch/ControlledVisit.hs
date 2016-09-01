import Utils

import Data.List (sort)
import Control.Monad (mapM, forM, liftM)
import System.FilePath  ((</>))
import Control.Exception (bracket, handle)
import System.IO (IOMode(..), hClose, hFileSize, openFile)
import System.Directory (doesDirectoryExist, getDirectoryContents)

import System.Directory
  ( Permissions(..)
  , getModificationTime
  , getPermissions
  )
import Data.Time (UTCTime(..))

data Info = Info {
  infoPath    :: FilePath
, infoPerms   :: Maybe Permissions
, infoSize    :: Maybe Integer
, infoModTime :: Maybe UTCTime
} deriving (Eq, Ord, Show)

getInfo :: FilePath -> IO Info
getInfo path = do
  perms    <- maybeIO (getPermissions path)
  size     <- maybeIO (bracket (openFile path ReadMode) hClose hFileSize)
  modified <- maybeIO (getModificationTime path)
  return $ Info path perms size modified

traverseI :: ([Info] -> [Info]) -> FilePath -> IO [Info]
traverseI order path = do
  names    <- getUsefulContents path
  contents <- mapM getInfo (path : map (path </>) names)
  liftM concat $ forM (order contents) $ \info -> do
    if isDirectory info && infoPath info /= path
      then traverseI order $ infoPath info
      else return [info]

getUsefulContents :: FilePath -> IO [String]
getUsefulContents path = do
  names <- getDirectoryContents path
  return $ filter (`notElem` [".", ".."]) names

isDirectory :: Info -> Bool
isDirectory = maybe False searchable . infoPerms

maybeIO :: IO a -> IO (Maybe a)
maybeIO act = handle ((\_ -> return Nothing) :: IOError -> IO (Maybe a)) $ Just `liftM` act

maybeIO' :: IO a -> IO (Maybe a)
maybeIO' =
  let exHandler = (const $ return Nothing) :: IOError -> IO (Maybe a)
      tryIO     = liftM Just

  in handle exHandler . tryIO

getUsefulContents' :: FilePath -> IO [String]
getUsefulContents' = getDirectoryContents >>> (>>= return . filter (`notElem` [".", ".."]))

reverseAlpha :: FilePath -> IO [Info]
reverseAlpha = traverseI (reverse . sort)

postOrder :: FilePath -> IO [Info]
postOrder = traverseI childrenFirst
  where ifNotEmpty     = (tail &&& ((:[]) . head)) >>> uncurry (++)
        ifEmpty        = const []
        sortList       = (ifEmpty &&& ifNotEmpty) >>> uncurry (<<?)
        childrenFirst  = (sortList &&& null) >>> app
