module ControlledVisit
  ( Info(..)
  , getInfo
  , isDirectory
  , getUsefulContents
  ) where

import Utils

import Data.List (sort)
import Control.Monad (mapM, forM, liftM)
import System.FilePath  ((</>), takeExtension)
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

traverseIVerbose :: ([Info] -> [Info]) -> FilePath -> IO [Info]
traverseIVerbose order path = do
    names <- getDirectoryContents path
    let usefulNames = filter (`notElem` [".", ".."]) names
    contents <- mapM getEntryName $ "" : usefulNames
    recursiveContents <- mapM recurse $ order contents
    return $ concat recursiveContents
  where getEntryName name = getInfo (path </> name)

        isDirectory info  = case infoPerms info of
          Nothing    -> False
          Just perms -> searchable perms

        recurse info      = do
          if isDirectory info && infoPath info /= path
          then traverseIVerbose order $ infoPath info
          else return [info]

{-
 - Excessive use of (.) to compose long pipelines may be difficult to
 - understand, fragile to change (code is too dense).
 -}

type InfoP a = Info -> a

pathP :: InfoP FilePath
pathP = infoPath

sizeP :: InfoP Integer
sizeP = maybe (-1) id . infoSize

equalP :: Eq a => InfoP a -> a -> InfoP Bool
equalP f k inf = f inf == k

liftP :: (a -> b -> c) -> InfoP a -> b -> InfoP c
liftP op f k inf = f inf `op` k

greaterP, lesserP :: Ord a => InfoP a -> a -> InfoP Bool
greaterP = liftP (>)
lesserP = liftP (<)

simpleAndP :: InfoP Bool -> InfoP Bool -> InfoP Bool
-- simpleAndP f g inf = f inf &&& g inf
simpleAndP = (&&&) **. uncurry (&&)

liftP2 :: (a -> b -> c) -> InfoP a -> InfoP b -> InfoP c
liftP2 op f g inf = f inf `op` g inf

andP, orP :: InfoP Bool -> InfoP Bool -> InfoP Bool
andP = liftP2 (&&)
orP  = liftP2 (||)

constP :: a -> InfoP a
constP = const

liftP' :: (a -> b -> c) -> InfoP a -> b -> InfoP c
-- liftP' op f k inf = liftP2 op f (constP k) inf
liftP' = liftP2 *. (. constP)

liftPath :: (FilePath -> a) -> InfoP a
-- liftPath f = f . pathP
liftPath = (. pathP)

myTest2 :: InfoP Bool
myTest2 = (liftPath takeExtension `equalP` ".cpp") `andP` (sizeP `greaterP` 131072)

infix 4 ==?
infixr 3 &&?
infix 4 >?

(==?) = equalP
(&&?) = andP
(>?)  = greaterP

myTest3 :: InfoP Bool
myTest3 = (liftPath takeExtension ==? ".cpp") &&? (sizeP >? 131072)

myTest4 :: InfoP Bool
myTest4 = liftPath takeExtension ==? ".cpp" &&? sizeP >? 131072

traverseIWrapper :: (Info -> Bool) -> (Info -> Bool) -> FilePath -> IO [Info]
traverseIWrapper p f = traverseI (predAndFilter p f)
  where
    predAndFilter :: (Info -> Bool) -> (Info -> Bool) -> [Info] -> [Info]
    predAndFilter p f = filter ((isDirectoryAndPred p &&& f) >>> uncurry (||))

    isDirectoryAndPred :: (Info -> Bool) -> Info -> Bool
    isDirectoryAndPred = (isDirectory &&&) *. uncurry (&&)
    -- isDirectoryAndPred pred = (isDirectory &&& pred) >>> uncurry (&&)
