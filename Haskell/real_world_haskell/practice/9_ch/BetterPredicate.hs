import Utils

import Control.Monad (filterM)
import System.Directory
  ( Permissions(..)
  , getModificationTime
  , getPermissions
  )
import Data.Time (UTCTime(..))
import System.FilePath (takeExtension)
import Control.Exception (bracket, handle)
import System.IO (IOMode(..), hClose, hFileSize, openFile)

import RecursiveContents (getRecursiveContents, getRecursiveContents')

type Predicate =  FilePath      -- path to directory entry
               -> Permissions   -- permissions
               -> Maybe Integer -- file size (Nothing if not file)
               -> UTCTime     -- last modified time
               -> Bool

-- Note that 'Predicate' is a function of return type 'Bool' and not 'IO Bool'
-- ie: the 'Predicate' function is pure

{-
 - Order for bracket and handle is important as we want bracket to be the
 - inner function to first encounter any exeptions, so it can call the
 - release function
 -}
getFileSize :: FilePath -> IO (Maybe Integer)
getFileSize path = handle ((\_ -> return Nothing) :: IOError -> IO (Maybe Integer)) $
  bracket (openFile path ReadMode) hClose $ \h -> do
    size <- hFileSize h
    return $ Just size

getFileSize' :: FilePath -> IO (Maybe Integer)
getFileSize' =
  let exceptionHandler = (const $ return Nothing) :: IOError -> IO (Maybe Integer)
      useFunction      = hFileSize >>> (>>= (return . Just))
      releaseFunction  = hClose
      acquireFunction  = (`openFile` ReadMode)
      getSize          = flip (`bracket` releaseFunction) useFunction

  in acquireFunction >>> getSize >>> handle exceptionHandler


betterFind :: Predicate -> FilePath -> IO [FilePath]
betterFind p path = getRecursiveContents path >>= filterM check
  where check name = do
          perms    <- getPermissions name
          size     <- getFileSize name
          modified <- getModificationTime name
          return $ p name perms size modified

-- Note: The pointfree versions of 'simpleFileSize' and 'safeFileSize' may not
--       work due to closing file handle before getting the size with 'hFileSize'

simpleFileSize :: FilePath -> IO Integer
simpleFileSize path = do
  h    <- openFile path ReadMode
  size <- hFileSize h
  hClose h
  return size

simpleFileSize' :: FilePath -> IO Integer
simpleFileSize' =
  let getSize          = (hFileSize &&& id)
      closeFile        = second hClose
      returnFileSize   = fst >>> (>>= return)
      openFileFromPath = flip openFile ReadMode

  in openFileFromPath
     >>> (>>= (
     getSize
     >>> closeFile
     >>> returnFileSize))

{-
 - Note that 'saferFileSize' may still crash the program due to 'hFileSize'
 - failing (throwing an exception) and thus not closing the file handle
 - properly.
 -
 - The file handle may not be closed by Haskell's garbage collection in time
 - before all file handles are used up. At which case the program will still
 - crash.
 -}

saferFileSize :: FilePath -> IO (Maybe Integer)
saferFileSize path = handle ((\_ -> return Nothing) :: IOError -> IO (Maybe Integer)) $ do
  h    <- openFile path ReadMode
  size <- hFileSize h
  hClose h
  return $ Just size

saferFileSize' :: FilePath -> IO (Maybe Integer)
saferFileSize' =
  let exceptionHandler = (const $ return Nothing) :: IOError -> IO (Maybe Integer)
      openFileFrom     = (`openFile` ReadMode)
      getSize          = (hFileSize &&& id)
      closeFile        = second hClose
      returnFileSize   = fst >>> (>>= (return . Just))

  in handle exceptionHandler . (
     openFileFrom
     >>> (>>= (
     getSize
     >>> closeFile
     >>> returnFileSize)))

myTest :: Predicate
myTest path _ (Just size) _ = takeExtension path == ".cpp" && size > 131072
myTest _    _ _           _ = False

type InfoP a =  FilePath      -- path to directory entry
             -> Permissions   -- permissions
             -> Maybe Integer -- file size (Nothing if not file)
             -> UTCTime     -- last modified
             -> a

pathP :: InfoP FilePath
pathP path _ _ _ = path

sizeP :: InfoP Integer
sizeP _ _ (Just size) _ = size
sizeP _ _ Nothing     _ = -1

equalP :: Eq a => InfoP a -> a -> InfoP Bool
equalP f k w x y z = f w x y z == k

liftP :: (a -> b -> c) -> InfoP a -> b -> InfoP c
liftP q f k w x y z = f w x y z `q` k

greaterP :: Ord a => InfoP a -> a -> InfoP Bool
greaterP = liftP (>)

lesserP :: Ord a => InfoP a -> a -> InfoP Bool
lesserP  = liftP (<)

simpleAndP :: InfoP Bool -> InfoP Bool -> InfoP Bool
simpleAndP f g w x y z = f w x y z && g w x y z

liftP2 :: (a -> b -> c) -> InfoP a -> InfoP b -> InfoP c
liftP2 q f g w x y z = f w x y z `q` g w x y z

andP :: InfoP Bool -> InfoP Bool -> InfoP Bool
andP = liftP2 (&&)

orP :: InfoP Bool -> InfoP Bool -> InfoP Bool
orP = liftP2 (||)

constP :: a -> InfoP a
constP k _ _ _ _ = k

liftP' :: (a -> b -> c) -> InfoP a -> b -> InfoP c
liftP' q f k w x y z = f w x y z `q` constP k w x y z

liftPath :: (FilePath -> a) -> InfoP a
liftPath f w _ _ _ = f w

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
