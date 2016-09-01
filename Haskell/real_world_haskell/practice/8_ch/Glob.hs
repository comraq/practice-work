module Glob
  ( namesMatching
  , namesMatching'
  , module Utils
  ) where

import Utils

import System.Directory ( doesDirectoryExist
                        , doesFileExist
                        , getCurrentDirectory
                        , getDirectoryContents
                        )
import System.FilePath ( dropTrailingPathSeparator
                       , splitFileName
                       , (</>)
                       , pathSeparator
                       )
import System.Posix.Files (fileExist)

import Control.Exception (handle)
import Control.Monad (forM)
import GlobRegex (matchesGlob', matchesGlobCase')
import GlobRegexEither (matchesGlob, matchesGlobCase)

isPattern :: String -> Bool
isPattern = any (`elem` "[*?")

namesMatching :: String -> IO [String]
namesMatching pat
  | not (isPattern pat) = do
    exists <- doesNameExist pat
    return $ if exists then [pat] else []

  | otherwise = do
      case splitFileName pat of
        ("", baseName)      -> do
          curDir <- getCurrentDirectory
          listMatches curDir baseName

        (dirName, baseName) -> do
          dirs <- if isPattern dirName then (namesMatching (dropTrailingPathSeparator dirName))
                                       else return [dirName]

          let listDir = if isPattern baseName then listMatches
                                              else listPlain

          pathNames <- forM dirs $ \dir -> do
                         baseNames <- listDir dir baseName
                         return $ map (dir </>) baseNames

          return $ concat pathNames

doesNameExist :: FilePath -> IO Bool
doesNameExist name = do
  fileExists <- doesFileExist name
  if fileExists then return True
                else doesDirectoryExist name

listMatches :: FilePath -> String -> IO [String]
listMatches dirName pat = do
  dirName' <- if null dirName then getCurrentDirectory
                              else return dirName

  handle ((const $ return []) :: IOError -> IO [String]) $ do
    names <- getDirectoryContents dirName'
    let names' = if isHidden pat then filter isHidden names
                                 else filter (not . isHidden) names
    return $ filter (`matchesGlob'` pat) names'

isHidden :: String -> Bool
isHidden ('.':_) = True
isHidden _       = False

listPlain :: FilePath -> String -> IO [String]
listPlain dirName baseName = do
  exists <- if null baseName then doesDirectoryExist dirName
                             else doesNameExist (dirName </> baseName)
  return $ if exists then [baseName] else []

doesNameExist' :: FilePath -> IO Bool
doesNameExist' =
  let checkExists = flip $ const (return True) <<? doesDirectoryExist
  in app . (((>>=) . doesFileExist) &&& checkExists)

listMatches' :: FilePath -> String -> IO [String]
listMatches' =
  let getCWDIfDirNameEmpty     = flip $ (const getCurrentDirectory) <<? return
      getDirName               = app . (getCWDIfDirNameEmpty &&& null)
      getDirExceptionHandler   = const $ return [] :: IOError -> IO [String]

      getNames                 = isHidden >>> id <<? not >>> (. isHidden) >>> filter
      matchAgainst             = app . first (.) . ((filter . (flip matchesGlob')) &&& getNames)
      getMatches'              = return .* matchAgainst
      getMatchesWithPatFromDir = curry $ (((=<<) . getMatches') *** getDirectoryContents) >>> app >>> handle getDirExceptionHandler

  in curry $ ((getDirName >>> (>>=)) *** getMatchesWithPatFromDir) >>> app

namesMatching' :: String -> IO [String]
namesMatching' pat =
  let notPattern         = not . isPattern
      getMatchIfNotPat   = (doesNameExist &&& ((:[]) >>> (<<? []) >>> (return .) >>> (=<<))) >>> swap >>> app
      handleDirNameEmpty = (getCurrentDirectory >>=) . flip listMatches
      getDirs            = ((uncurry (<<?) . ((namesMatching . dropTrailingPathSeparator) &&& (return . (:[])))) &&& isPattern) >>> app
      listDir            = app . ((uncurry (<<?) . (flip listMatches &&& flip listPlain)) &&& isPattern)
      getAbsDirNames     = (return .* map) . (</>)
      getAllAbsDirNames  = app .* (((>>=) .* listDir) >>> (&&& getAbsDirNames))
      concatAllDirNames  = (>>= (return . concat)) .* (flip forM . getAllAbsDirNames)

  in notPattern pat ?>> getMatchIfNotPat pat
                    $ case splitFileName pat of
                        ("", baseName)      -> handleDirNameEmpty baseName
                        (dirName, baseName) -> getDirs dirName >>= concatAllDirNames baseName

listPlain' :: FilePath -> String -> IO [String]
listPlain' =
  let existOrEmpty  = return .* uncurry (<<?) . ((:[]) &&& const [])
      checkIfExists = app .* uncurry (&&&) . (((. null) . (flip (?>>)) . doesDirectoryExist) &&& (doesNameExist .* (</>)))

  in (checkIfExists >>> (&&& existOrEmpty)) *. uncurry (>>=)

listMatchesCase' :: FilePath -> String -> IO [String]
listMatchesCase' =
  let getCWDIfDirNameEmpty     = flip $ (const getCurrentDirectory) <<? return
      getDirName               = app . (getCWDIfDirNameEmpty &&& null)
      getDirExceptionHandler   = const $ return [] :: IOError -> IO [String]

      getNames                 = isHidden >>> id <<? not >>> (. isHidden) >>> filter
      matchAgainst             = app . first (.) . ((filter . (flip $ matchesGlobCase' isPosix)) &&& getNames)
      getMatches'              = return .* matchAgainst
      getMatchesWithPatFromDir = curry $ (((=<<) . getMatches') *** getDirectoryContents) >>> app >>> handle getDirExceptionHandler

  in curry $ ((getDirName >>> (>>=)) *** getMatchesWithPatFromDir) >>> app

isPosix :: Bool
isPosix = pathSeparator == '/'

ifPosixDoesNameExist :: FilePath -> IO Bool
ifPosixDoesNameExist = fileExist
