module Glob (namesMatching, module Utils) where

import Utils

import System.Directory ( doesDirectoryExist
                        , doesFileExist
                        , getCurrentDirectory
                        , getDirectoryContents
                        )
import System.FilePath ( dropTrailingPathSeparator
                       , splitFileName
                       , (</>)
                       )
import Control.Exception (handle)
import Control.Monad (forM)
import GlobRegex (matchesGlob')

isPattern :: String -> Bool
isPattern = any (`elem` "[*?")

namesMatching :: String -> IO [String]
namesMatching pat
  | not (isPattern pat) = do
    exists <- doesNameExist pat
    return $ exists ?>> [pat]
                    $ []

  | otherwise = do
      case splitFileName pat of
        ("", baseName)      -> do
          curDir <- getCurrentDirectory
          listMatches curDir baseName

        (dirName, baseName) -> do
          dirs <- isPattern dirName ?>> (namesMatching (dropTrailingPathSeparator dirName))
                                    $ return [dirName]

          let listDir = isPattern baseName ?>> listMatches
                                           $ listPlain

          pathNames <- forM dirs $ \dir -> do
                         baseNames <- listDir dir baseName
                         return $ map (dir </>) baseNames

          return $ concat pathNames

doesNameExist :: FilePath -> IO Bool
doesNameExist name = do
  fileExists <- doesFileExist name
  fileExists ?>> return True
             $ doesDirectoryExist name

listMatches :: FilePath -> String -> IO [String]
listMatches dirName pat = do
  dirName' <- null dirName ?>> getCurrentDirectory
                           $ return dirName

  handle ((const $ return []) :: IOError -> IO [String]) $ do
    names <- getDirectoryContents dirName'
    let names' = isHidden pat ?>> filter isHidden names
                              $ filter (not . isHidden) names
    return $ filter (`matchesGlob'` pat) names'

isHidden :: String -> Bool
isHidden ('.':_) = True
isHidden _       = False

listPlain = undefined

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
      -- getAllAbsDirNames  = ((>>=) . listDir) >>> (&&& getAbsDirNames) *. app

  in notPattern pat ?>> getMatchIfNotPat pat
                    $ case splitFileName pat of
                        ("", baseName)      -> handleDirNameEmpty baseName

                        (dirName, baseName) -> do
                          dirs <- getDirs dirName
                          -- pathNames <- forM dirs $ getAllAbsDirNames baseName
                          pathNames <- forM dirs $ ((listDir baseName >>> (>>=)) &&& getAbsDirNames) >>> app

                          return $ concat pathNames
