module GlobRegex
  ( globToRegex'
  , matchesGlob'
  , matchesGlobCase'
  ) where

-- pat = "(foo[a-z]*bar|quux)"
-- getAllMatches ("i foobarbar a quux" =~ pat) :: [(Int, Int)]
--
-- getAllTextMatches("good food" =~ ".ood") :: [String]

import Text.Regex.Posix ((=~))
import Data.Char (toLower)
import System.FilePath (pathSeparator)
import Utils

globToRegex :: String -> String
globToRegex cs = '^' : globToRegexHelper cs ++ "$"

globToRegexHelper :: String -> String
globToRegexHelper ""             = ""
globToRegexHelper ('*':cs)       = ".*" ++ globToRegexHelper cs
globToRegexHelper ('?':cs)       = '.' : globToRegexHelper cs
globToRegexHelper ('[':'!':c:cs) = "[^" ++ c : charClass cs
globToRegexHelper ('[':c:cs)     = '[' : c : charClass cs
globToRegexHelper ('[':_)        = error "Unterminated Character Class!"
globToRegexHelper (c:cs)         = escape c ++ globToRegexHelper cs

globToRegexHelper2 :: String -> String
globToRegexHelper2 ""             = ""
globToRegexHelper2 ('*':'*':cs)   = ".*" ++ globToRegexHelper cs
globToRegexHelper2 ('*':cs)       = "[^" ++ pathSeparator:"]*" ++ globToRegexHelper cs
globToRegexHelper2 ('?':cs)       = '.' : globToRegexHelper cs
globToRegexHelper2 ('[':'!':c:cs) = "[^" ++ c : charClass cs
globToRegexHelper2 ('[':c:cs)     = '[' : c : charClass cs
globToRegexHelper2 ('[':_)        = error "Unterminated Character Class!"
globToRegexHelper2 (c:cs)         = escape c ++ globToRegexHelper cs

escape :: Char -> String
escape c
  | c `elem` regexChars = '\\' : [c]
  | otherwise           = [c]
  where regexChars = "\\+()^$.{}]|"

charClass :: String -> String
charClass (']':cs) = ']' : globToRegexHelper cs
charClass (c:cs)   = c : charClass cs
charClass []       = error "Unterminated Character Class!"

matchesGlob :: FilePath -> String -> Bool
name `matchesGlob` pat = name =~ globToRegex pat

matchesGlobCase :: Bool -> FilePath -> String -> Bool
matchesGlobCase caseSense name pat =
  let mapCase = if caseSense then id else (fmap toLower :: String -> String)
  in mapCase name =~ mapCase (globToRegex pat)

matchesGlobCase' :: Bool -> FilePath -> String -> Bool
matchesGlobCase' =
  let mapCase   = id <<? (fmap toLower :: String -> String)
      matchGlob = (id &&& globToRegex)
  in ((mapCase
     *. (const &&& flip const))
     *. uncurry (&&&))
     **. uncurry (=~)

globToRegex' :: String -> String
globToRegex' = ('^':) . (++ "$") . globToRegexHelper

matchesGlob' :: FilePath -> String -> Bool
matchesGlob' = curry $ app . ((=~) *** globToRegex')
-- matchesGlob' = app .* curry ((=~) *** globToRegex')

escape' :: Char -> String
escape' =
  let regexChars   = "\\+()^$.{}]|"
      escapeIfTrue = flip (?>> ('\\' :)) id
      charToString = pure :: Char -> String
  in app . ((escapeIfTrue . (`elem` regexChars)) &&& charToString)
