module GlobRegex
  ( globToRegex
  , matchesGlob
  ) where

-- pat = "(foo[a-z]*bar|quux)"
-- getAllMatches ("i foobarbar a quux" =~ pat) :: [(Int, Int)]
--
-- getAllTextMatches("good food" =~ ".ood") :: [String]

import Text.Regex.Posix ((=~))
import Control.Arrow

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

(?) :: Bool -> a -> a -> a
(?) b x y = if b then x else y

globToRegex' :: String -> String
globToRegex' = ('^':) . (++ "$") . globToRegexHelper

(.**) :: (b -> c) -> (a0 -> a1 -> b) -> a0 -> a1 -> c
(.**) = (.) . (.)

matchesGlob' :: FilePath -> String -> Bool
matchesGlob' = curry $ app . ((=~) *** globToRegex')
-- matchesGlob' = app .** curry ((=~) *** globToRegex')

escape' :: Char -> String
escape' =
  let regexChars   = "\\+()^$.{}]|"
      escapeIfTrue = flip (? ('\\' :)) id
      charToString = pure :: Char -> String

  in app . ((escapeIfTrue . (`elem` regexChars)) &&& charToString)

