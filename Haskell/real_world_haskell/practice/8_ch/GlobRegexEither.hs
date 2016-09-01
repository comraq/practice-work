module GlobRegexEither
  ( globToRegex
  , matchesGlob
  , matchesGlobCase
  ) where

import Text.Regex.Posix ((=~))
import System.FilePath (pathSeparator)
import Data.Char (toLower)

type GlobError = String

globToRegex :: String -> Either GlobError String
globToRegex = fmap (('^':) . (++ "$")) . globToRegexHelper

globToRegexHelper :: String -> Either GlobError String
globToRegexHelper ""             = Right ""
globToRegexHelper ('*':'*':cs)   = (".*" ++) <$> globToRegexHelper cs
globToRegexHelper ('*':cs)       = (("[^" ++ pathSeparator:"]*") ++) <$> globToRegexHelper cs
globToRegexHelper ('?':cs)       = ('.' :) <$> globToRegexHelper cs
globToRegexHelper ('[':'!':c:cs) = (("[^" ++ [c]) ++) <$> charClass cs
globToRegexHelper ('[':c:cs)     = (("[" ++ [c]) ++) <$> charClass cs
globToRegexHelper ('[':_)        = Left "Unterminated Character Class!"
globToRegexHelper (c:cs)         = (escape c ++) <$> globToRegexHelper cs

charClass :: String -> Either GlobError String
charClass (']':cs) = (']' :) <$> globToRegexHelper cs
charClass (c:cs)   = (c :) <$> charClass cs
charClass []       = Left "Unterminated Character Class!"

escape :: Char -> String
escape c
  | c `elem` regexChars = '\\' : [c]
  | otherwise           = [c]
  where regexChars = "\\+()^$.{}]|"

matchesGlob :: FilePath -> String -> Bool
name `matchesGlob` pat = case globToRegex pat of
  Left _      -> False
  Right regex -> name =~ regex

matchesGlobCase :: Bool -> FilePath -> String -> Bool
matchesGlobCase caseSense name pat =
  let mapCase = if caseSense then id else (fmap toLower :: String -> String)
  in case globToRegex pat of
    Left _      -> False
    Right regex -> mapCase name =~ mapCase regex
