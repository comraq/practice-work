module GlobNoRegex (matchesGlob) where

matchesGlob :: FilePath -> String -> Bool
matchesGlob name     ""        = null name
matchesGlob (c:name) ('?':pat) = matchesGlob name pat
matchesGlob name     ('*':pat) = matchStar name pat
matchesGlob name     ('[':pat) = matchCharClass name pat
matchesGlob (c:name) (p:pat)   = c == p && matchesGlob name pat
matchesGlob ""       _         = False

matchStar :: FilePath -> String -> Bool
matchStar ""   pat = matchesGlob "" pat
matchStar name pat = matchesGlob name pat || matchStar (tail name) pat

matchCharClass :: FilePath -> String -> Bool
matchCharClass name     pat
  | ']' `notElem` pat = error "Unterminated Character Class!"
matchCharClass (c:name) (p:pat)   =
  let (charClass, rest) = span (/= ']') pat
      checkCharClass    = if p == '!' then notElem else elem
  in checkCharClass c charClass && matchesGlob name (tail rest)

