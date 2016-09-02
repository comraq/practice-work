module Prettify2 where

data Doc = Empty
         | Char Char
         | Text String
         | Line
         | Concat Doc Doc
         | Union Doc Doc
         deriving (Show, Eq)

empty :: Doc
empty = Empty

(<>) :: Doc -> Doc -> Doc
Empty <> y = y
x <> Empty = x
x <> y     = Concat x y

char :: Char -> Doc
char = Char

line :: Doc
line = Line

text :: String -> Doc
text "" = Empty
text s  = Text s

double :: Double -> Doc
double = text . show

fold :: (Doc -> Doc -> Doc) -> [Doc] -> Doc
-- fold f = foldr f empty
fold = (`foldr` empty)

hcat :: [Doc] -> Doc
hcat = fold (<>)

punctuate :: Doc -> [Doc] -> [Doc]
punctuate p []     = []
punctuate p [d]    = [d]
punctuate p (d:ds) = (d <> p) : punctuate p ds

instance Monoid Doc where
  mempty  = empty
  mappend = (<>)
