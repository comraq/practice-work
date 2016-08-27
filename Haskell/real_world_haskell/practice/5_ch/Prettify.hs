module Prettify
    (
    -- * Constructors
      Doc

    -- * Basic combinators
    , (<>)
    , empty
    , char
    , text
    , line
    , (</>)

    -- * Derived combinators
    , double
    , fsep
    , docConcat
    , punctuate

    -- * Formatters
    , fill
    , nest

    -- * Renderers
    , compact
    , pretty
    ) where

data Doc = Empty
         | Char Char
         | Text String
         | Line
         | Concat Doc Doc
         | Union Doc Doc
           deriving (Show, Eq)

empty :: Doc
empty =  Empty

string :: String -> Doc
string = undefined

text :: String -> Doc
text "" = Empty
text s  = Text s

double :: Double -> Doc
double = text . show

char :: Char -> Doc
char = Char

line :: Doc
line = Line

softline :: Doc
softline = group line

docConcat :: [Doc] -> Doc
docConcat = fold (<>)

fold :: (Doc -> Doc -> Doc) -> [Doc] -> Doc
fold f = foldr f empty

fsep :: [Doc] -> Doc
fsep = fold (</>)

(</>) :: Doc -> Doc -> Doc
x </> y  = x <> softline <> y

group :: Doc -> Doc
group x = flatten x `Union` x

flatten :: Doc -> Doc
flatten (x `Concat` y) = flatten x `Concat` flatten y
flatten Line           = Char ' '
flatten (x `Union` _)  = flatten x
flatten other          = other

(<>) :: Doc -> Doc -> Doc
Empty <> y = y
x <> Empty = x
x <> y     = x `Concat` y

punctuate :: Doc -> [Doc] -> [Doc]
punctuate p []     = []
punctuate p [d]    = [d]
punctuate p (d:ds) = (d <> p) : punctuate p ds

compact :: Doc -> String
compact x = transform [x]
  where transform []     = ""
        transform (d:ds) = case d of
          Empty        -> transform ds
          Char c       -> c : transform ds
          Text s       -> s ++ transform ds
          Line         -> '\n' : transform ds
          a `Concat` b -> transform (a:b:ds)
          _ `Union` b  -> transform (b:ds)

pretty :: Int -> Doc -> String
pretty width x = best 0 [x]
  where best col (d:ds) = case d of
          Empty        -> best col ds
          Char c       -> c : best (col + 1) ds
          Text s       -> s ++ best (col + length s) ds
          Line         -> '\n' : best 0 ds
          a `Concat` b -> best col (a:b:ds)
          a `Union` b  -> nicest col (best col (a:ds))
                                     (best col (b:ds))
        best _ _        = ""
        nicest col a b
          | (width - least) `fits` a = a
          | otherwise                = b
          where least = min width col

fits :: Int -> String -> Bool
w `fits` _
  | w < 0         = False
w `fits` ""       = True
w `fits` ('\n':_) = True
w `fits` (c:cs)   = (w - 1) `fits` cs

fill :: Int -> Doc -> Doc
fill w d = docConcat $ init $ scanLines 0 [d <> Line]
  where scanLines col (d:ds) = case d of
          Empty        -> scanLines col ds
          Char c       -> Char c : scanLines (col + 1) ds
          Text s       -> Text s : scanLines (col + length s) ds
          Line         -> (padLine $ w - col) : Line : scanLines 0 ds
          a `Concat` b -> scanLines col (a:b:ds)
          a `Union` b  -> scanLines col (b:ds)
        scanLines _   _      = []
        padLine w = Text $ replicate w '#'

nest :: Int -> Doc -> Doc
nest i d = scanLines 0 [d]
  where scanLines col (d:ds) = case d of
          Empty        -> Empty <> scanLines col ds
          Char c       -> indentChar col (d:ds)
          Text s       -> Text s <> scanLines col ds
          Line         -> Line <> indentLine col <> scanLines col ds
          a `Concat` b -> scanLines col (a:b:ds)
          a `Union` b  -> scanLines col (a:ds) `Union` scanLines col (b:ds)
        scanLines _   _      = Empty
        indentChar n ((Char c):ds)
          | c `elem` "[{" = Char c <> scanLines (n + i) (Line:ds)
          | c `elem` "]}" = Line <> indentLine (n - i) <> Char c <> scanLines (n - i) ds
          | otherwise     = Char c <> scanLines n ds
        indentLine n = Text $ replicate n ' '
