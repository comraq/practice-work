> import Data.Char

----- Chapter 1 -----

1)

> double :: (Num a) => a -> a
> double =  (*2)

2)

= sum [x]
{ applying sum }

= x + sum []
{ applying sum }

= x + 0
= x

3)

> product' :: (Num a) => [a] -> a
> product' =  foldr (*) 1

4)

> qsort        :: (Ord a) => [a] -> [a]
> qsort []     =  []
> qsort (x:xs) =  qsort smaller ++ [x] ++ qsort larger
>   where smaller = [ a | a <- xs, a <= x ]
>         larger  = [ a | a <- xs, a > x ]
>
> qsortrev        :: (Ord a) => [a] -> [a]
> qsortrev []     =  []
> qsortrev (x:xs) =  qsortrev larger ++ [x] ++ qsortrev smaller
>   where smaller = [ a | a <- xs, a <= x ]
>         larger  = [ a | a <- xs, a > x ]

5)

Duplicate elements will be filtered out of the result list


----- Chapter 2 -----

1)

= (2 ^ 3) * 4
= (2 * 3) + (4 * 5)
= 2 * (3 * (4 ^ 5))

3)

> n :: Int
> n =  a `div` length xs
>   where a = 10
>         xs = [1, 2, 3, 4, 5]

4) 

> last' :: [a] -> a
> last' =  head . reverse
>
> last''    :: [a] -> a
> last'' xs =  head $ drop ((length xs) - 1) xs


5)

> init' :: [a] -> [a]
> init' =  reverse . tail . reverse
>
> init''    :: [a] -> [a]
> init'' xs =  take ((length xs) - 1) xs


----- Chapter 3 -----

1)

[Char]
(Char, Char, Char)
[(Bool, Char)]
([Bool], [Char])
[([a] -> [a])]

2)

second :: [a] -> a

swap :: (a, b) -> (b, a)

pair :: a -> b -> (a, b)

double :: (Num a) => a -> a

palindrome :: (Eq a) => [a] -> Bool

twice :: (a -> a) -> a -> a

4)

Since function signatures are polymorphic, all possible inputs must be
equated with all possible outputs to check for equality if instance of 'Eq'
class.

It is infeasible to exhaust all such possible combinations


----- Chapter 4 -----

1)

> halve    :: [a] -> ([a], [a])
> halve xs =  splitAt (length xs `div` 2) xs

2)

> safetail :: [a] -> [a]
> safetail xs = if null xs then [] else tail xs
>
> safetail' :: [a] -> [a]
> safetail' xs
>   | null xs   = []
>   | otherwise = tail xs
>
> safetail''        :: [a] -> [a]
> safetail'' []     =  []
> safetail'' (x:xs) =  xs

3)

> v               :: Bool -> Bool -> Bool
> False `v` False = False
> _ `v` _         = True
>
> v'            :: Bool -> Bool -> Bool
> False `v'` b  = b
> _ `v'` _      = True
>
> v''       :: Bool -> Bool -> Bool
> b `v''` c
>   | b /= c    = True
>   | otherwise = b

4)

> and     :: Bool -> Bool -> Bool
> and b c =  if b && c then True else False

5)

> and'     :: Bool -> Bool -> Bool
> and' c b =  if c then b else False

6)

> mult :: (Num a) => a -> a -> a -> a
> mult =  \x -> \y -> \z -> x * y * z


----- Chapter 5 -----

1)

> one :: (Integral a) => a
> one =  sum [ a * a | a <- [1..100] ]

2)

> replicate'     :: Int -> a -> [a]
> replicate' n x =  [ x | a <- [1..n] ]

3)

> pyths   :: Int -> [(Int, Int, Int)]
> pyths n =  [ (x, y, z) | x <- [1..n], y <- [1..n], z <- [1..n], (x*x) + (y*y) == z*z ]

4)

> factors   :: Int -> [Int]
> factors n =  [ x | x <- [1..n], n `mod` x == 0 ]
>
> perfects   :: Int -> [Int]
> perfects n =  [ x | x <- [1..n], x + x == sum (factors x) ]

5)

> ex5 :: [(Int, Int)]
> ex5 =  [(x, y) | x <- [1,2,3], y <- [4,5,6]]
>
> ex5' :: [(Int, Int)]
> ex5' =  concat [ [(x, y) | x <- [1,2,3] ] | y <- [4,5,6] ]

6)

> positions      :: (Eq a) => a -> [a] -> [Int]
> positions x xs =  [ i | (x',i) <- zip xs [0..n], x == x' ]
>   where n = length xs - 1
>
> find     :: (Eq a) => a -> [(a, b)] -> [b]
> find k t =  [ v | (k', v) <- t, k == k' ] 
>
> positions'      :: (Eq a) => a -> [a] -> [Int]
> positions' x xs =  find x $ zip xs [0..]

7)

> chisqr       :: [Float] -> [Float] -> Float
> chisqr os es =  sum [ ((o - e) ** 2) / e | (o, e) <- zip os es ]
>
> scalarproduct       :: (Num a) => [a] -> [a] -> a
> scalarproduct xs ys =  sum [ x * y | (x, y) <- zip xs ys ]

8)

> let2int            :: Char -> Char -> Int
> let2int baseChar c =  ord c - ord baseChar
>
> int2let            :: Char -> Int -> Char
> int2let baseChar n =  chr (ord baseChar + n)
>
> shift     :: Int -> Char -> Char
> shift n c
>   | isLower c = int2let 'a' $ (let2int 'a' c + n) `mod` 26
>   | isUpper c = int2let 'A' $ (let2int 'A' c + n) `mod` 26
>   | otherwise = c
