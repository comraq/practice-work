toDigitsRev :: (Integral a, Ord a) => a -> [a]
toDigitsRev x
  | x < 10 = [x]
  | otherwise = y : toDigitsRev remain
    where y      = x `mod` 10
          remain = div x 10

toDigits :: (Integral a, Ord a) => a -> [a]
toDigits = reverse . toDigitsRev

dblEveryOther :: (Num a) => [a] -> [a]
dblEveryOther = foldr (\x acc -> (if (length acc) `mod` 2 /= 0 then (x * 2) else x): acc ) []

sumDigits :: (Integral a, Ord a) => [a] -> a
sumDigits (x:[]) = x
sumDigits (x:xs)
  | x >= 10 = (x `mod` 10) + div x 10 + sumDigits xs
  | otherwise = x + sumDigits xs

validate :: (Integral a) => a -> Bool
validate = \x -> ((sumDigits . dblEveryOther . toDigits) x `mod` 10) == 0

type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi n a b c = hanoi (n-1) a c b ++ [(a, b)] ++ hanoi (n-1) c b a
