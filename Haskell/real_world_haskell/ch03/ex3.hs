import qualified Data.List as L
import Tree

length' :: [a] -> Int
length' =  foldr (\_ x -> x + 1) 0

mean    :: (Num a, Integral a) => [a] -> Float
mean xs =  (fromIntegral $ sum xs) / (fromIntegral $ length xs)

palindrome    :: [a] -> [a]
palindrome xs =  xs ++ reverse xs

isPali    :: (Eq a) => [a] -> Bool
isPali xs =
  let (ys, zs) = splitAt (length xs `div` 2) xs
      checkEqual []            = True
      checkEqual ((y, z):rest) = if y == z then checkEqual rest else False
  in case odd $ length xs of
    True  -> checkEqual $ zip ys (reverse $ tail zs)
    False -> checkEqual $ zip ys (reverse zs)

cmpLen       :: [a] -> [b] -> Ordering
cmpLen xs ys
  | length xs > length ys = GT
  | length xs < length ys = LT
  | otherwise             = EQ

sortll :: [[a]] -> [[a]]
sortll =  L.sortBy cmpLen

intersperse   :: a -> [[a]] -> [a]
intersperse x =  init . foldr (\n acc -> n ++ (x:acc)) []

height              :: Tree a -> Int
height Empty        =  0
height (Node _ l r) =
  let greaterOf x y = if x > y then x else y
  in 1 + greaterOf (height l) (height r)

data Direction = LD | RD | SD
  deriving (Show)

type Point = (Double, Double)

slope                 :: Point -> Point -> Double
slope (x, y) (x', y') =  (y' - y) / (x' - x)

cmpSlope     :: Double -> Double -> Direction
cmpSlope x y
  | x > y     = RD
  | x < y     = LD
  | otherwise = SD

calcTurn       :: Point -> Point -> Point -> Direction
calcTurn a b c =  cmpSlope (slope b a) (slope c b)

calcTurns            :: [Point] -> [Direction]
calcTurns (a:b:c:ps) =  (calcTurn a b c) : calcTurns ps
calcTurns _          =  []

cmpByLowY                 :: Point -> Point -> Ordering
cmpByLowY (x, y) (x', y')
  | y < y'            = LT
  | y == y' && x < x' = LT
  | otherwise         = GT

extractLowestY        :: [Point] -> (Point, [Point])
extractLowestY (x:xs) =  checkAll [] x xs
  where checkAll ls x (r:rs) = case x `cmpByLowY` r of
          GT -> checkAll (x:ls) r rs
          LT -> checkAll (r:ls) x rs
        checkAll ls x [] = (x, ls)

-- y - y1 = m(x - x1)
-- 0 - y1 = m(x - x1)
--    -y1 = mx - mx1
-- mx1-y1 = mx
--      x = (m(x1) - y1) / m
xIntercept          :: Double -> Point -> Point
xIntercept m (x, y) =  ((m * x - y) / m, 0)

sortByDir         :: (Point, [Point]) -> (Point, [Point])
sortByDir (p, ps) =  (p, L.sortBy slopeWithX ps)
  where slopeWithX a b =
          let xa = xIntercept (slope p a) a
              xb = xIntercept (slope p b) b
          in case cmpSlope (slope p xa) (slope xb p) of
               RD -> LT
               LD -> GT
               SD -> EQ
     --case cmpSlope (slope p a) (slope b p) of 
     --     RD -> LT
     --     LD -> GT
     --     SD -> EQ

-- IM BAD AT MATH T_T
