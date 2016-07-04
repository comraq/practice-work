module Lab3 where

-----------------------------------------------------------------------------------------------------------------------------
-- LIST COMPREHENSIONS
------------------------------------------------------------------------------------------------------------------------------

-- ===================================
-- Ex. 0 - 2
-- ===================================

evens :: [Integer] -> [Integer]
evens = filter even

-- ===================================
-- Ex. 3 - 4 
-- ===================================

-- complete the following line with the correct type signature for this function
squares :: Integer -> [Integer]
squares n = [ x ^ 2 | x <- [ 1..n ] ]

sumSquares :: Integer -> Integer
sumSquares n = sum (squares n)

-- ===================================
-- Ex. 5 - 7
-- ===================================

-- complete the following line with the correct type signature for this function
squares' :: Integer -> Integer -> [Integer]
squares' m n = [ x ^ 2 | x <- [ n + 1 .. n + m ] ]

sumSquares' :: Integer -> Integer
sumSquares' x = sum . uncurry squares' $ (x, x)

-- ===================================
-- Ex. 8
-- ===================================

coords :: Integer -> Integer -> [(Integer,Integer)]
coords m n = [ (a, b) | a <- [ 0 .. m ], b <- [ 0 .. n ] ]
