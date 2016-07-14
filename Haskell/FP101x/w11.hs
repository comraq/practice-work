fibs :: [Integer]
fibs =  0 : 1 : [ x + y | (x, y) <- zip fibs (tail fibs) ]

fib :: Int -> Integer
fib =  (!!) fibs

largeFib :: Integer
largeFib =  head $ dropWhile (<=1000) fibs 

data Tree a = Leaf | Node (Tree a) a (Tree a)
  deriving Show

repeatTree   :: a -> Tree a
repeatTree x =  Node t x t
  where t = repeatTree x
