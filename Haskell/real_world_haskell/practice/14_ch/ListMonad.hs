returnSingleton :: a -> [a]
returnSingleton = (:[])

comprehensive :: [a] -> [b] -> [(a, b)]
comprehensive xs ys = [ (x, y) | x <- xs, y <- ys ]

monadic :: [a] -> [b] -> [(a, b)]
monadic xs ys = do { x <- xs; y <- ys; return (x, y) }

blockyDo :: [a] -> [b] -> [(a, b)]
blockyDo xs ys = do
  x <- xs
  y <- ys
  return (x, y)

{-
 - Note that 'blockyDo' has implicitly encoded a double nested loop!
 - Where every line after 'x <- xs' is executed once for every 'x' in 'xs'
 - and likewise for every line after 'y <- ys' for every 'y' in 'ys'.
 -
 - This highlights the fact that the behaviour of a 'do block' for a piece of
 - monadic code cannot be predicted unless we known which monad it will
 - execute in.
 -}

blockyPlain :: [a] -> [b] -> [(a, b)]
blockyPlain xs ys =
  xs >>=
  \x -> ys >>=
  \y -> return (x, y)

blockyPlain_reloaded :: [a] -> [b] -> [(a, b)]
blockyPlain_reloaded xs ys =
  concat (map (\x ->
               concat (map (\y ->
                            return (x, y))
                       ys))
          xs)

guarded :: Bool -> [a] -> [a]
guarded True  xs = xs
guarded False _  = []

multiplyTo :: Int -> [(Int, Int)]
multiplyTo n = do
  x <- [1..n]
  y <- [x..n]
  guarded (x * y == n) $ return (x, y)

{-
 - Note that concat . map == (>>=) and calling concat on empty lists will
 - not concatenate any result, ie:
 -   concat [[1], [2]] == concat [[1], [2], []]
 -   -> True
 -}

robust :: [a] -> Maybe a
robust xs = do (_:x:_) <- Just xs
               return x
{-
 - Note that a failure in the pattern match in 'robust' calls the monad's
 - fail implementation.
 -
 - In the case of 'Maybe', fail == Nothing
 -}

wordCount :: IO ()
wordCount = print . length . words =<< getContents
