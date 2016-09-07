import System.Environment (getArgs)
import Text.Printf (printf)

main :: IO ()
main = do
  [d] <- map read <$> getArgs
  printf "%f\n" $ mean [1..d]

mean :: [Double] -> Double
mean xs = s / fromIntegral n
  where
    (n, s)     = foldl k (0, 0) xs
    k (n, s) x = (n + 1, s + x)

{-
 - Now, instead of taking the sum of the list and retain all list elements
 - by taking the lenght of it at the end. We perform a left fold,
 - accumulating the intermediate sum and length values in a pair.
 -}
