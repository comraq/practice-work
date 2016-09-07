import System.Environment (getArgs)
import Text.Printf (printf)
import Data.List (foldl')

main :: IO ()
main = do
  [d] <- map read <$> getArgs
  printf "%f\n" $ mean [1..d]

mean :: [Double] -> Double
mean xs = s / fromIntegral n
  where
    (n, s)     = foldl' k (0, 0) xs
    k (n, s) x = (n + 1, s + x)

{-
 - Same as B except we avoid the space leak of 'foldl' with 'foldl''
 -}
