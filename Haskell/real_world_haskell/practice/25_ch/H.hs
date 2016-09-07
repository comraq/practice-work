import System.Environment (getArgs)
import Text.Printf (printf)
import Data.List (foldl')

data Pair = Pair !Int !Double

main :: IO ()
main = do
  [d] <- map read <$> getArgs
  printf "%f\n" $ mean [1..d]

mean :: [Double] -> Double
mean xs = s / fromIntegral n
  where
    Pair n s      = foldl' k (Pair 0 0) xs
    k (Pair n s) x = Pair (n + 1) (s + x)
