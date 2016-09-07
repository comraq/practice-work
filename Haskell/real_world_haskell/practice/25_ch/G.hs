import System.Environment (getArgs)
import Text.Printf (printf)
import Data.List (foldl')

{-
 - An alternative to enforcing strictness within functions is to declare
 - 'strict' datatypes via the '!' ("bang" character)
 -}
data Pair a b = Pair !a !b

main :: IO ()
main = do
  [d] <- map read <$> getArgs
  printf "%f\n" $ mean [1..d]

mean :: [Double] -> Double
mean xs = s / fromIntegral n
  where
    Pair n s      = foldl' k (Pair 0 0) xs
    k (Pair n s) x = Pair (n + 1) (s + x)
