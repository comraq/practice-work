import System.Environment (getArgs)
import Text.Printf (printf)
import Foldl (foldl'')

main :: IO ()
main = do
  [d] <- map read <$> getArgs
  printf "%f\n" $ mean [1..d]

mean :: [Double] -> Double
mean xs = s / fromIntegral n
  where
    (n, s)     = foldl'' k (0, 0) xs
    k (n, s) x = n `seq` s `seq` (n + 1, s + x)

{-
 - Same as C except we use our own implementation of strict foldl
 -}
