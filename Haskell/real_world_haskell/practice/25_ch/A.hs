import System.Environment (getArgs)
import Text.Printf (printf)

main :: IO ()
main = do
  [d] <- map read <$> getArgs
  printf "%f\n" $ mean [1..d]

mean :: [Double] -> Double
mean xs = sum xs / fromIntegral (length xs)

{-
 - Since 'sum xs' forces the evaluation of every element in the list. Heap
 - memory is being allocated for all these nodes. However, these nodes
 - cannot be freed until 'length xs' is evaluated, thus the Haskell garbage
 - collector cannot free any of the memory occupied by the after processing
 - each element during the sum function.
 -}
