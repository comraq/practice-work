import GHC.Conc (numCapabilities)
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  putStrLn $ "command line arguments: " ++ show args
  putStrLn $ "number of cores: " ++ show numCapabilities

{-
 - Note that within a threaded runtime using multiple cores, the threads and
 - sharing data is more expensive than in a non-threaded runtime.
 -}
