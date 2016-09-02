import Prettify2
import Test.QuickCheck
import QC
import Control.Monad(forM_)

options :: Args
options = stdArgs { maxSuccess = 200, maxSize = 200 }

type Run = Args -> IO ()

run :: Testable prop => prop -> Run
run = flip quickCheckWith

runTests :: String -> Args -> [Run] -> IO ()
runTests name opts tests =
  putStrLn ("Running " ++ name ++ " tests:")
  >> forM_ tests (\rn -> rn opts)

main :: IO ()
main = do
  runTests "simple" options
    [ run prop_empty_id
    , run prop_char
    , run prop_text
    , run prop_line
    , run prop_double
    ]

  runTests "complex" options
    [ run prop_hcat
    , run prop_punctuate'
    ]
