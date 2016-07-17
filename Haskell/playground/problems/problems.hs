import Control.Monad
import Data.List

solveRPN :: String -> Maybe Float
solveRPN =  head . foldl' calc [] . words

calc              :: [Maybe Float] -> String -> [Maybe Float]
calc (x:y:xs) "*" =  (liftM2 (*) x y):xs
calc (x:y:xs) "+" =  (liftM2 (+) x y):xs
calc (x:y:xs) "-" =  (liftM2 (-) y x):xs
calc (x:y:xs) "/" =  (liftM2 (/) y x):xs
calc (x:y:xs) "^" =  (liftM2 (**) y x):xs
calc (x:xs)  "ln" =  (liftM log x):xs
calc xs     "sum" =  [fmap sum $ sequence xs]
calc xs numStr    =
  case reads numStr :: [(Float, String)] of
    []                 -> Nothing:xs
    [(floatNumber, _)] -> (Just floatNumber):xs

data Node = Node Road Road | End Road
data Road = Road Int Node
data Section = Section { getA :: Int
                       , getB :: Int
                       , getC :: Int
                       } deriving Show
type RoadSystem = [Section]

heathrowToLondon :: RoadSystem
heathrowToLondon =  [ Section 50 10 30
                    , Section 5 90 20
                    , Section 40 2 25
                    , Section 10 8 0
                    ]

data Label = A | B | C deriving Show
type Path = [ ( Label, Int ) ]

roadStep                                              :: (Path, Path, Int, Int) -> Section -> (Path, Path, Int, Int)
roadStep (pathA, pathB, timeA, timeB) (Section a b c) = 
  let toA    = timeA + a
      crossA = timeB + b + c
      toB    = timeB + b
      crossB = timeA + a + c
      newPA  = if toA < crossA then (A, a):pathA else (C, c):(B, b):pathB
      newPB  = if toB < crossB then (B, b):pathB else (C, c):(A, a):pathA
  in ( newPA
     , newPB
     , if toA < crossA then toA else crossA
     , if toB < crossB then toB else crossB
     )

optimalPath            :: RoadSystem -> Path
optimalPath roadSystem =
  let (pathA, pathB, timeA, timeB) = foldl' roadStep ([], [], 0, 0) roadSystem
  in if (timeA <= timeB)
       then reverse pathA
       else reverse pathB

groupsOf      :: Int -> [a] -> [[a]]
groupsOf 0 _  =  undefined
groupsOf _ [] =  []
groupsOf n xs =  take n xs : groupsOf n (drop n xs)

main = do
  contents <- getContents
  let threes     = groupsOf 3 (map read $ lines contents)
      roadSystem = map (\[a, b, c] -> Section a b c) threes
      path       = optimalPath roadSystem
      pathString = concat $ map (show . fst) path
      pathTime   = sum $ map snd path
  putStrLn $ "The best path to take is: " ++ pathString
  putStrLn $ "The time required: " ++ show pathTime
