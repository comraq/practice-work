import System.Random
import Data.List
import Control.Monad(when)

threeCoins     :: StdGen -> (Bool, Bool, Bool)
threeCoins gen = (coin1, coin2, coin3)
  where (coin1, gen')  = random gen
        (coin2, gen'') = random gen'
        (coin3, _)     = random gen''

randoms'     :: (RandomGen g, Random a) => g -> [a]  
randoms' gen = let (value, newGen) = random gen in value:randoms' newGen  

finiteRandoms       :: (RandomGen g, Random a, Eq n, Num n) => n -> g -> ([a], g)
finiteRandoms 0 gen =  ([], gen)
finiteRandoms n gen =  (v:rest, gen'')
  where (v, gen')     = random gen
        (rest, gen'') = finiteRandoms (n - 1) gen'

main1 = do
  gen <- getStdGen
  putStr $ take 20 (randomRs ('a', 'z') gen)

main2 = do
  gen <- getStdGen
  let rChars = randomRs ('a', 'z') gen
      (first20, rest) = splitAt 20 rChars
      (second20, _)   = splitAt 20 rest
  putStrLn first20
  putStr second20

main = do
  gen <- getStdGen
  askForNumber gen

askForNumber     :: StdGen -> IO ()
askForNumber gen =  do
  let (rNum, newGen) = randomR (1, 10) gen :: (Int, StdGen)
  putStrLn "Guess a number from 1 to 10: "
  numStr <- getLine
  when (not $ null numStr) $ do
    case reads numStr :: [(Int, String)] of
      []               -> putStrLn "Please input a number between 1 to 10!"
      [(number, rest)] -> if rNum == number
                            then putStrLn "You guessed correctly!"
                            else putStrLn $ "Sorry, the number was " ++ show rNum
    askForNumber newGen
