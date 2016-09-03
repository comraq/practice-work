import Control.Monad (liftM)
import Control.Monad.State

import System.Random

rand :: IO Int
rand = getStdRandom $ randomR (0, maxBound)

twoBadRandoms :: RandomGen g => g -> (Int, Int)
twoBadRandoms gen = (fst $ random gen, fst $ random gen)

twoGoodRandoms :: RandomGen g => g -> ((Int, Int), g)
twoGoodRandoms gen = let (a, gen')  = random gen
                         (b, gen'') = random gen'
                     in ((a, b), gen'')

type RandomState a = State StdGen a

getRandom :: Random a => RandomState a
getRandom = get >>= \gen ->
  let (val, gen') = random gen
  in  put gen' >> return val

getRandom' :: Random a => RandomState a
getRandom' = state random

{-
 - Running the state monad
 - > 'runState' returns both the result and the final state
 - > 'evalState' returns only the result, throwing away the final state
 - > 'execState' throws the result away, returning only the final state
 -
 - Note: 'evalState' and 'execState' are simply compositions of 'fst' and
 -       'snd' with 'runState'
 -}

getRandomDo :: Random a => RandomState a
getRandomDo = do
  gen <- get
  let (val, gen') = random gen
  put gen'
  return val

getTwoRandoms :: Random a => RandomState (a, a)
getTwoRandoms = liftM2 (,) getRandom getRandom

runTwoRandoms :: IO (Int, Int)
runTwoRandoms = do
  oldState <- getStdGen
  let (result, newState) = runState getTwoRandoms oldState
  setStdGen newState
  return result

data CountedRandom = CountedRandom {
  crGen   :: StdGen,
  crCount :: Int
}

type CRState = State CountedRandom

getCountedRandom :: Random a => CRState a
getCountedRandom = do
  st <- get
  let (val, gen') = random $ crGen st
  put CountedRandom { crGen = gen', crCount = crCount st + 1 }
  return val

getCount :: CRState Int
getCount = crCount `liftM` get

putCount :: Int -> CRState ()
putCount a = do
  st <- get
  put st { crCount = a }

putCountModify :: Int -> CRState ()
putCountModify a = modify $ \st -> st { crCount = a }

class Functor m => AltMonad m where
  joinAlt   :: m (m a) -> m a
  returnAlt :: a -> m a

join' :: Monad m => m (m a) -> m a
join' x = x >>= id
