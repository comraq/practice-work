{-# LANGUAGE Arrows #-}

module Main where

import Utils
import Control.Monad
import qualified Control.Category as Cat
import Data.List
import Data.Maybe
import System.Random

main :: IO ()
main = do
  rng <- getStdGen
  interact $ unlines                -- Concatenate lines out to output
    . ("Welcome to Arrow Hangman":) -- Prepend a greeting to the output
    . concatMap snd . takeWhile fst -- Take the [String]s as long as the first element of the tuples is 'True'
    . runCircuit (hangman rng)      -- Process the input lazily
    . ("":)                         -- Act as if the user pressed "ENER" once at the start
    . lines                         -- Split input into lines

newtype Circuit a b = Circuit { unCircuit :: a -> (Circuit a b, b) }

instance Cat.Category Circuit where
  id  = Circuit $ \a -> (Cat.id, a)
  (.) = dot
    where
      (Circuit cir2) `dot` (Circuit cir1) = Circuit $ \a ->
        let (cir1', b) = cir1 a
            (cir2', c) = cir2 b
        in  (cir2' `dot` cir1', c)

instance Arrow Circuit where
  arr f = Circuit $ \a -> (arr f, f a)
  first (Circuit cir) = Circuit $ \(b, d) ->
    let (cir', c) = cir b
    in  (first cir', (c, d))

runCircuit :: Circuit a b -> [a] -> [b]
runCircuit _   []     = []
runCircuit cir (x:xs) =
  let (cir', x') = unCircuit cir x
  in  x' : runCircuit cir' xs

{-
 - Note:
 - > mapAccumL :: Traversable t => (a -> b -> (a, c)) -> a -> t b -> (a, t c)
 -}
runCircuit' :: Circuit a b -> [a] -> [b]
runCircuit' cir inputs = snd $ mapAccumL unCircuit cir inputs

-- | Accumulator that outputs a value determined by the supplied function
accum :: acc -> (a -> acc -> (b, acc)) -> Circuit a b
accum acc f = Circuit $ \input ->
  let (output, acc') = f input acc
  in  (accum acc' f, output)

-- | Accumulator that outputs the accumlator value
accum' :: b -> (a -> b -> b) -> Circuit a b
accum' base f = accum base $ \a b ->
  let b' = f a b
  in  (b', b')

total :: Num a => Circuit a a
total = accum' 0 (+)

-- Note: "const 1 ^>> total === arr (const 1) >>> total"
mean1 :: Fractional a => Circuit a a
mean1 = (total &&& (const 1 ^>> total)) >>> arr (uncurry (/))

mean2 :: Fractional a => Circuit a a
mean2 = proc value -> do
  t <- total -< value
  n <- total -< 1
  returnA -< t / n

{-
 - 'banana brackets' within arrow 'proc' block are optional but may help
 - resolve ambiguity
 -}

mean3 :: Fractional a => Circuit a a
mean3 = proc value -> do
  (t, n) <- (| (&&&) (total -< value) (total -< 1) |)
  returnA -< t / n

mean4 :: Fractional a => Circuit a a
mean4 = proc value -> do
  (t, n) <- (total -< value) &&& (total -< 1)
  returnA -< t / n

generator :: Random a => (a, a) -> StdGen -> Circuit () a
generator range rng = accum rng $ \() rng -> randomR range rng

dictionary :: [String]
dictionary = ["dog", "cat", "bird"]

pickWord :: StdGen -> Circuit () String
pickWord rng = proc () -> do
  idx <- generator (0, length dictionary - 1) rng -< ()
  returnA -< dictionary !! idx

-- Returns 'True' the first time, 'False' from second time on
oneShot :: Circuit () Bool
oneShot = accum True $ \_ acc -> (acc, False)

-- Previous 'acc' becomes the current output
delayedEcho :: a -> Circuit a a
delayedEcho acc = accum acc $ flip (,)

instance ArrowChoice Circuit where
  -- Only run the circuit if input is instance of 'Left'
  left orig@(Circuit cir) = Circuit $ \ebd -> case ebd of
    Left b  -> let (cir', c) = cir b
               in  (left cir', Left c)
    Right d -> (left orig, Right d)

getWord :: StdGen -> Circuit () String
getWord rng = proc () -> do
  {-
   - * first game loop:  run 'pickWord' and 'mPicked' becomes 'Just <word>'
   - * subsequent loops: 'mPicked' is 'Nothing'
   -
   - Note: the 'if' and 'case' inside an 'Arrow proc block' requires the an
   -       'ArrowChoice' instance
   -}
  firstTime <- oneShot -< ()
  mPicked   <- if firstTime
                 then do
                   picked  <- pickWord rng -< ()
                   returnA -< Just picked
                 else returnA -< Nothing

  -- An accumulator that retains the last 'Just' value
  mWord   <- accum' Nothing mplus -< mPicked
  returnA -< fromJust mWord

{-
 - Note:
 -   - no local name bindings, including the 'proc' argument is in scope
 -     between '<-' and '-<' except within and 'if' or 'case' block
 -   - this is because expressions within '<-' and '-<' are evaluated
 -     outside the scope of the 'proc' block and thus local bindings and
 -     proc argument does not exist
 -}

attempts :: Int
attempts = 5

livesLeft :: Int -> String
livesLeft hung = "Lives: ["
              ++ replicate (attempts - hung) '#'
              ++ replicate hung ' '
              ++ "]"

hangman :: StdGen -> Circuit String (Bool, [String])
hangman rng = proc userInput -> do
    word    <- getWord rng      -< ()
    let letter = listToMaybe userInput

    guessed <- updateGuess      -< (word, letter)
    hung    <- updateHung       -< (word, letter)
    end     <- delayedEcho True -< not (word == guessed || hung >= attempts)

    let result | word == guessed  = [guessed, "You won!"]
               | hung >= attempts = [guessed, livesLeft hung, "You died!"]
               | otherwise        = [guessed, livesLeft hung]
    returnA -< (end, result)
  where
    updateGuess :: Circuit (String, Maybe Char) String
    updateGuess = accum' (repeat '_') $ \(word, letter) guess ->
      case letter of
        Just l  -> map (\(w, g) -> if w == l then w else g) $ zip word guess
        Nothing -> take (length word) guess

    updateHung :: Circuit (String, Maybe Char) Int
    updateHung = proc (word, letter) ->
      total -< case letter of
        Just l  -> if l `elem` word then 0 else 1
        Nothing -> 0

instance ArrowLoop Circuit where
  loop (Circuit cir) = Circuit $ \b ->
    let (cir', (c, d)) = cir (b, d)
    in  (loop cir', c)

delay :: a -> Circuit a a
delay last = Circuit $ \this -> (delay this, last)

mean5 :: Fractional a => Circuit a a
mean5 = proc value -> do
  rec
      (lastT, lastN) <- delay (0, 0) -< (t, n)
      let (t, n) = (lastT + value, lastN + 1)
          mean   = t / n
  returnA -< mean

{-
 - * the 'rec' in a 'proc' block allows variables to be referenced in a
 -   cycle
 -   ie: variable bindings mutually depend on the values of each other
 -
 - * any variable defined in 'rec' that are forward referenced in 'rec' are
 -   looped around by passing them through the second tuple element of loop
 -   * thus, variable bindings and references/usages of these bindings can
 -     be in any order
 -   * note that the effects of the corresponding arrow statements are still
 -     executed in order
 - * any variables defined in 'rec' that are referenced from outside 'rec'
 -   are returned in the first tuple element of loop
 -
 - * Note: 'loop' (and therefore 'rec') simply binds variables, as it does
 -         not hold onto values and pass them back in the next invocation of the
 -         'Circuit'
 -}

{-
 - Note:
 -   * the 'Arrow' expression preceding '-<' cannot contain local
 -     bindings
 -   * however, arrow expressions preceding '-<<' allows local bindings but
 -     the arrow must be an instance of 'ArrowApply' typeclass
 -}
