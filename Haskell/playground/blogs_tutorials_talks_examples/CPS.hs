{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import Control.Monad (when)
import Control.Monad.Cont
import Control.Monad.State
import Data.Char (intToDigit, digitToInt)

add :: Int -> Int -> Int
add x y = x + y

square :: Int -> Int
square x = x * x

pythagoras :: Int -> Int -> Int
pythagoras x y = add (square x) (square y)

{-
 - We assume CPS versions of the add and square primitives.
 -
 - Note: the actual definitions of add_cps and square_cps are not in CPS
 -       form, but only the correct type
 -}
add_cps :: Int -> Int -> ((Int -> r) -> r)
add_cps x y = \k -> k (add x y)

square_cps :: Int -> ((Int -> r) -> r)
square_cps x = \k -> k (square x)

pythagoras_cps :: Int -> Int -> ((Int -> r) -> r)
pythagoras_cps x y = \k ->
  square_cps x $ \x_squared ->
  square_cps y $ \y_squared ->
  add_cps x_squared y_squared $ k

{-
 - Explanation of 'pythagoras_cps':
 - 1) square x and throw the result into the (\x_squared -> ...) continuation
 - 2) square y and throw the result into the (\y_squared -> ...) continuation
 - 3) add 'x_squared' and 'y_squared' and throw the result into the top
 -    level/program continuation 'k'
 -    (note: 'k' is the continuation passed to 'pythagoras_cps')
 -}

thrice :: (a -> a) -> a -> a
thrice f x = f (f (f x))

{-
 - Since 'thrice' is a higher order function, the function argument it takes
 - must also be in CPS style
 -}
thrice_cps :: (a -> ((a -> r) -> r)) -> a -> ((a -> r) -> r)
thrice_cps f_cps x = \k ->
  f_cps x   $ \fx  ->
  f_cps fx  $ \ffx ->
  f_cps ffx $ k

-- composing CPS styled functions
chainCPS :: ((a -> r) -> r) -> (a -> ((b -> r) -> r)) -> ((b -> r) -> r)
chainCPS s f = \k -> s (\x -> f x k)

{-
 - Note the similarity between 'chainCPS' and '>>='. Suppose:
 -   Monad m => m a === (a -> r) -> r
 -              m b === (b -> r) -> r
 -
 - chainCPS ::            ((a -> r) -> r) -> (a -> ((b -> r) -> r)) -> ((b -> r) -> r)
 - (>>=)    :: Monad m => m a             -> (a -> m b)             -> m b
 -
 - Note 'flip ($)'
 -   flip ($) ::             a -> ((a -> r) -> r)
 -   return   :: Monad m  => a -> m a

newtype Cont r a = Cont { runCont :: (a -> r) -> r }

instance Monad (Cont r) where
  return  = Cont . flip ($)
  s >>= f = Cont $ \c -> runCont s $ \x -> runCont (f x) c

instance Applicative (Cont r) where
  pure    = return
  f <*> a = f >>= \f' -> fmap f' a

instance Functor (Cont r) where
  fmap f a = a >>= return . f

 -}

add_cont :: Int -> Int -> Cont r Int
add_cont x y = return $ add x y

square_cont :: Int -> Cont r Int
square_cont x = return $ square x

pythagoras_cont :: Int -> Int -> Cont r Int
pythagoras_cont x y = do
  x_squared <- square_cont x
  y_squared <- square_cont y
  add_cont x_squared y_squared

{-
 - callCC :: ((a -> ContT r m b) -> ContT r m a) -> ContT r m a
 -
 - callCC "call with current continuation", which is 'k' in the 'squareCCC'
 - example.
 -}
squareCCC :: Int -> Cont r Int
squareCCC n = callCC $ \k -> k (n ^ 2)

{-
 - The argument passed to 'callCC' is a function, whose result is a
 - suspended computation (general type 'Cont r a') which we will refer to as
 - "the 'callCC' computation".
 -
 - In principle, the 'callCC' computation is what the whole 'callCC'
 - expressiojn evaluates to. The caveat, and what makes 'callCC' so special,
 - is due to 'k', the sole argument to the function argument passed to
 - 'callCC'. 'k' is a function which acts as an "eject button", as calling
 - it anywhere will lead to the value passed to it being made into a
 - suspended computation, which is then inserted into the control flow at
 - the point of the 'callCC' invocation. This happens unconditionally; in
 - particular, whatever follows a 'k' invocation in the 'callCC' computation
 - is summarily discarded.
 -
 - From another perspective, 'k' captures the "rest of the computation"
 - following the 'callCC'. Calling 'k' throws a value into the continuation
 - at that particular point.
 -}

{-
 - If the result of the foo computation is greater than 20, then foo returns
 - from the 'callCC' computation (and, in this case, from the whole
 - function) immediately, throwing the string "over twenty" into the
 - continuation that will be passed to 'foo'. If not, then four is
 - subtracted from the previous computation, 'show' it and throw the new
 - result into the continuation.
 -
 - Remarkably, 'k' here is used just like the 'return' statement from an
 - imperative language, that immediately exits the function. Yet this being
 - in Haskell, 'k' is just an ordinary first-class function, so 'k' can be
 - passed to other functions like 'when', stored in a 'Reader' and etc...
 -}
foo :: Int -> Cont r String
foo x = callCC $ \k -> do
  let y = x ^ 2 + 3
  when (y > 20) $ k "over twenty"
  return . show $ y - 4

bar :: Char -> String -> Cont r Int
bar c s = do
  msg <- callCC $ \k -> do
    let s0 = c : s
    when (s0 == "hello") $ k "They say hello."
    let s1 = show s0
    return $ "They appear to be saying " ++ s1

  return $ length msg

{-
 - When calling 'k' with a value, the entire 'callCC' call takes that value.
 - In effect, that makes 'k' a lot like an 'goto' statement in other
 - languages.
 -
 - When calling 'k' in 'bar', it pops the execution out to where 'callCC'
 - was first used, ie: "msg <- callCC $ ...".
 - No more of the argument to 'callCC' (the inner do-block) is executed.
 -
 - Hence, in the 'quux' example, the last "return 25" line is effectively
 - useless.
 -}
quux :: Cont r Int
quux = callCC $ \k -> do
  let n = 5
  k n
  return 25

{-
 - callCC :: ((a -> ContT r m b) -> ContT r m a) -> ContT r m a
 -
 - Note that the overall result type and the result type of the function
 - argument to 'callCC' have to be the same 'Cont r a'. This is because, in
 - the absence of calling 'k', the continuation returned by 'callCC' and
 - result when 'callCC's' function argument is invoked should be of the same
 - type.
 -
 - As for 'k', 'k' must take an argument that matches the type of the
 - continuation (ie: if the resulting Continuation is of type 'Cont r a',
 - then k's argument must be of type 'a'). However, k's result type can be
 - arbitrary, as the result type continuation will take its own continuation
 - function (and that's where the type matters, but irrelevant for the
 - current continuation).
 -
 - Example, the following would result in a type error:
 -   quux :: Cont r Int
 -   quux = callCC $ \k -> do
 -     let n = 5
 -     when True $ k n
 -     k 25
 -
 - The use of 'when True $ k n' indicates that 'k' must be of type
 - 'Cont r ()', however the last line 'k 25' suggests that the current
 - continuation's type is also 'Cont r ()', which is not the case as the
 - type signature of 'quux' is 'Cont r Int' (and thus 'callCC's' function
 - argument must return a continuation of type 'Cont r Int' as well).
 -
 - Note, 'callCC' is implemented as follows:
 -   callCC f = cont $ \h -> runCont (f (\a -> cont $ \_ -> h a)) h
 -
 -   * 'k' of type "(a -> ContT r m b)" is the "(\a -> cont $ \_ -> h a)"
 -   * 'a' is the 'a' in "(a -> ContT r m b)"
 -   * the irrelevant type 'b' matches the ignored argument in "\_ -> ..."
 -   * 'h' is the continuation function that evaluates 'a' to 'r' in the
 -     repeated 'ContT r m a'
 -}

{-
 - We use the continuation monad to perform "escapes" from code blocks.
 - This function implements a complicated control structure to process
 - numbers:
 -
 - Input (n)     Output                    List Shown
 - =========     ======                    ==========
 - 0-9           n                         none
 - 10-199        number of digits in (n/2) digits of (n/2)
 - 200-19999     n                         digits of (n/2)
 - 20000-1999999 (n/2) backwards           none
 - >= 2000000    sum of digits of (n/2)    digits of (n/2)
-}

fun :: Int -> String
fun n = (`runCont` id) $ do
  str <- callCC $ \exit1 -> do                      -- define "exit1"
    when (n < 10) (exit1 $ show n)
    let ns = map digitToInt (show $ n `div` 2)
    n' <- callCC $ \exit2 -> do                     -- define "exit2"
      when (length ns < 3) (exit2 $ length ns)
      when (length ns < 5) (exit2 n)
      when (length ns < 7) $ do
        let ns' = map intToDigit $ reverse ns
        exit1 $ dropWhile (=='0') ns'               -- escape 2 levels
      return $ sum ns
    return $ "(ns = " ++ show ns ++ ") " ++ show n'
  return $ "Answer: " ++ str

{-
 - Continuations can also be used to model exceptions, managing explicit
 - control over control flow.
 -
 - Example to run the following 'divExcpt':
 -   > runCont (divExcpt 10 2 error) id --> 5
 -   > runCont (divExcpt 10 0 error) id --> *** Exception: Denominator 0
 -}
divExcpt :: Int -> Int -> (String -> Cont r Int) -> Cont r Int
divExcpt x y handler = callCC $ \ok -> do
  err <- callCC $ \notOk -> do
    when (y == 0) $ notOk "Denominator 0"
    ok $ x `div` y
  handler err

{-
 - A more general approach to handling exceptions can be seen with the
 - following 'tryCont'.
 -
 - 'tryCont' takes a computation as the first paramemter (more precisely, a
 - function which takes an error-throwing function and results in the
 - computation) and an error handler as the second parameter.
 -}
tryCont :: MonadCont m => ((err -> m a) -> m a) -> (err -> m a) -> m a
tryCont c h = callCC $ \ok -> do
  err <- callCC $ \notOk -> do
    x <- c notOk
    ok x
  h err

data SqrtException = LessThanZero deriving (Show, Eq)

{-
 - Note the 'throw' argument to 'sqrtIO' is the 'notOk' binding in 'tryCont'
 - when called as shown in 'main'
 -}
sqrtIO :: (SqrtException -> ContT r IO ()) -> ContT r IO ()
sqrtIO throw = do
  ln <- liftIO $ putStr "Enter a number to sqrt: " >> readLn
  when (ln < 0) (throw LessThanZero)
  liftIO . print $ sqrt ln

main :: IO ()
main = runContT (tryCont sqrtIO (lift . print)) return

{-
 - The 'CoroutineT' monad is just 'ContT' stacked with a 'StateT' containing
 - the suspended coroutines
 -}
newtype CoroutineT r m a = CoroutineT {
  runCoroutineT' :: ContT r (StateT [CoroutineT r m ()] m) a
} deriving (Functor, Applicative, Monad, MonadCont, MonadIO)


-- Helper functions used to manipulate the coroutine queue

getCCs :: Monad m => CoroutineT r m [CoroutineT r m ()]
getCCs = CoroutineT $ lift get

putCCs :: Monad m => [CoroutineT r m ()] -> CoroutineT r m ()
putCCs = CoroutineT . lift . put

-- Push and Pop coroutines to the queue
dequeue :: Monad m => CoroutineT r m ()
dequeue = do
  current_ccs <- getCCs
  case current_ccs of
    []     -> return ()
    (p:ps) -> do
      putCCs ps
      p

queue :: Monad m => CoroutineT r m () -> CoroutineT r m ()
queue p = do
  ccs <- getCCs
  putCCs $ ccs ++ [p]

-- The interface
yield :: Monad m => CoroutineT r m ()
yield = callCC $ \k -> do
  queue $ k ()
  dequeue

fork :: Monad m => CoroutineT r m () -> CoroutineT r m ()
fork p = callCC $ \k -> do
  queue $ k ()
  p
  dequeue

{-
 - Exhaust passes control to suspended coroutines repeatedly until there
 - isn't any left
 -}
exhaust :: Monad m => CoroutineT r m ()
exhaust = do
  exhausted <- null <$> getCCs
  if not exhausted
    then yield >> exhaust
    else return ()

-- Runs the coroutines in the base monad
runCoroutineT :: Monad m => CoroutineT r m r -> m r
runCoroutineT = flip evalStateT [] . flip runContT return . runCoroutineT' . (<* exhaust)

-- Example using 'CoroutineT'
printOne :: Show a => a -> CoroutineT r IO ()
printOne n = do
  liftIO $ print n
  yield

example = runCoroutineT $ do
  fork . replicateM_ 3 $ printOne 3
  fork . replicateM_ 4 $ printOne 4
  replicateM_ 2 $ printOne 2

-- Using CPS functions to implement our own pattern matching
check :: Bool -> String
check b = case b of
  True  -> "It's True"
  False -> "It's False"

type BoolCPS r = r -> r -> r

true :: BoolCPS r
true = const

false :: BoolCPS r
false = flip const

checkCPS :: BoolCPS String -> String
checkCPS b = b "It's True" "It's False"

{-
 - Note that 'true' and 'false' are simply functions that would choose either
 - the first or the second argument passed to them. Since 'true' and 'false'
 - behave differently, we can achieve the same effect as pattern matching.
 -}

data Foobar = Zero | One Int | Two Int Int
type FoobarCPS r = r -> (Int -> r) -> (Int -> Int -> r) -> r

zero :: FoobarCPS r
zero x _ _ = x

one :: Int -> FoobarCPS r
one x _ f _ = f x

two :: Int -> Int -> FoobarCPS r
two x y _ _ f = f x y

func :: Foobar -> Int
func x = case x of
  Zero    -> 0
  One a   -> a + 1
  Two a b -> a + b + 2

{-
 - Contrasting with the non-CPS version 'func', 'funcCPS' represents the
 - pattern match via functions. These functions are continuations passed to
 - the "function-values" (ie: 'zero', 'one', 'two').
 -
 - Note that this process involves no comparison. When traditional pattern
 - matching requires types that are instances of 'Eq', these
 - "function-values" "know" what their patterns are and automatically picks
 - the corresponding continuations to call.
 -}
funcCPS :: FoobarCPS Int -> Int
funcCPS x = x 0 (+1) (\a b -> a + b + 2)
