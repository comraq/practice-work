{-# LANGUAGE DeriveFunctor #-}

-- From Gabriel Gonzalez's Blog Post
-- @link - http://www.haskellforall.com/2012/06/you-could-have-invented-free-monads.html

import Control.Monad (forever, void, when)

data Toy b next = Output b next
                | Bell next
                | Done
  deriving Show

data Fix f = Fix (f (Fix f))

data FixE f e = FixE (f (FixE f e)) | Throw e

catch :: Functor f => FixE f e1 -> (e1 -> FixE f e2) -> FixE f e2
catch (FixE x) f  = FixE (fmap (`catch` f) x)
catch (Throw e) f = f e

instance Functor (Toy b) where
  fmap f (Output x next) = Output x (f next)
  fmap f (Bell next)     = Bell     (f next)
  fmap _ Done            = Done

data IncompleteException = IncompleteException

subroutine = FixE (Output 'A' (Throw IncompleteException))
program = subroutine `catch` const (FixE (Bell (FixE Done)))

data Free f a = Free (f (Free f a)) | Pure a

instance Functor f => Functor (Free f) where
  fmap f (Free x) = Free (fmap (fmap f) x)
  fmap f (Pure a) = Pure (f a)

instance Functor f => Applicative (Free f) where
  pure    = return
  f <*> a = f >>= \f' -> fmap f' a

instance Functor f => Monad (Free f) where
  return  = Pure
  (Pure a) >>= f = f a
  (Free m) >>= f = Free (fmap (>>= f) m)

output :: a -> Free (Toy a) ()
output x = Free (Output x (Pure ()))

bell :: Free (Toy a) ()
bell = Free (Bell (Pure ()))

done :: Free (Toy a) r
done = Free Done

liftF :: Functor f => f r -> Free f r
liftF f = Free (fmap Pure f)

output' x = liftF (Output x ())
bell'     = liftF (Bell     ())
done'     = liftF Done

subroutine' :: Free (Toy Char) ()
subroutine' = output 'A'

program' :: Free (Toy Char) r
program' = do
  subroutine'
  bell'
  done'

showProgram :: (Show a, Show r) => Free (Toy a) r -> String
showProgram (Free (Output a x)) = "output " ++ show a ++ "\n" ++ showProgram x
showProgram (Free (Bell x))     = "bell\n" ++ showProgram x
showProgram (Free Done)         = "done\n"
showProgram (Pure r)            = "return " ++ show r ++ "\n"

pretty :: (Show a, Show r) => Free (Toy a) r -> IO ()
pretty = putStr . showProgram


data Thread m r = Atomic (m (Thread m r)) | Return r

atomic :: Monad m => m a -> Thread m a
atomic m = Atomic $ fmap Return m

instance Functor m => Monad (Thread m) where
  return = Return
  (Return r) >>= f = f r
  (Atomic m) >>= f = Atomic $ fmap (>>= f) m

instance Functor m => Functor (Thread m) where
  fmap f (Return r) = Return $ f r
  fmap f (Atomic m) = Atomic $ fmap (fmap f) m

instance Functor m => Applicative (Thread m) where
  pure = Return
  f <*> a = f >>= \f' -> fmap f' a

thread1 :: Thread IO ()
thread1 = do
  atomic $ print 1
  atomic $ print 2

thread2 :: Thread IO ()
thread2 = do
  str <- atomic getLine
  atomic $ putStrLn str

interleave :: Monad m => Thread m r -> Thread m r -> Thread m r
interleave (Atomic m1) (Atomic m2) = do
  next1 <- atomic m1
  next2 <- atomic m2
  interleave next1 next2

interleave t (Return _) = t
interleave (Return _) t = t

runThread :: Monad m => Thread m r -> m r
runThread (Atomic m) = m >>= runThread
runThread (Return r) = return r


{-
 - An analogy to the Free Monad is the list, a list a functors
 - data Free f a = Free (f (Free f a)) | Pure a
 - data List a   = Cons  a (List a)    | Nil
 -}
data List a = Cons a (List a) | Nil

-- Nil === ()
-- Cons(a, List' a) === (a, List' a)
type List' a = Free ((,) a) ()

-- List' a
-- Free ((,) a) ()
-- Free a (List' a)  | Pure ()
-- Free (a, List' a) | Pure ()

-- liftF x = Free (fmap Pure x)
-- ie: construct a list with functor as head and tail as "Pure x" (Nil)
--     - here, the functor itself is the value at each node of "List' a"
singleton x = Cons x Nil -- ie: x:[]

merge (x1:xs1) (x2:xs2) = x1:x2:merge xs1 xs2
merge xs [] = xs
merge [] xs = xs

{-
 - "merge" is akin to the "interleave" function for "Thread m r":
 -
 -   interleave :: Monad m => Thread m r -> Thread m r -> Thread m r
 -   interleave (Atomic m1) (Atomic m2) = do
 -     next1 <- atomic m1
 -     next2 <- atomic m2
 -     interleave next1 next2

 -   interleave t (Return _) = t
 -   interleave (Return _) t = t
 -
 - 1) "atomic m1" and "atomic m2" extracts out the head of list (head of the
 -    cons cell) while also applying the action
 - 2) "next1" and "next2" are the tails of the two lists
 - 3) recursively merge/interleave on the two tails
 -}

{-
 - Naive Attempt:
 -
 - data Request = Look Direction
 -              | ReadLine
 -              | Fire Direction
 -              | WriteLine String
 -
 - data Response = Image Picture   -- Response for Look
 -               | ChatLine String -- Response for Read
 -               | Succeeded Bool  -- Response for Write
 -
 - main :: [Response] -> [Request]
 -}

-- Placeholder types
data Direction = Forward
type Image = ()

data Interaction next = Look Direction (Image -> next)
                      | Fire Direction next
                      | ReadLine (String -> next)
                      | WriteLine String (Bool -> next)
  deriving Functor

type Program = Free Interaction

easyToAnger :: Free Interaction a
easyToAnger = Free . ReadLine $ \s -> case s of
  "No" -> Free . Fire Forward . Free . WriteLine "Take That!" $ const easyToAnger
  _    -> easyToAnger

{-
 - Example "interpret" function for the "Program" free monad:
 - interpret :: Program r -> Game r
 - interpret prog = case prog of
 -   Free (Look dir g) -> do
 -     img <- collectImage
 -     interpret (g img)
 -
 -   Free (Fire dir next) -> do
 -     sendBullet dir
 -     interpret next
 -
 -   Free (ReadLine g) -> do
 -     str <- getChatLine
 -     interpret (g str)
 -
 -   Free (WriteLine s g) -> do
 -     putChatLine s
 -     interpret (g True)
 -
 -   Pure r -> return r
 -}

look :: Direction -> Program Image
look dir = liftF $ Look dir id

fire :: Direction -> Program ()
fire dir = liftF $ Fire dir ()

readLine :: Program String
readLine = liftF $ ReadLine id

writeLine :: String -> Program Bool
writeLine str = liftF $ WriteLine str id

easyToAnger' :: Program a
easyToAnger' = forever $ do
  str <- readLine
  when (str == "No") $ do
    fire Forward
    -- Ignore the Bool returned by writeLine
    void $ writeLine "Take that!"
