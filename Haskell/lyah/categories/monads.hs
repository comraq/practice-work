import Data.Monoid
import System.Random
import qualified Data.List as L
import Data.Ratio

class MyMonad m where
  myReturn :: a -> m a
  myBind  :: m a -> (a -> m b) -> m b

  myThen   :: m a -> m b -> m b
  x `myThen` y = x `myBind` \_ -> y

  myFail     :: String -> m a
  myFail msg =  error msg

instance MyMonad Maybe where
  myReturn = Just

  Nothing `myBind` f = Nothing
  Just x `myBind` f = f x

  myFail _ = Nothing

type Birds = Int
type Pole  = (Birds, Birds)

landLeft          :: Birds -> Pole -> Maybe Pole
landLeft x (l, r)
  | abs ((l + x) - r) < 4 = Just (l + x, r)
  | otherwise             = Nothing

landRight          :: Birds -> Pole -> Maybe Pole
landRight x (l, r)
  | abs (l - (r + x)) < 4 = Just (l, r + x)
  | otherwise             = Nothing

(-:)   :: a -> (a -> b) -> b
x -: f =  f x

banana   :: Pole -> Maybe Pole
banana _ =  Nothing

routine :: Maybe Pole
routine =  do
  start  <- return (0, 0)
  first  <- landLeft 2 start
  Nothing
  second <- landRight 2 first
  landLeft 1 second

justH :: Maybe Char
justH =  do
  (x:xs) <- Just "Hello"
  return x

wopwop :: Maybe Char
wopwop =  do
  (x:xs) <- Just ""
  return x

instance MyMonad [] where
  myReturn x = [x]
  xs `myBind` f = concat (map f xs)
  myFail _ = []

-- listOfTuples == [ (n, ch) | n <- [ 4, 5, 6 ], ch <- [ 'a', 'b', 'c' ] ]
listOfTuples :: [(Int, Char)]
listOfTuples =  do
  n <- [ 4, 5, 6 ]
  ch <- [ 'a', 'b', 'c' ]
  return (n, ch)

class MyMonad m => MyMonadPlus m where
  myMzero :: m a
  myMplus :: m a -> m a -> m a

-- Near analogous to monoids for monads
-- http://stackoverflow.com/questions/17056881/monoid-vs-monadplus
instance MyMonadPlus [] where
  myMzero = []
  myMplus = (++)

guard       :: (MyMonadPlus m) => Bool -> m ()
guard True  =  myReturn ()
guard False =  myMzero

sevensOnly :: [Int]
sevensOnly = do
  x <- [1..50]
  guard ('7' `elem` show x)
  return x

type KnightPos = (Int, Int)

moveKnight        :: KnightPos -> [KnightPos]
moveKnight (c, r) =  do
  (c', r') <- [ (c+2,r-1),(c+2,r+1),(c-2,r-1),(c-2,r+1)
              , (c+1,r-2),(c+1,r+2),(c-1,r-2),(c-1,r+2)
              ]
  guard (c' `elem` [1..8] && r' `elem` [1..8])
  return (c', r')

moveKn3       :: KnightPos -> [KnightPos]
moveKn3 start =  do
  first <- moveKnight start
  second <- moveKnight first
  moveKnight second

canReach3     :: KnightPos -> KnightPos -> Bool
canReach3 s e =  e `elem` moveKn3 s

(<=<)   :: (Monad m) => (b -> m c) -> (a -> m b) -> (a -> m c)
f <=< g =  \x -> g x >>= f

(<-<)   :: (MyMonad m) => (b -> m c) -> (a -> m b) -> (a -> m c)
f <-< g =  \x -> g x `myBind` f

isBigGang   :: Int -> (Bool, String)
isBigGang x =  (x > 9, "Compared gang size to 9.")

applyLog            :: (Monoid m) => (a, m) -> (a -> (b, m)) -> (b, m)
applyLog (x, log) f =
  let (y, newLog) = f x
  in (y, log `mappend` newLog)

type Food = String
type Price = Sum Int

addDrink         :: Food -> (Food, Price)
addDrink "beans" =  ("milk", Sum 25)
addDrink "jerky" =  ("whiskey", Sum 99)
addDrink _       =  ("beer", Sum 30)

newtype Writer w a = Writer { runWriter :: (a, w) }
  deriving (Show)

instance Functor (Writer w) where
  fmap f (Writer (x, v)) =  Writer (f x, v)

instance (Monoid w) => Applicative (Writer w) where
  pure x                          = Writer (x, mempty)
  Writer (f, v) <*> Writer (x, _) = Writer (f x, v)

instance (Monoid w) => Monad (Writer w) where
  return              = pure
  Writer (x, v) >>= f =
    let (Writer (y, v')) = f x
    in Writer (y, mappend v v')

logNumber   :: Int -> Writer [String] Int
logNumber x =  Writer (x, ["Got number: " ++ show x])

-- Attempting to implement Control.Monad.Writer.tell
tell   :: (Monoid w) => w -> Writer w ()
tell w =  Writer ( (), w )

multWithLog :: Writer [String] Int
multWithLog =  do
  a <- logNumber 3
  b <- logNumber 5
  tell [ "My own tell implementation" ]
  return (a * b)

gcd'     :: Int -> Int -> Writer [String] Int
gcd' a b
  | b == 0    = do
    tell [ "Finished with " ++ show a ]
    return a
  | otherwise = do
    tell [ show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b) ]
    gcd' b (a `mod` b)

gcdReverse :: Int -> Int -> Writer (DiffList String) Int
gcdReverse a b
  | b == 0    = do
    tell $ toDiffList [ "Finished with " ++ show a ]
    return a
  | otherwise = do
    result <- gcdReverse b (a `mod` b)
    tell $ toDiffList [ show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b) ]
    return result

newtype DiffList a = DiffList { getDiffList :: [a] -> [a] }

toDiffList    :: [a] -> DiffList a
toDiffList xs =  DiffList (xs++)

fromDiffList              :: DiffList a -> [a]
fromDiffList (DiffList f) =  f []

instance Monoid (DiffList a) where
  mempty                          = DiffList $ \xs -> [] ++ xs
  DiffList f `mappend` DiffList g = DiffList $ \xs -> f $ g xs

countDown   :: Int -> Writer (DiffList String) ()
countDown 0 =  do
  tell (toDiffList ["0"])
countDown x =  do
  countDown (x - 1)
  tell (toDiffList [show x])

countDown'   :: Int -> Writer [String] ()
countDOwn' 0 =  do
  tell ["0"]
countDown' x =  do
  countDown' (x - 1)
  tell [show x]

instance MyMonad ((->) r) where
  myReturn x   = \_ -> x
  h `myBind` f = \x -> f (h x) x

addStuff :: Int -> Int
addStuff =  do
  a <- (*2)
  b <- (+10)
  return (a + b)

addStuff' :: Int -> Int
addStuff' = (*2) `myBind` \a -> (+10) `myBind` \b -> myReturn $ a + b

-- addStuff = (*2) >>= \a -> (+10) >>= \b -> return $ a + b
-- addStuff 3 == 19   --True
--
-- 1) \a' -> (\a -> (+10) >>= \b -> return $ a + b) (2 * a') a'
-- 2)  3  -> (\a -> (+10) >>= \b -> return $ a + b) (2 * 3 ) 3
-- 3)        ( 6 -> (+10) >>= \b -> return $ 6 + b)          3
-- 4)        (\b' -> (\b  -> return $ 6 + b ) (10 + b') b')  3
-- 5)          3  -> (\b  -> return $ 6 + b ) (10 + 3 ) 3
-- 6)                (\13 -> return $ 6 + 13))          3
-- 7)                        (\_ -> 19)                 3
-- 8)                               19

addStuff''   :: Int -> Int
addStuff'' x =  let
  a = (*2) x
  b = (+10) x
  in a + b

type Stack = [Int]

pop        :: Stack -> (Int, Stack)
pop (x:xs) =  (x, xs)

push      :: Int -> Stack -> ((), Stack)
push a xs =  ((), a:xs)

stackManip       :: Stack -> (Int, Stack)
stackManip stack =  let
  ((), newStack1) = push 3 stack
  (a, newStack2)  = pop newStack1
  (b, newStack3)  = pop newStack2
  in pop newStack2

-- Does Not Work! State is lost at each expression (line in do block)!
-- runState stackManip' [5, 8, 2, 1] /= (8, [2, 1])      --True
-- runState stackManip' [5, 8, 2, 1] == (5, [8, 2, 1])   --True
stackManip' :: Stack -> (Int, Stack)
stackManip' =  do
  push 3
  pop
  pop
  pop

-- stackManip'' == stackManip'
stackManip'' :: Stack -> (Int, Stack)
stackManip'' = push 3 `myThen` pop `myThen` pop `myThen` pop

-- stackManip' == stackManip'' = push 3 >> pop >> pop >> pop
-- stackManip' [5, 8, 2, 1] == (5, [8, 2, 1])   --True
--
-- push 3 >>= \_ -> pop >> pop >> pop
-- \a         -> (\_ -> pop >> pop >> pop) (push 3 a)               a
-- \[5,8,2,1] -> (\_ -> pop >> pop >> pop) (push 3 [5,8,2,1])       [5,8,2,1]
--               (\_ -> pop >> pop >> pop) ((), [3,5,8,2,1])        [5,8,2,1]
--                \_ -> pop >> pop >> pop                           [5,8,2,1]
--                      pop >>= \_ -> pop >> pop                    [5,8,2,1]
--                      (\b -> (\_ -> pop >> pop) (pop b) b)        [5,8,2,1]
--
-- (\b         -> (\_ -> pop >> pop) (pop b)             b)         [5,8,2,1]
-- (\[5,8,2,1] -> (\_ -> pop >> pop) (pop [5,8,2,1])     [5,8,2,1]
--                (\_ -> pop >> pop) (5, [8,2,1])        [5,8,2,1]
--                 \_ -> pop >> pop                      [5,8,2,1]
--                       pop >>= \_ -> pop               [5,8,2,1]
--                       (\c -> (\_ -> pop) (pop c) c)   [5,8,2,1]
--
-- (\c         -> (\_ -> pop) (pop c)         c)         [5,8,2,1]
-- (\[5,8,2,1] -> (\_ -> pop) (pop [5,8,2,1]) [5,8,2,1]
--                (\_ -> pop) (5, [8,2,1])    [5,8,2,1]
--                 \_ -> pop                  [5,8,2,1]
--                       pop                  [5,8,2,1]
--                       (5, [8,2,1])

newtype State s a = State { runState :: s -> (a, s) }

instance Functor (State s) where
  fmap f (State m) = State $ \s -> let (a, stateA) = m s
                                   in  (f a, stateA)

instance Applicative (State s) where
  pure x              = State $ \s -> (x, s)
  State f <*> State g = State $ \s -> let (h, state1) = f s
                                          (b, state2) = g state1
                                      in (h b, state2)

instance Monad (State s) where
  return        = pure

  -- (>>=) :: State s a -> (a -> State s b) -> State s b
  -- Note: (State s) represents a state monad, if s is changed then it would
  --       be two DIFFERENT monads
  State h >>= f = State $ \s -> let (a, newState) = h s
                                    (State g)     = f a
                                in  g newState

popS :: State Stack Int
popS =  State $ \(x:xs) -> (x, xs)

pushS   :: Int -> State Stack ()
pushS x =  State $ \xs -> ((), x:xs)

-- Works! State persists!
-- runState stackManipS [5, 8, 2, 1] == (8, [2, 1])  --True
stackManipS :: State Stack Int
stackManipS =  do
  pushS 3
  popS
  popS
  popS

-- stackManipS = pushS 3 >> popS >> popS >> popS
-- runState stackManipS [5, 8, 2, 1] == (8, [2, 1])      --True
--
-- pushS 3                   >>= \_ -> popS >> popS >> popS
-- State $ \xs -> ((), 3:xs) >>= \_ -> popS >> popS >> popS
--
-- State $ \s -> let (a, newState) = (\xs -> ((), 3:xs)) s
--                   (State g)     = (\_ -> popS >> popS >> popS) a
--               in g newState
--
-- State $ \s -> let (a, newState) = (\xs -> ((), 3:xs)) s
--                   (State g)     = popS >> popS >> popS
--               in g newState
--
-- State $ \s -> let (a, newState) = (\xs -> ((), 3:xs)) s
--                   (State g)     = State $ \(x':xs') -> (x', xs') >>= \_ -> popS >> popS
--               in g newState
--
-- State $ \s -> let (a, newState) = (\xs -> ((), 3:xs)) s
--                   (State g)     = State $ \s' -> let (b, newState') = (\(x':xs') -> (x', xs')) s'
--                                                      (State g')     = (\_ -> popS >> popS) b
--                                                  in g' newState'
--               in g newState
--
-- State $ \s -> let (a, newState) = (\xs -> ((), 3:xs)) s
--               in  (\s' -> let (b, newState') = (\(x':xs') -> (x', xs')) s'
--                               (State g')     = (\_ -> popS >> popS) b
--                           in g' newState'
--                   ) newState
--
-- State $ \s -> let (a, newState) = (\xs -> ((), 3:xs)) s
--               in  (\s' -> let (b, newState') = (\(x':xs') -> (x', xs')) s'
--                               (State g')     = popS >> popS
--                           in g' newState'
--                   ) newState
--
-- State $ \s -> let (a, newState) = (\xs -> ((), 3:xs)) s
--               in  (\s' -> let (b, newState') = (\(x':xs') -> (x', xs')) s'
--                               (State g')     = State $ \(x'':xs'') -> (x'', xs'') >>= \_ -> popS
--                           in g' newState'
--                   ) newState
--
-- State $ \s -> let (a, newState) = (\xs -> ((), 3:xs)) s
--               in  (\s' -> let (b, newState') = (\(x':xs') -> (x', xs')) s'
--                               (State g')     = State $ \s'' -> let (c, newState'') = (\(x'':xs'') -> (x'', xs'')) s''
--                                                                    (State g'')     = (\_ -> popS) c
--                           in g' newState'
--                   ) newState
--
-- State $ \s -> let (a, newState) = (\xs -> ((), 3:xs)) s
--               in  (\s' -> let (b, newState') = (\(x':xs') -> (x', xs')) s'
--                           in (\s'' -> let (c, newState'') = (\(x'':xs'') -> (x'', xs'')) s''
--                                           (State g'')     = (\_ -> popS) c
--                                       in  g'' newState''
--                              ) newState'
--                   ) newState
--
-- State $ \s -> let (a, newState) = (\xs -> ((), 3:xs)) s
--               in  (\s' -> let (b, newState') = State $ \(x':xs') -> (x', xs') s'
--                           in (\s'' -> let (c, newState'') = (\(x'':xs'') -> (x'', xs'')) s''
--                                           (State g'')     = popS
--                                       in  g'' newState''
--                              ) newState'
--                   ) newState
--
-- State $ \s -> let (a, newState) = (\xs -> ((), 3:xs)) s
--               in  (\s' -> let (b, newState') = State $ \(x':xs') -> (x', xs') s'
--                           in (\s'' -> let (c, newState'') = (\(x'':xs'') -> (x'', xs'')) s''
--                                           (State g'')     = State $ \(x''':xs''') -> (x''', xs''')
--                                       in  g'' newState''
--                              ) newState'
--                   ) newState
--
-- State $ \s -> let (a, newState) = (\xs -> ((), 3:xs)) s
--               in  (\s' -> let (b, newState') = (\(x':xs') -> (x', xs')) s'
--                           in (\s'' -> let (c, newState'') = (\(x'':xs'') -> (x'', xs'')) s''
--                                       in  (\(x''':xs''') -> (x''', xs''')) newState''
--                              ) newState'
--                   ) newState
--
--
-- runState stateManipS [5, 8, 2, 1]
-- (\s -> let (a, newState) = (\xs -> ((), 3:xs)) s
--        in  (\s' -> let (b, newState') = (\(x':xs') -> (x', xs')) s'
--                    in (\s'' -> let (c, newState'') = (\(x'':xs'') -> (x'', xs'')) s''
--                                in  (\(x''':xs''') -> (x''', xs''')) newState''
--                        ) newState'
--            ) newState
-- ) [5,8,2,1]
--
-- \[5,8,2,1] -> let (a, newState) = (\xs -> ((), 3:xs)) [5,8,2,1]
--               in  (\s' -> let (b, newState') = (\(x':xs') -> (x', xs')) s'
--                           in (\s'' -> let (c, newState'') = (\(x'':xs'') -> (x'', xs'')) s''
--                                       in  (\(x''':xs''') -> (x''', xs''')) newState''
--                              ) newState'
--                   ) newState
--
-- let (a, newState) = \[5,8,2,1] -> ((), 3:[5,8,2,1])
-- in  (\s' -> let (b, newState') = (\(x':xs') -> (x', xs')) s'
--             in (\s'' -> let (c, newState'') = (\(x'':xs'') -> (x'', xs'')) s''
--                         in  (\(x''':xs''') -> (x''', xs''')) newState''
--                ) newState'
--     ) newState
--
-- let (a, newState) = ((), [3,5,8,2,1])
-- in  (\s' -> let (b, newState') = (\(x':xs') -> (x', xs')) s'
--             in (\s'' -> let (c, newState'') = (\(x'':xs'') -> (x'', xs'')) s''
--                         in  (\(x''':xs''') -> (x''', xs''')) newState''
--                ) newState'
--     ) newState
--
-- let ((), [3,5,8,2,1])
-- in  (\s' -> let (b, newState') = (\(x':xs') -> (x', xs')) s'
--             in (\s'' -> let (c, newState'') = (\(x'':xs'') -> (x'', xs'')) s''
--                         in  (\(x''':xs''') -> (x''', xs''')) newState''
--                ) newState'
--     ) [3,5,8,2,1]
--
-- \[3,5,8,2,1] -> let (b, newState') = (\(x':xs') -> (x', xs')) [3,5,8,2,1]
--                 in (\s'' -> let (c, newState'') = (\(x'':xs'') -> (x'', xs'')) s''
--                             in  (\(x''':xs''') -> (x''', xs''')) newState''
--                    ) newState'
--
-- let (b, newState') = \(3:[5,8,2,1]) -> (3, [5,8,2,1])
-- in (\s'' -> let (c, newState'') = (\(x'':xs'') -> (x'', xs'')) s''
--             in  (\(x''':xs''') -> (x''', xs''')) newState''
--    ) newState'
--
-- let (3, [5,8,2,1])
-- in (\s'' -> let (c, newState'') = (\(x'':xs'') -> (x'', xs'')) s''
--             in  (\(x''':xs''') -> (x''', xs''')) newState''
--    ) [5,8,2,1]
--
-- \[5,8,2,1] -> let (c, newState'') = (\(x'':xs'') -> (x'', xs'')) [5,8,2,1]
--               in  (\(x''':xs''') -> (x''', xs''')) newState''
--
-- let (c, newState'') = \(5:[8,2,1]) -> (5, [8,2,1])
-- in  (\(x''':xs''') -> (x''', xs''')) newState''
--
-- let (5, [8,2,1])
-- in  (\(x''':xs''') -> (x''', xs''')) [8,2,1]
--
-- \(8:[2,1]) -> (8:[2,1])
--
-- (8:[2,1])



-- runState stackStuff $ snd $ runState stackStuff [9, 0, 2, 1, 0]
-- = ((),[8,3,3,0,2,1,0])
stackStuff :: State Stack ()
stackStuff =  do
  a <- popS
  if a == 5
    then pushS 5
    else do
      pushS 3
      pushS 8

moreStack :: State Stack ()
moreStack =  do
  a <- stackManipS
  if a == 100
    then stackStuff
    else return ()

get :: State a a
get =  State $ \s -> (s, s)

put          :: s -> State s ()
put newState =  State $ \s -> ((), newState)

stackyStack :: State Stack ()
stackyStack =  do
  stackNow <- get
  if stackNow == [ 1, 2, 3 ]
    then put [ 8, 3, 1 ]
    else put [ 9, 2, 1 ]

-- random :: (RandomGen g, Random a) => g -> (a, g)
randomSt :: (RandomGen g, Random a) => State g a
randomSt =  State random

-- runState threeCoins $ snd $ runState threeCoins (mkStdGen 7)
-- example of tossing 'threeCoins' twice where '7' was the initial seed for
-- mkStdGen and the returned StdGen instance is passed to the second
-- 'threeCoins' call
threeCoins :: State StdGen (Bool, Bool, Bool)
threeCoins =  do
  a <- randomSt
  b <- randomSt
  c <- randomSt
  return (a, b, c)

instance MyMonad (Either e) where
  myReturn x         = Right x
  Right x `myBind` f = f x
  Left e `myBind`  f = Left e
  myFail msg         = Left $ error msg

myLiftM     :: (MyMonad m) => (a -> b) -> m a -> m b
myLiftM f m =  m `myBind` \a -> myReturn $ f a

liftM     :: (Monad m) => (a -> b) -> m a -> m b
liftM f m =  m >>= \a -> return $ f a

myAp     :: (MyMonad m) => m (a -> b) -> m a -> m b
myAp f m =  f `myBind` \g -> myLiftM g m

ap     :: (Monad m) => m (a -> b) -> m a -> m b
ap f m =  f >>= \g -> liftM g m

liftA2       :: (Applicative f) => (a -> b -> c) -> f a -> f b -> f c
liftA2 f x y =  f <$> x <*> y

myJoin   :: (MyMonad m) => m (m a) -> m a
myJoin m =  m `myBind` id

join   :: (Monad m) => m (m a) -> m a
join m =  m >>= id

filterM   :: (Monad m) => (a -> m Bool) -> [a] -> m [a]
filterM f =
  foldr pred (return [])
    where pred = \x acc -> ((f x) >>= \b -> case b of
                   True  -> (acc >>= \xs -> return $ x:xs)
                   False -> acc)

myFilterM   :: (MyMonad m) => (a -> m Bool) -> [a] -> m [a]
myFilterM f =
  foldr pred (myReturn [])
    where pred = \x acc -> ((f x) `myBind` \b -> case b of
                   True  -> (acc `myBind` \xs -> myReturn $ x:xs)
                   False -> acc)

keepSmall   :: Int -> Writer [String] Bool
keepSmall x
  | x < 4 = do
    tell [ "Keeping " ++ show x ]
    return True

  | otherwise = do
    tell [ show x ++ " is too large, throwing it away" ]
    return False

powerset :: [a] -> [[a]]
powerset =  filterM (\x -> [True, False])

foldM       :: (Monad m, Foldable t) => (a -> b -> m a) -> a -> t b -> m a
foldM f acc =  L.foldl' g (return acc)
  where g = (\ma b -> ma >>= \a -> f a b)

binSmalls       :: Int -> Int -> Maybe Int
binSmalls acc x
  | x > 9     = Nothing
  | otherwise = Just (acc + x)

solveRPN     :: String -> Maybe Double
solveRPN str =  (foldM foldingFunction [] $ words str) >>= onlyOne

onlyOne     :: [a] -> Maybe a
onlyOne [x] =  Just x
onlyOne _   =  Nothing

foldingFunction              :: [Double] -> String -> Maybe [Double]
foldingFunction (x:y:ys) "*" =  return $ (x * y):ys
foldingFunction (x:y:ys) "+" =  return $ (x + y):ys
foldingFunction (x:y:ys) "-" =  return $ (y - x):ys
foldingFunction xs numStr    =  liftM (:xs) $ readMaybe numStr

readMaybe    :: (Read a) => String -> Maybe a
readMaybe st =  case reads st of
  [(x, "")] -> Just x
  _         -> Nothing

inMany         :: Int -> KnightPos -> [KnightPos]
inMany x start =  return start >>= foldr (<=<) return (replicate x moveKnight)

canReachIn             :: Int -> KnightPos -> KnightPos -> Bool
canReachIn x start end =  end `elem` inMany x start

newtype Prob a = Prob { getProb :: [(a, Rational)] } deriving Show

instance Functor Prob where
  fmap f (Prob xs) = Prob $ map (\(x, p) -> (f x, p)) xs

testCase :: Prob (Prob Char)
testCase =  Prob
  [ ( Prob [ ('a', 1%2), ('b', 1%2) ], 1%4 )
  , ( Prob [ ('c', 1%2), ('d', 1%2) ], 3%4 )
  ]

flatten           :: Prob (Prob a) -> Prob a
flatten (Prob xs) =  Prob $ concat $ map f xs
  where f = \( (Prob ys), p ) ->
              map (\(x, p') -> (x, p' * p)) ys

instance Applicative Prob where
  pure x  = Prob [(x, 1%1)]

  -- (<*>) :: (Applicative f) => f (a -> b) -> f a -> f b
  f <*> a = f >>= \f' -> fmap f' a

instance Monad Prob where
  return  = pure
  m >>= f = flatten $ fmap f m
  fail _  = Prob []

data Coin = Heads | Tails
  deriving (Show, Eq)

coin :: Prob Coin
coin =  Prob [ (Heads, 1%2), (Tails, 1%2) ]

loadedCoin :: Prob Coin
loadedCoin =  Prob [ (Heads, 1%10), (Tails, 9%10) ]

flipThree :: Prob Bool
flipThree =  coin >>= \a -> coin >>= \b -> loadedCoin >>= \c -> return $ all (== Tails) [a, b, c]
