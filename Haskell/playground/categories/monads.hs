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

composeK     :: (MyMonad m) => (b -> m c) -> (a -> m b) -> (a -> m c)
composeK f g =  (\x -> g x `myBind` f)
