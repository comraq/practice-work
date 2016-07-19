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
