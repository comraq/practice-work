import Data.Char
import Data.List
import Control.Applicative
import Data.Monoid

main = do
  line <- mymap (bang . intersperse '-' . reverse . map toUpper) getLine
  putStrLn $ "The reversed line is: " ++ line

bang   :: String -> String
bang x =  x ++ "!"

class MyFunctor f where
  mymap :: (a -> b) -> f a -> f b

instance MyFunctor IO where
  mymap f action = do
    val <- action
    return $ f val

instance MyFunctor ((->) r) where
  mymap = (.)

instance MyFunctor Maybe where
  mymap f Nothing  = Nothing
  mymap f (Just x) = Just $ f x

data CMaybe a = CNothing | CJust Int a
  deriving Show

-- Stateful Pseudo Functor
instance MyFunctor CMaybe where
  mymap f CNothing          = CNothing
  mymap f (CJust counter x) = CJust (counter + 1) (f x)

class (MyFunctor f) => MyAppli f where
  mypure :: a -> f a
  myap   :: f (a -> b) -> f a -> f b 

instance MyAppli Maybe where
  mypure          = Just
  myap (Just f) x = fmap f x
  myap Nothing  _ = Nothing

instance MyFunctor [] where
  mymap f xs = [ f x | x <- xs ]

instance MyAppli [] where
  mypure x   = [x]
  myap fs xs = [ f x | f <- fs, x <- xs ]

instance MyAppli IO where
  mypure   = return
  myap a b = do
    f <- a
    x <- b
    return $ f x

instance MyAppli ((->) r) where
  mypure x = \_ -> x
  myap a b = \x -> a x (b x)

myAction :: IO String
myAction =  (++) `mymap` getLine `myap` getLine

instance MyFunctor ZipList where
  mymap f (ZipList xs) = ZipList $ [ f x | x <- xs ]

instance MyAppli ZipList where
  mypure                         = ZipList . repeat
  myap (ZipList fs) (ZipList xs) = ZipList $ zipWith (\f x -> f x) fs xs

mySeqA :: (Applicative f) => [f a] -> f [a]
mySeqA =  foldr (liftA2 (:)) (pure [])

newtype CharList = CharList { getCharList :: [Char] } deriving (Eq, Show)

newtype Pair b a = Pair { getPair :: (a, b) }

instance Functor (Pair c) where
  fmap f (Pair (x, y)) = Pair (f x, y)

newtype CoolBool = CoolBool { getCoolBool :: Bool }

helloMe              :: CoolBool -> String
helloMe (CoolBool _) =  "hello"

--lengthCompare     :: String -> String -> Ordering
--lengthCompare x y =  (length x `compare` length y) `myAppend`
--                     (vowels x `compare` vowels y) `myAppend`
--                     (x `compare` y)
--  where vowels = length . filter (`elem` "aeiou")

--class MyMonoid m where
--  myEmpty  :: m
--  myAppend :: m -> m -> m
--
--  myConcat :: [m] -> m
--  myConcat =  foldr myAppend myEmpty
--
--instance MyMonoid [a] where
--  myEmpty  = []
--  myAppend = (++)
--
--newtype Product a = Product { getProduct :: a }
--  deriving (Eq, Ord, Read, Show, Bounded)
--
--newtype Sum a = Sum { getSum :: a }
--  deriving (Eq, Ord, Read, Show, Bounded)
--
--newtype Any = Any { getAny :: Bool }
--  deriving (Eq, Ord, Read, Show, Bounded)
--
--newtype All = All { getAll :: Bool }
--  deriving (Eq, Ord, Read, Show, Bounded)
--
--instance Num a => MyMonoid (Product a) where
--  myEmpty                          = Product 1
--  myAppend (Product x) (Product y) = Product $ x * y
--
--instance Num a => MyMonoid (Sum a) where
--  myEmpty                  = Sum 0
--  myAppend (Sum x) (Sum y) = Sum $ x + y
--
--instance MyMonoid Any where
--  myEmpty                  = Any False
--  myAppend (Any x) (Any y) = Any $ x || y
--
--instance MyMonoid All where
--  myEmpty                  = All True
--  myAppend (All x) (All y) = All $ x && y
--
--instance MyMonoid Ordering where
--  myEmpty       = EQ
--  myAppend LT _ = LT
--  myAppend EQ y = y
--  myAppend GT _ = GT
--
--instance MyMonoid a => MyMonoid (Maybe a) where
--  myEmpty                      = Nothing
--  myAppend Nothing m           = m
--  myAppend m Nothing           = m
--  myAppend (Just m1) (Just m2) = Just $ myAppend m1 m2
--
--newtype First a = First { getFirst :: Maybe a }
--  deriving (Eq, Ord, Read, Show)
--
--instance MyMonoid (First a) where
--  myEmpty                     = First Nothing
--  myAppend (First (Just x)) _ = First $ Just x
--  myAppend (First Nothing) x  = x
--
--newtype Last a = Last { getLast :: Maybe a }
--  deriving (Eq, Ord, Read, Show)
--
--instance MyMonoid (Last a) where
--  myEmpty                    = Last Nothing
--  myAppend _ (Last (Just x)) = Last $ Just x
--  myAppend x (Last Nothing)  = x

data Tree a = Leaf | Node a (Tree a) (Tree a)
  deriving (Show, Read, Eq)

instance Foldable Tree where
  foldMap f Leaf = mempty
  foldMap f (Node x l r) = foldMap f l `mappend`
                           f x         `mappend`
                           foldMap f r

testTree = Node 5
             (Node 3
               (Node 1 Leaf Leaf)
               (Node 6 Leaf Leaf)
             )
             (Node 9
               (Node 8 Leaf Leaf)
               (Node 10 Leaf Leaf)
             )
