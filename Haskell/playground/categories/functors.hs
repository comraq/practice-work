import Data.Char
import Data.List
import Control.Applicative

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

sequenceA :: (Applicative f) => [f a] -> f [a]
sequenceA =  foldr (liftA2 (:)) (pure [])
