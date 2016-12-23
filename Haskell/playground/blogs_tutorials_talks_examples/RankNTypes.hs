{-# LANGUAGE RankNTypes
           , TypeFamilies
           , GADTs
           , KindSignatures
  #-}

module RankNTypes where

import Control.Monad.State
import Data.Char
import Prelude hiding (id)
import System.Random

-- @link - https://ocharles.org.uk/blog/guest-posts/2014-12-18-rank-n-types.html

-- Monomorphic functions
intId :: Integer -> Integer
intId x = x

doubleId :: Double -> Double
doubleId x = x


-- Rank-1 Polymorphism
id :: forall a. a -> a
id x = x


-- Rank-2 and higher
type IdFunc = forall a. a -> a

id' :: IdFunc
id' x = x

someInt :: IdFunc -> Integer
someInt idF = idF 3

type SomeInt = IdFunc -> Integer

someOtherInt :: SomeInt -> Integer
someOtherInt someInt' = someInt' id + someInt' id

data Player = Player {
  playerName :: String
, playerPos  :: (Double, Double)
} deriving (Eq, Show, Ord)

type GenAction m  = forall a. Random a => m a
type GenActionR m = forall a. Random a => (a, a) -> m a

genRandom :: RandomGen g => GenAction (State g)
genRandom = state random

genRandomR :: RandomGen g => GenActionR (State g)
genRandomR range = state $ randomR range

randomPlayer :: MonadIO m => GenActionR m -> m Player
randomPlayer genR = do
  liftIO $ putStrLn "Generating random player..."

  len  <- genR (8, 12)
  name <- replicateM len $ genR ('a', 'z')
  x    <- genR (-100, 100)
  y    <- genR (-100, 100)

  liftIO $ putStrLn "Done."
  return $ Player name (x, y)

main1 :: IO ()
main1 = randomPlayer randomRIO >>= print


data List a = Cons a (List a) | Nil
  deriving (Show, Eq)

uncons :: (a -> List a -> r) -> r -> List a -> r
uncons co _  (Cons x xs) = co x xs
uncons _  ni Nil         = ni

listNull :: List a -> Bool
listNull = uncons (const $ const False) True

listMap :: (a -> b) -> List a -> List b
listMap f = uncons (\x xs -> Cons (f x) $ listMap f xs) Nil

-- Scott Encoding
-- > Representing the list in terms of its uncons operation
newtype ListS a = ListS {
  unconsS :: forall r. (a -> ListS a -> r) -> r -> r
}

{-
 - Although the list argument may appear to be missing from 'unconsS', but
 - since it is an accessor function, it's actual type is:
 - >:t unconsS :: ListS a -> (forall r. (a -> ListS a -> r) -> r -> r)
 -}

-- Scott encoded empty list, ignore the cons continuation, uses the nil
-- continuation
-- > nilS = ListS (\_ ni -> ni)
nilS :: ListS a
nilS = ListS $ flip const

-- Using only the "cons" continuation
consS :: a -> ListS a -> ListS a
consS x xs = ListS $ \co _ -> co x xs

-- Reordering the arguments of unconsS
unconsS' :: (a -> ListS a -> r) -> r -> ListS a -> r
unconsS' co ni (ListS f) = f co ni

instance Functor ListS where
  fmap f = unconsS' (\x xs -> consS (f x) $ fmap f xs) nilS

{-
 - Note:
 -   ListS requires Rank-2 polymorphism, consider the "ListS" constructor:
 -   > ListS :: (forall r. (a -> ListS a -> r) -> r -> r) -> ListS a
 -}

{-
 - Recall that defining/identifying a list in terms of "uncons" by handling
 - the pattern match in uncons is the Scott encoding.
 -
 - We can also define alists in terms of what happens when folded.
 - Identifying a list in terms of its fold is called the Church Encoding.
 -}
newtype ListC a = ListC {
  foldC :: forall r. (a -> r -> r) -> r -> r
}

foldC' :: (a -> r -> r) -> r -> ListC a -> r
foldC' co ni (ListC f) = f co ni

instance Functor ListC where
  fmap f = foldC' (\x xs -> consC (f x) xs) nilC

nilC :: ListC a
nilC = ListC $ \_ ni -> ni

consC :: a -> ListC a -> ListC a
consC x (ListC f) = ListC $ \co ni -> co x (f co ni)

-- Compare with "foldl2" that is implemented via foldr
unconsC :: (a -> r -> r) -> r -> ListC a -> r
unconsC co ni (ListC f) = f (\n g -> \a -> a n (g co)) (const ni) (const id)

foldl2 :: Foldable t => (a -> b -> b) -> b -> t a -> b
foldl2 f i xs = foldr (\n g -> \a -> f n (g a)) id xs i

foldlWithFoldr :: Foldable t => (b -> a -> b) -> b -> t a -> b
foldlWithFoldr f i xs = foldr (\n g -> \a -> f (g a) n) id xs i


listC :: Foldable t => t a -> ListC a
listC = foldr consC nilC

unListC :: ListC a -> [a]
unListC (ListC f) = f (:) []

data Some :: * -> * where
  SomeInt  :: Int  -> Some Int
  SomeChar :: Char -> Some Char
  Anything :: a    -> Some a

unSome :: Some a -> a
unSome (SomeInt x)  = x + 3
unSome (SomeChar c) = toLower c
unSome (Anything x) = x

-- SomeC has 3 continuations to match each of the constructors in the type
-- "Some"
newtype SomeC a = SomeC {
  runSomeC :: forall r. ((a ~ Int) => Int -> r)
           -> ((a ~ Char) => Char -> r)
           -> (a -> r)
           -> r
}

idKind :: forall (a :: *). a -> a
idKind x = x
