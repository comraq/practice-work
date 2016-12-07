{-# LANGUAGE DeriveFunctor
           , TypeFamilies
           , ScopedTypeVariables
  #-}

module RecursionSchemes2 where

import Control.Monad (join)
import Data.Functor.Foldable
import Data.List (delete)
import GHC.Real

-- @link - http://jozefg.bitbucket.org/posts/2014-05-19-like-recursion-but-cooler.html

data MyList a  = MyCons a (MyList a) | MyNil
  deriving Show

data BList a b = BCons  a b          | BNil
  deriving Functor

type instance Base (MyList a) = BList a

------- Recursive (Folds) -------

instance Recursive (MyList a) where
  project (MyCons a b) = BCons a b
  project _            = BNil

myMap :: forall a b. (a -> b) -> MyList a -> MyList b
myMap f = cata mapper where
  mapper :: Base (MyList a) (MyList b) -> MyList b
  mapper (BCons a b) = f a `MyCons` b
  mapper _           = MyNil

myFilter :: forall a. (a -> Bool) -> MyList a -> MyList a
myFilter p = cata filterer where
  filterer :: Base (MyList a) (MyList a) -> MyList a
  filterer (BCons a b)
    | p a       = MyCons a b
    | otherwise = b
  filterer _ = MyNil


{-
  foldr :: (           a -> b  -> b) -> b -> [a] -> b
  foldr :: ((->)       a    b  -> b) -> b -> [a] -> b
  foldr :: (Maybe (    a,   b) -> b) ->      [a] -> b
  foldr :: (Maybe ((,) a    b) -> b) ->      [a] -> b
  cata  :: (BList      a    b  -> b) -> MyList a -> b
  cata  :: (Base (MyList a) b  -> b) -> MyList a -> b
  cata  :: (Base t          b  -> b) -> t        -> b

  Note: 'cata' does not require an "initial" seed/value,
         as it is up to the algebra's responsibility to handle the different
         cases (corresponding to the different constructors)
 -}

sumTails :: forall a. Num a => [a] -> [a]
sumTails = para summer where
  summer :: Base [a] ([a], [a]) -> [a]
  summer (Cons a (list, rest)) = a + sum list : rest
  summer _                     = []


-- Example with an 'Foo' Language AST

data Op = Plus | Sub | Mult | Div
  deriving Show

data Foo = Num    Int             -- Numeric literals
         | String String          -- String literals
         | Binop  Op     Foo Foo  -- Primitive operation
         | Fun    String Foo      -- Lambda Abstraction
         | App    Foo    Foo      -- Application
         | Var    String          -- Variables
  deriving Show

compute :: Op -> Int -> Int -> Int
compute Plus = (+)
compute Sub  = (-)
compute Mult = (*)
compute Div  = div

reduce :: Foo -> Foo
reduce (Binop op (Num a) (Num b)) = Num $ compute op a b
reduce a                          = a


data FooB a = NumB    Int
            | StringB String
            | BinopB  Op     a a
            | FunB    String a
            | AppB    a      a
            | VarB    String
  deriving Functor

type instance Base Foo = FooB

instance Recursive Foo where
  project (Num a)        = NumB a
  project (String a)     = StringB a
  project (Binop op a b) = BinopB op a b
  project (Fun v a)      = FunB v a
  project (App a b)      = AppB a b
  project (Var a)        = VarB a

rProject :: Base Foo Foo -> Foo
rProject (NumB a)        = Num a
rProject (StringB a)     = String a
rProject (BinopB op a b) = Binop op a b
rProject (FunB v a)      = Fun v a
rProject (AppB a b)      = App a b
rProject (VarB a)        = Var a


constFold :: Foo -> Foo
constFold = cata reduceB
  where reduceB :: Base Foo Foo -> Foo
        reduceB (BinopB op (Num a) (Num b)) = Num $ compute op a b
        reduceB a                           = rProject a


t1 = Binop Plus (Num 1) (Binop Mult (Num 2) (Num 3))

ex1 :: IO ()
ex1 = print $ constFold t1


freeVarsIn :: Foo -> [String]
freeVarsIn = cata freeVar
  where freeVar :: Base Foo [String] -> [String]
        freeVar (NumB _)         = []
        freeVar (StringB _)      = []
        freeVar (VarB s)         = [s]
        freeVar (BinopB _ v1 v2) = v1 ++ v2
        freeVar (AppB v1 v2)     = v1 ++ v2
        freeVar (FunB v vs)      = delete v vs

t2 = App (Fun "f1" (Var "v1")) (Var "v2")

ex2 :: IO ()
ex2 = print $ freeVarsIn t2


------- Corecursive (Unfolds) -------

instance Corecursive (MyList a) where
  embed (BCons a b) = MyCons a b
  embed _           = MyNil

myBetween' :: (Eq a, Enum a) => a -> a -> MyList a
myBetween' a b | a == b    = MyNil
               | otherwise = succ a `MyCons` myBetween' (succ a) b

myBetween :: forall a. (Eq a, Enum a) => a -> a -> MyList a
myBetween low high = ana builder low where
  builder :: a -> Base (MyList a) a
  builder a | a == high = BNil
         -- | otherwise = let next = succ a
         --               in  BCons next next
            | otherwise = join BCons (succ a)

between :: forall a. (Eq a, Enum a) => a -> a -> [a]
between low high = ana builder low where
  builder :: a -> Base [a] a
  builder a | a == high = Nil
            | otherwise = join Cons (succ a)


{-
   Consider 'Data.List.unfoldr':
     unfoldr :: (b -> Maybe (    a,   b)) -> b -> [a]
     unfoldr :: (b -> Maybe ((,) a    b)) -> b -> [a]
     ana     :: (b -> BList      a    b ) -> b -> MyList a
     ana     :: (b -> Base (MyList a) b ) -> b -> MyList a
     ana     :: (b -> Base t          b ) -> b -> t

   where BCons a b ~= Maybe (a, b)
         BNil      ~= Nothing
 -}

data RedBlack a = Red   a (RedBlack a) (RedBlack a)
                | Black a (RedBlack a) (RedBlack a)
                | Leaf


data Bin a    = Node a (Bin a) (Bin a)
  deriving Show

data BBin a b = NodeB a b b
  deriving Functor

type instance Base (Bin a) = BBin a

instance Corecursive (Bin a) where
  embed (NodeB a l r) = Node a l r

instance Recursive (Bin a) where
  project (Node a l r) = NodeB a l r

rats :: Bin Rational
rats = ana builder (1 % 1)

builder :: Integral a => Ratio a -> BBin (Ratio a) (Ratio a)
builder r@(p :% q) = NodeB r ((p + q) % q) (p % (p + q))

collapse :: Bin a -> [a]
collapse = cata folder

folder :: BBin a [a] -> [a]
folder (NodeB a l r)     = a : interleave l r
  where
    interleave :: [a] -> [a] -> [a]
    interleave (x:xs) (y:ys) = x : y : interleave xs ys

allRats :: [Rational]
allRats = hylo folder builder (1 % 1)


-- Sliding Window Problem
sliding :: forall a. Int -> [a] -> [[a]]
sliding n = para alg where
  alg :: Base [a] ([a], [[a]]) -> [[a]]
  alg (Cons a (as, rss)) = take n (a:as) : rss
  alg _                  = []


-- Coinductive Streams

data StreamF a r = StreamF a r
  deriving Functor

data Stream a    = Stream a (Stream a)
  deriving Show

type instance Base (Stream a) = StreamF a

instance Corecursive (Stream a) where
  embed (StreamF a r) = Stream a r

consS :: a -> Stream a -> Stream a
consS = Stream

headS :: Stream a -> a
headS (Stream x _ ) = x

tailS :: Stream a -> Stream a
tailS (Stream _ xs) = xs

takeS :: Int -> Stream a -> [a]
takeS 0 _             = []
takeS n (Stream x xs) = x : takeS (n - 1) xs

iterateS :: forall a. (a -> a) -> a -> Stream a
iterateS f = ana coalg where
  coalg :: a -> Base (Stream a) a
  coalg x = StreamF x (f x)

stream1 :: Stream Int
stream1 = iterateS (+1) 1

-- > takeS 6 stream1
-- > [1, 2, 3, 4, 5, 6]
