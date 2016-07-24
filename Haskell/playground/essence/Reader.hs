module Reader where

import Eith

type Name = String
type Position = Int

newtype Reader env a = Reader { runReader :: env -> Eith a }

instance Monad (Reader env) where
  return x  = Reader $ \_ -> Eith $ Right x
  (Reader g) >>= f = Reader $
    \x -> case runEith (g x) of
      Left  e -> Eith $ Left e
      Right s -> runReader (f s) x

  fail   x  = Reader $ \_ -> Eith $ Left x

instance Applicative (Reader env) where
  pure    = return
  f <*> a = f >>= \f' -> fmap f' a

instance Functor (Reader env) where
  fmap f (Reader g) = Reader $ \x -> fmap f $ g x

local              :: (env -> env) -> Reader env a -> Reader env a
local f (Reader g) =  Reader $ \x -> g $ f x

ask :: Reader env env
ask =  Reader $ \x -> return x

data Term = Var Name
          | Con Int
          | Add Term Term
          | Lam Name Term
          | App Term Term
          | At Position Term
  deriving (Show)

data Value m = Wrong
             | Num Int
             | Fun (Value m -> m (Value m))

instance Monad m => Show (Value m) where
  show Wrong   =  "<wrong>"
  show (Num i) =  show i
  show (Fun f) =  "<function>"

type Environment m = [(Name, Value m)]




{- Parsing Functions -}

interp             :: Term -> Environment (Reader Position) -> (Reader Position) (Value (Reader Position))
interp (Var x) e   =  lookup' x e
interp (Con i) e   =  return $ Num i
interp (Add u v) e =  interp u e >>= (\a ->
                      interp v e >>= (\b ->
                      add a b))
interp (Lam x v) e =  return $ Fun (\a -> interp v $ (x, a):e)
interp (App t u) e =  interp t e >>= (\f ->
                      interp u e >>= (\a ->
                      apply f a))
interp (At p t) e  =  local (const p) (interp t e) 

lookup'              :: Name -> Environment (Reader Position) -> (Reader Position) (Value (Reader Position))
lookup' x []         =  ask >>= \p -> fail $ "At: " ++ show p ++ "; Invalid lookup: " ++ x ++ "!"
lookup' x ((y, b):e)
  | x == y    = return b
  | otherwise = lookup' x e

add                 :: Value (Reader Position) -> Value (Reader Position) -> (Reader Position) (Value (Reader Position))
add (Num i) (Num j) =  return $ Num $ i + j
add a b             =  ask >>= \p -> fail $ "At: " ++ show p ++ "; Invalid args to add: " ++ show a ++ " and " ++ show b ++ "!"

apply           :: Value (Reader Position) -> Value (Reader Position) -> (Reader Position) (Value (Reader Position))
apply (Fun k) a =  k a
apply f a       =  ask >>= \p -> fail $ "At: " ++ show p ++ "; Invalid function: " ++ show f ++ " and arguments: " ++ show a ++ " to apply!"





{- Sample Terms -}

term42 :: Term
term42 =  (App (Lam "x" (Add (Var "x") (Var "x")))
               (Add (Con 10) (Con 11)))

termF :: Term
termF =  (Lam "x" (Lam "y" (Lam "z" (Add (Var "x") 
                                         (Add (Var "y") (Var "z"))))))

termP :: Term
termP =  (Add (Con 1) 
              (At 42 (App (Con 2) (Con 3))))

termE :: Term
termE =  (App (Con 1) (Con 2))




{- Test Functions for Different Monads/Contexts -}

testReader   :: Term -> String
testReader t =  show $ runReader (interp t []) pos
  where pos :: Position
        pos =  0








runMain   :: (Term -> String) -> IO ()
runMain f =  do
  putStrLn $ f term42
  putStrLn $ f termF
  putStrLn $ f termP
  putStrLn $ f termE
