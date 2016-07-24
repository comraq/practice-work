module Main where

import Ident
import Eith

{-
 - Essence of Functional Programming
 - @link - http://www.eliza.ch/doc/wadler92essence_of_FP.pdf
 -
 - Sample Abstract Syntax Tree (ADT)
 -}

type Name = String

data Term = Var Name
          | Con Int
          | Add Term Term
          | Lam Name Term
          | App Term Term
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

interp             :: (Monad m) => Term -> Environment m -> m (Value m)
interp (Var x) e   =  lookup' x e
interp (Con i) e   =  return $ Num i
interp (Add u v) e =  interp u e >>= (\a ->
                      interp v e >>= (\b ->
                      add a b))
interp (Lam x v) e =  return $ Fun (\a -> interp v $ (x, a):e)
interp (App t u) e =  interp t e >>= (\f ->
                      interp u e >>= (\a ->
                      apply f a))

lookup'              :: (Monad m) => Name -> Environment m -> m (Value m)
lookup' x []         =  fail $ "Invalid lookup: " ++ x ++ "!"
lookup' x ((y, b):e)
  | x == y    = return b
  | otherwise = lookup' x e

add                 :: (Monad m) => Value m -> Value m -> m (Value m)
add (Num i) (Num j) =  return $ Num $ i + j
add a b             =  fail $ "Invalid args to add: " ++ show a ++ " and " ++ show b ++ "!"

apply           :: (Monad m) => Value m -> Value m -> m (Value m)
apply (Fun k) a =  k a
apply f a       =  fail $ "Invalid function: " ++ show f ++ " and arguments: " ++ show a ++ " to apply!"





{- Sample Terms -}

term42 :: Term
term42 =  (App (Lam "x" (Add (Var "x") (Var "x")))
               (Add (Con 10) (Con 11)))

termE :: Term
termE =  App (Con 1) (Con 2)

termF :: Term
termF =  (Lam "x" (Lam "y" (Lam "z" (Add (Var "x") 
                                         (Add (Var "y") (Var "z"))))))




{- Test Functions for Different Monads/Contexts -}

testIdent   :: Term -> String
testIdent t =  show . runIdent $ interp t []

testEith   :: Term -> String
testEith t =  case runEith (interp t [] :: Eith (Value Eith)) of
  Left e  -> "<error: " ++ e ++ ">"
  Right s -> show s







runMain   :: (Term -> String) -> IO ()
runMain f =  do
  putStrLn $ f term42
  putStrLn $ f termF
  putStrLn $ f termE
