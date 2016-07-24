module NonDetermine where

import Control.Monad

type Name = String

data Term = Var Name
          | Con Int
          | Add Term Term
          | Lam Name Term
          | App Term Term
          | Fail
          | Amb Term Term
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

interp             :: (MonadPlus m) => Term -> Environment m -> m (Value m)
interp (Var x) e   =  lookup' x e
interp (Con i) e   =  return $ Num i
interp (Add u v) e =  interp u e >>= (\a ->
                      interp v e >>= (\b ->
                      add a b))
interp (Lam x v) e =  return $ Fun (\a -> interp v $ (x, a):e)
interp (App t u) e =  interp t e >>= (\f ->
                      interp u e >>= (\a ->
                      apply f a))
interp (Amb x y) e =  interp x e `mplus` interp y e
interp Fail e      =  mzero

lookup'              :: (Monad m) => Name -> Environment m -> m (Value m)
lookup' x []         =  return Wrong
lookup' x ((y, b):e)
  | x == y    = return b
  | otherwise = lookup' x e

add                 :: (Monad m) => Value m -> Value m -> m (Value m)
add (Num i) (Num j) =  return $ Num $ i + j
add a b             =  return Wrong

apply           :: (Monad m) => Value m -> Value m -> m (Value m)
apply (Fun k) a =  k a
apply f a       =  return Wrong





{- Sample Terms -}

term42 :: Term
term42 =  (App (Lam "x" (Add (Var "x") (Var "x")))
               (Add (Con 10) (Con 11)))

termE :: Term
termE =  (App (Con 1) (Con 2))

termF :: Term
termF =  (Lam "x" (Lam "y" (Lam "z" (Add (Var "x") 
                                         (Add (Var "y") (Var "z"))))))

termND :: Term
termND =  (App (Lam "x" (Add (Var "x") (Var "x")))
               (Amb (Con 4)
                    (Amb (Con 3)
                         (Amb (Con 2)
                              (Con 1)))))




{- Test Functions for Different Monads/Contexts -}

testND :: Term -> String
testND t = show (interp t [] :: [ Value [] ])

runMain   :: (Term -> String) -> IO ()
runMain f =  do
  putStrLn $ f term42
  putStrLn $ f termF
  putStrLn $ f termE
  putStrLn $ f Fail
  putStrLn $ f termND
