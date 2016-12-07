{-# LANGUAGE TypeFamilies
           , ScopedTypeVariables
           , DeriveFunctor
           , DeriveFoldable
           , DeriveTraversable
           , ViewPatterns
           , FlexibleContexts
  #-}

module Expression where

import Control.Arrow (first, second, app, (>>>), (&&&), (***), (|||))
import Data.Bool (bool)
import qualified Data.Foldable as F (fold)
import Data.Functor.Foldable
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import qualified Data.Set as S
import Text.PrettyPrint.ANSI.Leijen (Doc, (<+>), text, parens)


type Id  = String
type Env = M.Map Id Int

data ExprF r = ConstF Int
             | VarF   Id
             | AddF   r   r
             | MulF   r   r
             | IfNegF r   r   r
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

data Expr = Const Int
          | Var   Id
          | Add   Expr Expr
          | Mul   Expr Expr
          | IfNeg Expr Expr Expr
  deriving (Show, Eq, Ord)

type instance Base Expr = ExprF

instance Recursive Expr where
  project (Const n)     = ConstF n
  project (Var v)       = VarF v
  project (Add a b)     = AddF a b
  project (Mul a b)     = MulF a b
  project (IfNeg a b c) = IfNegF a b c

rProject :: Base Expr Expr -> Expr
rProject (ConstF n)     = Const n
rProject (VarF v)       = Var v
rProject (AddF a b)     = Add a b
rProject (MulF a b)     = Mul a b
rProject (IfNegF a b c) = IfNeg a b c

instance Corecursive Expr where
  embed = rProject

expr1 :: Expr
expr1 = Mul (IfNeg
              (Mul (Const 1)
                   (Var "a"))
              (Add (Var "b")
                   (Const 0))
              (Add (Var "b")
                   (Const 2)))
             (Const 3)

env1 :: Env
env1 = M.fromList [("a", 1), ("b", 3)]


-- > eval env1 expr1
-- > Just 9

eval :: Env -> Expr -> Maybe Int
eval = cata . evalAlg

evalAlg :: Env -> Base Expr (Maybe Int) -> Maybe Int
evalAlg env = alg where
  alg (ConstF n)     = pure n
  alg (VarF v)       = M.lookup v env
  alg (AddF a b)     = (+) <$> a <*> b
  alg (MulF a b)     = (*) <$> a <*> b
  alg (IfNegF a b c) = a >>= bool b c . (< 0)


-- > ppr expr1
-- > ((ifNeg (1 * a) then (1 * a) else (b + 0)) * 3)

ppr :: Expr -> Doc
ppr = cata pprAlg

pprAlg :: Base Expr Doc -> Doc
pprAlg (ConstF n)     = text $ show n
pprAlg (VarF v)       = text v
pprAlg (AddF a b)     = parens $ a <+> text "+" <+> b
pprAlg (MulF a b)     = parens $ a <+> text "*" <+> b
pprAlg (IfNegF a b c) = parens $ text "ifNeg"   <+> a
                          <+> text "then" <+> a
                          <+> text "else" <+> b


-- > freeVars expr1
-- > fromList ["a","b"]

freeVars :: Expr -> S.Set Id
freeVars = cata alg where
  alg :: Base Expr (S.Set Id) -> S.Set Id
  alg (VarF v) = S.singleton v
  alg e        = F.fold e



sub1 :: M.Map Id Expr
sub1 = M.fromList [("b", Var "a")]

-- > freeVars $ substitute sub1 expr1
-- > fromList ["a"]

substitute :: M.Map Id Expr -> Expr -> Expr
substitute env = cata alg where
  alg :: Base Expr Expr -> Expr
  alg e@(VarF v) = fromMaybe (Var v) $ M.lookup v env
  alg e          = rProject e


-- Composing Algebras

optAdd :: Base Expr Expr -> Expr
optAdd (AddF (Const 0) e) = e
optAdd (AddF e (Const 0)) = e
optAdd e                  = rProject e

optMul :: Base Expr Expr -> Expr
optMul (MulF (Const 1) e) = e
optMul (MulF e (Const 1)) = e
optMul e                  = rProject e

composeSlow :: Expr -> Expr
composeSlow = cata optAdd . cata optMul

-- Law:
--   cata f . cata g == cata (f `compRecursive` g)

compRecursive :: Recursive b => (Base b b -> c) -> (a -> b) -> a -> c
f `compRecursive` g = f . project . g

composeFast :: Expr -> Expr
composeFast = cata (optMul `compRecursive` optAdd)

funzip :: Functor f => f (a, b) -> (f a, f b)
funzip = fmap fst &&& fmap snd

algProd :: Functor f => (f a -> a) -> (f b -> b) -> f (a, b) -> (a, b)
algProd f g = (f *** g) . funzip

algCoprod :: (f a -> a) -> (g a -> a) -> Either (f a) (g a) -> a
algCoprod = (|||)


-- > traced env1 expr1
-- > [(2,Just 2),(3,Just 3),(a,Just 1),(b,Just 3),((b + 2),Just 5),(((ifNeg a then a else b) * 3),Just 9),((ifNeg a then a else b),Just 3)]

traced :: Env -> Expr -> [(Doc, Maybe Int)]
traced = curry
  $   ((cataTrace . evalAlg) *** composeFast)
  >>> app
  >>> M.toList
  >>> map (first ppr)

cataTrace :: forall t a. (Corecursive t, Recursive t, Ord t, Foldable (Base t))
          => (Base t a -> a)
          -> t
          -> M.Map t a
cataTrace alg = para phi where
  phi :: Base t (t, M.Map t a) -> M.Map t a
  phi (distPara -> (t, m)) = M.insert k v m'
    where k  = t                               -- Use the structure as key to the M.Map
          v  = alg $ fmap (m' M.!) (project t) -- Project structure 't' to fixed point 'Base t t',
                                               --   fmap over (Base t) by using the 't' value as map index,
                                               --   then apply 'alg' to obtain new value
          m' = F.fold m                        -- Fold nested M.Maps underneath the current structure into one

