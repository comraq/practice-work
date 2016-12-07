{-# LANGUAGE ScopedTypeVariables
           , TypeFamilies
           , FlexibleContexts
           , ViewPatterns
  #-}

module Annotate where

import Control.Arrow ((&&&))
import Control.Comonad.Cofree (Cofree(..), unwrap)
import qualified Control.Comonad.Trans.Cofree as CCTC
import Data.Foldable (sum)
import Data.Functor.Foldable
import Text.PrettyPrint.ANSI.Leijen (Doc, Pretty, (<+>), text, parens, pretty)

import Expression

{-
   The definitions of CofreeF and Cofree:
     data CoFree f a    = a :< (f (Cofree f a))
     data CoFreeF f a b = a :< (f b)

     type instance Base (Cofree f a) = CofreeF f a
 -}

-- Annotate "f (Ann f a)" with attribute "a"
--   Ann f a = a :< (f (Ann f a))
type Ann = Cofree

-- Attribute of the root node
attr :: Ann f a -> a
attr (a :< _) = a

-- Strip attribute from root
strip :: Functor f => Ann f a -> f (Ann f a)
strip = unwrap

-- Strip all attributes
stripAll :: forall f a. Functor f => Ann f a -> Fix f
stripAll = cata alg where
  alg :: Base (Ann f a) (Fix f) -> Fix f
  alg (_ CCTC.:< f) = Fix f

-- Annotation constructor
ann :: (a, f (Ann f a)) -> Ann f a
ann = uncurry (:<)

-- Annotation deconstructor
unAnn :: Ann f a -> (a, f (Ann f a))
unAnn (a :< f) = (a, f)

synthesize :: forall t f a. (Recursive t, Base t ~ f) => (Base t a -> a) -> t -> Ann f a
synthesize f = cata alg where
  alg :: Base t (Ann f a) -> Ann f a
  alg = ann . (f . fmap attr &&& id)

sizes :: (Base t ~ f, Foldable f, Recursive t) => t -> Ann f Int
sizes = synthesize $ (+1) . sum

pprAnn :: forall a. Pretty a => Ann ExprF a -> Doc
pprAnn = cata alg where
  alg :: Base (Ann ExprF a) Doc -> Doc
  alg (a CCTC.:< d) = pprAlg d <+> text "@" <+> pretty a


-- > pprAnn $ sizes expr1
-- > ((ifNeg (1 @ 1 * a @ 1) @ 3 then (1 @ 1 * a @ 1) @ 3 else (b @ 1 + 0 @ 1) @ 3) @ 10 * 3 @ 1) @ 12

{-
   'inherit' "top down" catamorphism with 'para',
   much like implementing 'foldl' with 'foldr',
   since para can be implemented with 'cata'
   > foldl :: Foldable f => (b -> a -> b) -> b -> f a -> b
   > foldl f = flip $ foldr (\a g -> n -> f (g n) a) id


   'inherit' is same as 'foldl' except the HO function is fliped,
   ie: from Foldable f => (b -> a -> b) -> b -> f a -> b
       to   Foldable f => (a -> b -> b) -> b -> f a -> b

   compare with:
                 (...) => (t -> a -> a) -> a -> t   -> Ann f a
 -}
inherit :: forall t f a. (Base t ~ f, Corecursive t, Recursive t)
        => (t -> a -> a)
        -> a
        -> t
        -> Ann f a
inherit f root n = para alg n root where
  alg :: Base t (t, a -> Ann f a) -> a -> Ann f a
  alg (distPara -> (n, ff)) p = ann (a, n')
    where
      a :: a
      a = f n p

      n' :: Base t (Ann f a)
      n' = fmap ($ a) ff

depths :: (Base t ~ f, Recursive t, Corecursive t) => t -> Ann f Int
depths = inherit (const (+1)) 0

-- > pprAnn $ depths expr1
-- > ((ifNeg (1 @ 4 * a @ 4) @ 3 then (1 @ 4 * a @ 4) @ 3 else (b @ 4 + 0 @ 4) @ 3) @ 2 * 3 @ 2) @ 1
