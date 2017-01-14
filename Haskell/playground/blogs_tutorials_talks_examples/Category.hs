{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
-- {-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
-- {-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Category where

-- @link - http://www.stephendiehl.com/posts/monads.html

import GHC.Exts (Constraint)
import Prelude (Maybe(..))

{-
 - In a category, there is a collection of:
 - > object    : *
 - > morphisms : * -> *
 -
 - with id morphism defined for all objects and an associative composition
 - operation satsifying the respective laws
 -}

-- Morphisms
type (a ~> b) c = c a b

class Category (c :: k -> k -> *) where
  id  :: (a ~> a) c
  (.) :: (y ~> z) c -> (x ~> y) c -> (x ~> z) c

type Hask = (->)

instance Category Hask where
  id x = x
  (f . g) x = f (g x)

class (Category c, Category d) => Functor c d f where
  fmap :: (a ~> b) c -> (f a ~> f b) d

newtype Id a = Id a

instance Functor Hask Hask Id where
  fmap f (Id a) = Id (f a)

instance Functor Hask Hask [] where
  fmap f (x:xs) = f x : fmap f xs
  fmap _ _      = []

instance Functor Hask Hask Maybe where
  fmap f (Just x) = Just (f x)
  fmap _ _        = Nothing

type Endofunctor c f = Functor c c f

newtype FComp g f x = C { unC :: g (f x) }
newtype Hom (c :: * -> Constraint) a b = Hom (a -> b)

instance (Functor a b f, Functor b c g, c ~ Hom k) => Functor a c (FComp g f) where
  fmap f = Hom C . fmapg (fmapf f) . Hom unC
    where
      fmapf = fmap :: (x ~> y) a -> (f x ~> f y) b
      fmapg = fmap :: (s ~> t) b -> (g s ~> g t) c

type Nat c f g = forall a. (f a ~> g a) c

type NatHask f g = forall a. f a -> g a

headMaybe :: [a] -> Maybe a
headMaybe (x:_) = Just x
headMaybe _     = Nothing

-- The Functor Category "Func(C,D)" where objects are functors from C to D
-- and arrows are natural transformations
newtype Func f g a b = FNat (f a -> g b)

-- Endofunctor Category
type Endo f = Func f f

instance Category (Endo f) where
  -- The identity natural transformation
  id = FNat id

  -- Vertical composition of natural transformations
  FNat f . FNat g = FNat (f . g)

class Endofunctor c f => Monad c f where
  eta :: (a ~> f a) c
  mu  :: (f (f a) ~> f a) c

(>>=) :: Monad c f => (a ~> f b) c -> (f a ~> f b) c
(>>=) f = mu . fmap f

return :: Monad c f => (a ~> f a) c
return = eta

-- Kleisli category
newtype Kleisli c f a b = K ((a :~> b) c f)

-- Kleisli morphisms ((a ~> f b) c)
type (a :~> b) c f = (a ~> f b) c

-- Kleisli morphism composition
(<=<) :: Monad c f => (y :~> z) c f -> (x :~> y) c f -> (x :~> z) c f
f <=< g = mu . fmap f . g

instance Monad c f => Category (Kleisli c f) where
  id = K eta
  K f . K g = K (f <=< g)
