{-# LANGUAGE ConstraintKinds
           , FlexibleContexts
           , GADTs
           , NoImplicitPrelude
           , LambdaCase
           , PolyKinds
           , RankNTypes
           , TypeFamilies
           , TypeOperators
  #-}

module Hask where

-- @link - https://www.youtube.com/watch?v=Klwkt9oJwg0

import Prelude (Either(..), ($), Ord(..), Eq(..))

type family (~>) :: i -> i -> *
type instance (~>) = (->)
type instance (~>) = Nat
type instance (~>) = (:-)

-- :k Category
-- > Category :: (k -> k -> *) -> Constraint
class h ~ (~>) => Category h where
  id  :: h a a
  (.) :: h b c -> h a b -> h a c

instance Category (->) where
  -- id x = x
  id  = id :: a -> a

  (.) = (.) :: (b -> c) -> (a -> b) -> a -> c
  -- (.) f g x = f (g x)

-- A Natural Transformation is:
--   phi . fmap f      = fmap f      . phi
--   phi . contramap f = contramap f . phi

-- :k Nat
-- > Nat :: (k -> *) -> (k -> *) -> *

-- old:
--   newtype Nat f g = Nat { runNat :: forall a. f a -> g a }

newtype Nat f g = Nat { runNat :: forall a. f a ~> g a }

-- :k Category Nat
-- > Category Nat :: Constraint

{-
 - old:
 -   instance Category Nat where
 -
 - - Nat runs f and g with a term of type 'i' and yields a '~>' with the
 -   results of f applied with 'i' and g applied with 'i'
 - - ie: Nat contains "'j' ~> 'j'", thus the constraints requirement for
 -       Category instance of ((~>) :: j -> j -> *)
 -
 - - Note: "j ~> j" is a custom arrow from kind 'j' to 'j'
 -}
instance Category ((~>) :: j -> j -> *)
      => Category (Nat :: (i -> j) -> (i -> j) -> *)
         where
  id = Nat id
  Nat f . Nat g = Nat (f . g)


class Functor f where
  fmap :: (a ~> b) -> f a ~> f b

instance Functor (Either a) where
  fmap _ (Left a)  = Left a
  fmap f (Right b) = Right (f b)

{-
 - The kind of 'Either' takes an argument of kind '*' and gives a natural
 - transformation of '* -> *'
 -   Either :: * -> (* -> *)
 -}
instance Functor Either where
  fmap f = Nat $ \case
    Left a  -> Left (f a)
    Right b -> Right b

class Contravariant f where
  contrapmap :: (a ~> b) -> f b ~> f a

-- Dict :: Constraint -> *
data Dict p where
  Dict :: p => Dict p

-- ':-' is read as 'entails'
newtype a :- b = Sub (a => Dict b)

(\\) :: a => (b => r) -> (a :- b) -> r
r \\ Sub Dict = r

instance Category (:-) where
  id = Sub Dict
  f . g = Sub $ Dict \\ f \\ g

foo :: Ord a :- Eq a
foo = Sub Dict

bar :: Eq a :- Eq [a]
bar = Sub Dict
