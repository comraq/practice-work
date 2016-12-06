{-# LANGUAGE ExplicitForAll #-}

module Yoneda where

-- From the series of posts (by Bartosz Milewski) at:
-- https://www.schoolofhaskell.com/user/bartosz/understanding-yoneda


data Color = Red | Green | Blue
  deriving Show

data Note = C | D | E | F | G | A | B
  deriving Show

colorMap x = if x then Blue else Red
heatMap  x = if x then 32   else 212
soundMap x = if x then C    else G

imager :: (Bool -> r) -> [r]
imager iffie = fmap iffie [True, False, True, True]

{-
   Representable Category:
   - A category 'C' is representable if there exists a functor "F: C -> Set"
     that is naturally isomorphic to "Hom(X, -)" for some object X in 'C'
     Note: F is the representable functor
 -}

------- The Category of "Hask" -------

-- Natural Transformation from [] to Maybe
safeHead :: [a] -> Maybe a
safeHead (x:_) = Just x
safeHead _     = Nothing

{-
   let f :: a -> b

   Consider the naturality square:
     F(a) -- alpha --> G(a)
      |                 |
      |                 |
     F(f)              G(f)
      |                 |
      |                 |
      V                 V
     F(b) -- alpha --> G(b)

   Laws:
     alpha . F(f) == G(f) . alpha

   Compare with 'fmap':
     [x] ----------- safeHead --> Maybe x
      |                              |
      |                              |
  fmap f :: ([a] -> [b])         fmap f :: (Maybe a -> Maybe b)
      |                              |
      |                              |
      V                              V
     [y] ----------- safeHead --> Maybe y

   Laws:
     safeHead . fmap f == fmap f . safeHead
 -}

{-
   yoneda lemma : Functor f => forall r. (a -> r) -> f r ~ f a
       imager  ::                     (Bool -> r) -> [r] ~ [Bool]

   Note:
     imager (f . iffie) == map f (imager iffie)
 -}

test = let f = show in do
  print $ imager (f . colorMap)
  print $ map f (imager colorMap)
