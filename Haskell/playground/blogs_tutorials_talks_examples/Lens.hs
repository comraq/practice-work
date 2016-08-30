{-# LANGUAGE RankNTypes, TupleSections #-}

------- Simplied Introduction to Lens -------

module Lenses where

import Utils
import Control.Monad.Identity
import Control.Applicative
import qualified Data.Map as M

{-
 - Gives access to structures, containers.
 - Access can refer to:
 - - read
 - - write
 - - modify
 - - fold
 - - traverse
 -
 - Lenses are First-Class values
 - - Lens' s a
 -   where 's' is the type of the container
 -         'a' is the type of the focus
 -   ex: Lens DateTime Mins
 -       Lens DateTime Hours
 -
 - Lenses should also be composable
 - - composeL :: Lens' s1 s2 -> Lens' s2 a -> Lens' s1 a
 -}

-- Motivation
data Person = P { name   :: String
                , addr   :: Address
                , salary :: Int
                }
  deriving (Show)

data Address = A { road       :: String
                 , city       :: String
                 , postalCode :: String
                 }
  deriving (Show)

setName :: String -> Person -> Person
setName n p = p { name = n }

setPostalCode :: String -> Person -> Person
setPostalCode pc p = p { addr = (addr p) { postalCode = pc } }

{-
 - Nested Record Updates Can be Painful!
 -
 - Want:
 - - lname   :: Lens' Person String
 - - laddr   :: Lens' Person Address
 - - lsalary :: Lens' Person Int
 -
 - - view     :: Lens' s a -> s -> a
 - - set      :: Lens' s a -> a -> s -> s
 - - composeL :: Lens' s1 s2 -> Lens' s2 a -> Lens' s1 a
 -
 - ex: setPostalCode :: String -> Person -> Person
 -     setPostalCode pc p = set (laddr `composeL` lpostalCode) pc p
 -}

-- Naive/Inefficient First Attempt

data LensR s a = L { viewR :: s -> a
                   , setR  :: a -> s -> s
                   }

composeLR :: LensR s1 s2 -> LensR s2 a -> LensR s1 a
composeLR (L v1 u1) (L v2 u2) = L (v2 . v1) (\a s -> u1 (u2 a (v1 s)) s)

-- 'over' is just 'modify', takes a lens, function and structure
-- then maps the function over that structure
overR :: LensR s a -> (a -> a) -> s -> s
overR l f s = setR l (f (viewR l s)) s

{-
 - Add a modify directly to 'LensR'. Also add effectful, may-fail methods as
 - well.
 -
 - { ...
 - , mod   :: (a -> a)       -> s -> s
 - , modM  :: (a -> Maybe a) -> s -> Maybe s
 - , modIO :: (a -> IO a)    -> s -> IO s
 - }
 -
 - Abstract the 'M' or 'IO' version to a 'Functor'
 - modF :: Functor f => (a -> f a) -> s -> f s
 -}

type Lens' s a = forall f. Functor f => (a -> f a) -> s -> f s

{-
 - It turns out Lens' and LensR are isomorphic!
 -
 - lensToLensR :: Lens' s a -> LensR s a
 - lensRToLens :: LensR s a -> Lens' s a
 -}

{-
 - Use of 'const':
 - - given the new value and then the old value, we discard the old value,
 -   return the new value
 -}
set :: Lens' s a -> a -> s -> s
set ln = runIdentity .* ln . Identity .* const

{-
 - Note the similarities between 'over' and 'set'
 - - 'set' throws away the old value, just returns the new
 -   (hence the use of const)
 - - 'over' applies the function ot the old value, returning the result of
 -   the function applied ot the old value
 -}
over :: Lens' s a -> (a -> a) -> s -> s
over ln = runIdentity .* ln . (Identity .)

{-
 - For 'view', use the 'Const' Functor defined in Control.Applicative as the
 - following:
 - - newtype Const v a = Const { getConst :: v }
 - - instance Functor (Const v) where
 -     fmap _ (Const v) = Const v
 -}
view :: Lens' s a -> s -> a
view ln = getConst . ln Const
-- view = getConst .* app . (, Const)

lensToLensR :: Lens' s a -> LensR s a
lensToLensR ln = L { viewR = view ln, setR = set ln }

data Person2 = P2 { _name   :: String
                  , _salary :: Int
                  }

-- name2 :: Functor f => (String -> f String) -> Person2 -> f Person2
name2 :: Lens' Person2 String
name2 f (P2 n s) = fmap (\n' -> P2 n' s) (f n)

{-
 - Think of 'name2' as a data structure with a hole in it.
 - ie: The Person2 data structure but with the '_name' property abstracted out
 -
 - ex: view name2 (P2 { _name = "Fred", _salary = 100 })
 -   = getConst (name Const (P {_name = "Fred", _salary = 100})
 -   = getConst (fmap (\n' -> P2 n' 100) (Const "Fred")
 -   = getConst (Const "Fred")
 -   = "Fred"
 -}

{-
 - Since Lens' is just a function, they can be composed via regular function
 - composiiton!
 -
 - Now setPostalCode can be written as:
 -   setPostalCode :: String -> Person -> Person
 -   setPostalCode pc p = set (addr . postalCode) pc p
 -     where:
 -       - addr = Lens' Person Address
 -       - postalCode = Lens' Person String
 -}
composeL' :: Lens' s1 s2 -> Lens' s2 a -> Lens' s1 a
composeL' = (.)


-- Examples:

data Temp = Temp { _fahrenheit :: Float }

centigrade :: Lens' Temp Float
centigrade f (Temp faren) = (\centi' -> Temp (cToF centi')) <$> (f . fToC $ faren)
  where cToF :: Float -> Float
        cToF = undefined
        fToC :: Float -> Float
        fToC = undefined

data Time = Time { _hours :: Int
                 , _mins  :: Int
                 }

mins :: Lens' Time Int
mins f (Time h m) = wrap <$> (f m)
  where wrap :: Int -> Time
        wrap m' | m' >= 60  = Time (h + 1) (m' - 60)
                | m < 0     = Time (h - 1) (m' + 60)
                | otherwise = Time h m'

at :: Ord k => k -> Lens' (M.Map k v) (Maybe v)
at k f m = wrap <$> (f mv)
  where mv = M.lookup k m
        wrap (Just v') = M.insert k v' m
        wrap Nothing   = case mv of
          Nothing -> m
          Just _  -> M.delete k m

-- another example: bitAt :: Bits b => Int -> Lens' b Bool

{-
 - "Multi-focus" Lens
 - - By changing the 'Functor' requirement in 'Lens'' to 'Applicative'
 -}
type Traversal' s a = forall f. Applicative f => (a -> f a) -> s -> f s

data Address2 = A2 { _road       :: String
                   , _city       :: String
                   , _postalCode :: String
                   }

lroad :: Lens' Address String
lroad f (A r c p) = (\r' -> A r' c p) <$> (f r)

-- Say we want a lens to "focus" on both '_road' and '_city'
addr_strs :: Traversal' Address2 String
addr_strs f (A2 r c p) = (\r' c' -> A2 r' c' p) <$> (f r) <*> (f c)

over' :: Traversal' s a -> (a -> a) -> s -> s
over' ln f = runIdentity . ln (Identity . f)

{-
 - 'Monoid a' is required because the 'Const' applicative is defined as:
 -   instance Monoid a => Applicative (Const a) where
 -     pure _                  = Const memtpy
 -     (Const f) <*> (Const a) = Const (f `mappend` a)
 -}
view' :: Monoid a => Traversal' s a -> s -> a
view' ln = getConst . ln Const

{-
 - Note: If 'Lens'' and 'Traversal'' were defined as newtypes, then a lot of
 -       additional work is required to compose 'Lens'' and 'Traversal''
 -}
