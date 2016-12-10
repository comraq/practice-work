{-# LANGUAGE GADTs
           , StandaloneDeriving
           , DataKinds
           , TypeOperators
  #-}

{-# OPTIONS_GHC -Wall #-}

module Gadts where

-- @link - https://github.com/goldfirere/glambda

-- Note: '~' is Haskell's symbol for type equalities

newtype Wrap a = Wrap { unwrap :: a }
  deriving Show

data STy ty where
  SInt   :: STy Int
  SBool  :: STy Bool
  SMaybe :: STy ty' -> STy (Maybe ty')
  SWrap  :: STy a   -> STy (Wrap a)
  SList  :: STy a   -> STy [a]
  SUnit  :: STy ()
  SFArr  :: STy a   -> STy b        -> STy (a -> b)

deriving instance Show (STy ty)

{-
 - Same as (need "ExistentialQuantification" language extension):
 - data STy ty = (ty ~ Int)  => SIng
 -             | (ty ~ Bool) => SBool
 -             | forall ty'. (ty ~ Maybe ty') => SMaybe (STy ty')
 -}

-- Pattern matching a "term" in 'zero' reveals "type" information
zero :: STy ty -> ty
zero SInt        = 0        -- GHC: I know ty ~ Int
zero SBool       = False    -- GHC: I know ty ~ Bool
zero (SMaybe _)  = Nothing  -- GHC: I know ty ~ (Maybe ty')
zero (SWrap a)   = Wrap (zero a)
zero (SList _)   = []
zero SUnit       = ()
zero (SFArr _ b) = const (zero b)

-- Note: GADTs often require explicit type signatures
-- ex: if the type signature of 'zero' is omitted, type checker will fail
--     Both of the following types are valid for "zero SBool = False":
--       zero :: STy ty -> Bool
--       zero :: STy ty -> ty

{-
 - The below will trigger warning due to non-exhaustive pattern match:
 -   styEq :: STy ty -> STy ty -> Bool
 -   styEq SInt SInt = True
 -}

------- H(eterogenous) Lists -------

data HList tys where
  HNil :: HList '[]
  (:>) :: h -> HList t -> HList (h ': t)

infixr 5 :>

{-
 - Example:
 - > (True :> () :> [Just 'x'] :> "hi" :> HNil)
 - > HList '[Bool, (), [Maybe Char], [Char]]
 -
 - (:>) creates a new HList where 'tys' is a list type that can contain
 - different types
 -
 - Note:
 -   ' in types marks promoted types, enabled by 'DataKinds' language extension
 -}

{-
 - Naive attempt to extract an element from 'HList':
 - (does not work, also requires 'PartialTypeSignatures' language extension)
 -   partialhget :: Int -> HList tys -> _
 -   partialhget 0 (x :> xs) = x
 -}

data Elem list elt where
  -- EZ: Element Zero
  --   "elt x" is an element of "list x ': xs"
  EZ :: Elem (x ': xs) x

  -- ES: Element Successor
  --   x is an element of "xs" => x is the successor element of "y ': xs"
  ES :: Elem xs x -> Elem (y ': xs) x

{-
 - Example:
 - > :t EZ :: Elem '[Bool, Int] Bool
 - > EZ :: Elem '[Bool, Int] Bool :: Elem '[Bool, Int] Bool
 -
 - > :t ES EZ :: Elem '[Bool, Int] Int
 - > ES EZ :: Elem '[Bool, Int] Int :: Elem '[Bool, Int] Int
 -}

hlistget :: Elem tys ty -> HList tys -> ty
hlistget EZ      (hd :> _ ) = hd
hlistget (ES el) (_  :> tl) = hlistget el tl

{-
 - Example:
 - > hlistget EZ (True :> "hello" :> Just (5 :: Int) :> HNil)
 - > True
 -
 - > hlistget (ES EZ) (True :> "hello" :> Just (5 :: Int) :> HNil)
 - > "hello"
 -}
