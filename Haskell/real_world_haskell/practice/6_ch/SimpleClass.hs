{-# LANGUAGE TypeSynonymInstances #-}

import Data.List

class Foo a where
  foo :: a -> String

instance Foo a => Foo [a] where
  foo = concat . intersperse ", " . map foo

instance Foo Char where
  foo c = [c]

instance Foo String where
  foo = id

data DataInt = D Int
  deriving (Eq, Ord, Show)

newtype NewtypeInt = N Int
  deriving (Eq, Ord, Show)

newtype UniqueID = UniqueID Int
  deriving (Eq)

data TwoFields = TwoFields Int Int

newtype Okay = ExactlyOne Int

newtype Param a b = Param (Either a b)

newtype Record = Record {
  getInt :: Int
}

{-
 - Invalid 'newtype' Declarations:
 - - newtype TooFew = TooFew
 - - newtype TooManyFields = Fields Int Int
 - - newtype TooManyConstructors = Bad Int | Worse Int
 -}

{-
 - Note: A type created with the 'data' keyword has bookkeeping cost at
 -       runtime, for example, in order to track which constructor created a
 -       value.
 -       A 'newtype' value, on the other hand, can have only one constructor
 -       and so does not need this overhead. (space and time efficient)
 -
 - Note: 'newtype' constructors are only used at compile time and does not
 -       exist at runtime. Thus, when pattern matching against 'undefined',
 -       types defined by 'data' and 'newtype' behave differently
 -       ex:
 -          - case D undefined of D _ -> 1   // 1
 -          - case   undefined of D _ -> 1   // Exception: Prelude.undefined
 -          - case N undefined of N _ -> 1   // 1
 -          - case   undefined of N _ -> 1   // 1
 -            - No exception because the constructor is not present at
 -              runtime, in other words, matching 'N _' is equivalent to
 -              just matching against '_', in which case the body (the
 -              undefined expression) is not evaluated and thus no crash
 -
 - Note: 'newtype' constructors are simply type coercisions that happen at
 -       compile time, no overhead is incurred when wrapping and unwrapping
 -       (via pattern matching) 'newtype' constructors at runtime
 -}
