{-# LANGUAGE DatatypeContexts #-}

data Foo a = Foo a

instance Functor Foo where
  fmap f (Foo a) = Foo $ f a

data Eq a => Bar a = Bar a

{-
 - Note: Bar cannot be made an instance of 'Functor' because 'fmap' does not
 -       enforce the parametrized value to be an instance of the 'Eq' typeclass
 -
 - instance Functor Bar where
 -   fmap f (Bar a) = Bar $ f a
 -}

{-# LANGUAGE FlexibleInstances #-}
instance Functor (Either Int) where
  fmap _ (Left n)  = Left n
  fmap f (Right r) = Right $ f r

{-
 - Functor Laws:
 - - fmap id      == id
 - - fmap (f . g) == fmap f . fmap g
 -}
