module Ident where

newtype Ident a = Ident { runIdent :: a }
  deriving (Eq, Ord, Read, Show)

instance Monad Ident where
  return  = Ident
  m >>= f = f $ runIdent m

instance Applicative Ident where
  pure    = return
  f <*> a = f >>= \f' -> fmap f' a

instance Functor Ident where
  fmap f = Ident . f . runIdent
