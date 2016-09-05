module ApplicativeParsec
  ( module Control.Applicative
  , module Text.ParserCombinators.Parsec
  ) where

import Control.Applicative
import Control.Monad (MonadPlus(..), ap)

import Text.ParserCombinators.Parsec hiding (many, optional, (<|>))

{-
 - Examples of how 'GenParser' implements 'Applicative' and 'Alternative'
 -
 - -- The Applicative instance for every Monad looks like this
 - instance Applicative (GenParser s a) where
 -   pure  = return
 -   (<*>) = ap
 -
 - -- The Alternative instance for every MonadPlus looks like this
 - instance Alternative (GenParser s a) where
 -   empty = mzero
 -   (<|>) = mplus
 -}
