{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module EitherTParse
  ( Parse
  , evalParse
  ) where

import EitherT
import Control.Monad.State
import Data.Int (Int64)
import qualified Data.ByteString.Lazy as L

data ParseState = ParseState {
  string :: L.ByteString
, offset :: Int64
} deriving (Show)

newtype Parse a = P {
  runP :: EitherT String (State ParseState) a
} deriving (Functor, Applicative, Monad, MonadState ParseState)

evalParse :: Parse a -> L.ByteString -> Either String a
evalParse m s = evalState (runEitherT (runP m)) (ParseState s 0)

-- Testing EitherTParse
simpleParserEither :: Parse String
simpleParserEither = return "OK"

simpleParserEitherFails :: Parse String
simpleParserEitherFails = P $ failET "ERROR in line 2"

test1 = evalParse simpleParserEither L.empty
test2 = evalParse simpleParserEitherFails L.empty
