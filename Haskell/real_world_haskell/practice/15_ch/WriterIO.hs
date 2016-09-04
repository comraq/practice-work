{-# LANGUAGE MultiParamTypeClasses
           , FlexibleInstances
           , GeneralizedNewtypeDeriving
  #-}

module WriterIO where

import Control.Monad.Writer

import MonadHandle
import qualified System.IO

import System.IO (IOMode(..))
import Control.Monad.Trans (MonadIO(..), MonadTrans(..))
import System.Directory (removeFile)

import SafeHello

data Event = Open FilePath IOMode
           | Put String String
           | Close String
           | GetContents String
             deriving (Show)

newtype WriterIO a = W { runW :: Writer [Event] a }
  deriving (
    Functor
  , Applicative
  , Monad
  , MonadWriter [Event]
  )

{-
 - Implements the 'MonadHandle' type class where WriterIO will be
 - parametrized by 'FilePath'
 -
 - - All class functions will first call tell on the MonadWriter (to save
 -   the event to log instead of actually executing the action)
 -}
instance MonadHandle FilePath WriterIO where
    openFile path mode = tell [Open path mode] >> return path
    hPutStr h str      = tell [Put h str]
    hClose h           = tell [Close h]
    hGetContents h     = tell [GetContents h] >> return ""

runWriterIO :: WriterIO a -> (a, [Event])
runWriterIO = runWriter . runW
