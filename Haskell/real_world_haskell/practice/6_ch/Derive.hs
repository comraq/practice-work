module Derive where

data Color = Red | Blue | Green
  deriving (Show)

data CannotShow = CannotShow
  deriving (Show)

data CannotDeriveShow = CannotDeriveShow CannotShow
  deriving (Show)

data OK = OK

instance Show OK where
  show _ = "OK"

data ThisWorks = ThisWorks OK
  deriving (Show)
