module Broken where

import JSONClass

instance (JSON a) => JSON [a] where
  toJValue = undefined
  fromJValue =  undefined

instance (JSON a) => JSON [(String, a)] where
  toJValue = undefined
  fromJValue = undefined

class Borked a where
  bork :: a -> String

instance Borked Int where
  bork = show

instance Borked (Int, Int) where
  bork (a, b) = bork a ++ ", " ++ bork b

instance (Borked a, Borked b) => Borked (a, b) where
  bork (a, b) = ">>" ++ bork a ++ " " ++ bork b ++ "<<"

