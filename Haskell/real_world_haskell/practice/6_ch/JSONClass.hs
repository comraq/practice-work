{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module JSONClass where

import Control.Arrow (second)

{-
 - data Maybe a = Nothing | Just a
 -   deriving (Eq, Ord, Read, Show)
 -
 - data Either a b = Left a | Right b
 -   deriving (Eq, Ord, Read, Show)
 -}

type JSONError = String

data JValue = JString String
            | JNumber Double
            | JBool Bool
            | JNull
            | JObject (JObj JValue)   -- was [(String, JValue)]
            | JArray (JAry JValue)    -- was [JValue]
              deriving (Eq, Ord, Show)

class JSON a where
  toJValue :: a -> JValue
  fromJValue :: JValue -> Either JSONError a

instance JSON JValue where
  toJValue = id
  fromJValue = Right

instance JSON Bool where
  toJValue = JBool

  fromJValue (JBool b) = Right b
  fromJValue _         = Left "Not a JSON boolean!"

instance JSON String where
  toJValue = JString

  fromJValue (JString s) = Right s
  fromJValue _           = Left "Not a JSON string!"

doubleToJValue :: (Double -> a) -> JValue -> Either JSONError a
doubleToJValue f (JNumber v) = Right (f v)
doubleToJValue _ _           = Left "Not a JSON number!"

instance JSON Int where
  toJValue = JNumber . realToFrac
  fromJValue = doubleToJValue round

instance JSON Integer where
  toJValue = JNumber . realToFrac
  fromJValue = doubleToJValue round

instance JSON Double where
  toJValue = JNumber
  fromJValue = doubleToJValue id

newtype JAry a = JAry {
  fromJAry :: [a]
} deriving (Eq, Ord, Show)

jaryFromJValue :: (JSON a) => JValue -> Either JSONError (JAry a)
jaryFromJValue (JArray (JAry a)) = whenRight JAry (mapEithers fromJValue a)
jaryFromJValue _                 = Left "Not a JSON array!"

jaryToJValue :: (JSON a) => JAry a -> JValue
jaryToJValue = jaryOfJValuesToJValue . jvaluesToJAry . listToJValues . fromJAry

instance (JSON a) => JSON (JAry a) where
  toJValue = jaryToJValue
  fromJValue = jaryFromJValue

listToJValues :: (JSON a) => [a] -> [JValue]
listToJValues = map toJValue

jvaluesToJAry :: [JValue] -> JAry JValue
jvaluesToJAry = JAry

jaryOfJValuesToJValue :: JAry JValue -> JValue
jaryOfJValuesToJValue = JArray

whenRight :: (b -> c) -> Either a b -> Either a c
whenRight _ (Left err) = Left err
whenRight f (Right a)  = Right (f a)

mapEithers :: (a -> Either b c) -> [a] -> Either b [c]
mapEithers f (x:xs) = case mapEithers f xs of
  Left err -> Left err
  Right cs -> case f x of
    Left err -> Left err
    Right c  -> Right (c:cs)
mapEithers _ _      = Right []

newtype JObj a = JObj {
  fromJObj :: [(String, a)]
} deriving (Eq, Ord, Show)

instance (JSON a) => JSON (JObj a) where
  toJValue = JObject . JObj . map (second toJValue) . fromJObj

  fromJValue (JObject (JObj o)) = let unwrap (k, v) = whenRight ((,) k) (fromJValue v)
                                  in whenRight JObj (mapEithers unwrap o)
  fromJValue _ = Left "Not a JSON object!"
{-
 - Control.Arrow.second :: (Arrow a) => a b c -> a (d, b) (d, c)
 - - maps a function (arrow) :: b -> c to (a, b) -> (a, c)
 -   ie: transforms the original function to a function accepting a tuple which
 -       only applies the original function to the second element of the tuple
 -
 - (,) :: a -> b -> (a, b)
 - - takes a, then b, then returns a tuple of (a, b)
 -
 - (,,) :: a -> b -> (a, b, c)
 - - same as (,) but operates on a third argument, returning a triple
 -}
