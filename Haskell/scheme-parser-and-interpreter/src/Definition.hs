module Definition
  ( LispVal(..)
  , SchemeNumber(..)
  ) where

import Data.Ratio
import Data.Complex


------- Type Definitions -------

data LispVal = LAtom       String
             | LList       [LispVal]
             | LDottedList [LispVal] LispVal
             | LNumber     SchemeNumber
             | LString     String
             | LBool       Bool
             | LChar       Char
  deriving (Eq, Read)

data SchemeNumber = SInt      Integer
                  | SDouble   Double
                  | SRational Rational
                  | SComplex  (Complex Double)
  deriving (Show, Eq, Read)

instance Show LispVal where
  show = showVal

showVal :: LispVal -> String
showVal (LString contents)      = "\"" ++ contents ++ "\""
showVal (LAtom name)            = name
showVal (LNumber contents)      = show contents
showVal (LBool bool)            = showLBool bool
  where
    showLBool :: Bool -> String
    showLBool True  = "#t"
    showLBool False = "#f"

showVal (LList contents)        = "(" ++ unwordsList contents ++ ")"
showVal (LDottedList head tail) = "(" ++ unwordsList head ++ " . "
                                      ++ showVal tail     ++ ")"

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

