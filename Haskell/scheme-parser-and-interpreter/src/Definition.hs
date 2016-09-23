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

instance Num SchemeNumber where
  (+)         = addSNum
  (*)         = mulSNum
  abs         = absSNum
  signum      = signSNum
  fromInteger = SInt
  negate      = negSNum

addSNum :: SchemeNumber -> SchemeNumber -> SchemeNumber
addSNum (SInt a) (SInt b)      = SInt      $ a + b
addSNum (SInt a) (SDouble b)   = SDouble   $ fromIntegral a + b
addSNum (SInt a) (SRational b) = SRational $ fromIntegral a + b
addSNum (SInt a) (SComplex b)  = SComplex  $ fromIntegral a + b

addSNum (SDouble a) (SInt b)      = SDouble   $ a + fromIntegral b
addSNum (SDouble a) (SDouble b)   = SDouble   $ a + b
addSNum (SDouble a) (SRational b) = SRational $ fromIntegral (truncate a) + b
addSNum (SDouble a) (SComplex b)  = SComplex  $ (a :+ 0) + b

addSNum (SRational a) (SInt b)      = SRational $ a + fromIntegral b
addSNum (SRational a) (SDouble b)   = SRational $ a + fromIntegral (truncate b)
addSNum (SRational a) (SRational b) = SRational $ a + b
addSNum (SRational a) (SComplex b)  = SComplex  $ calcRatioComplex (+) a b

addSNum (SComplex a) (SInt b)      = SComplex $ a + fromIntegral b
addSNum (SComplex a) (SDouble b)   = SComplex $ a + fromIntegral (truncate b)
addSNum (SComplex a) (SRational b) = SComplex $ calcRatioComplex (+) b a
addSNum (SComplex a) (SComplex b)  = SComplex $ a + b

mulSNum :: SchemeNumber -> SchemeNumber -> SchemeNumber
mulSNum (SInt a) (SInt b)      = SInt      $ a * b
mulSNum (SInt a) (SDouble b)   = SDouble   $ fromIntegral a * b
mulSNum (SInt a) (SRational b) = SRational $ fromIntegral a * b
mulSNum (SInt a) (SComplex b)  = SComplex  $ fromIntegral a * b

mulSNum (SDouble a) (SInt b)      = SDouble   $ a * fromIntegral b
mulSNum (SDouble a) (SDouble b)   = SDouble   $ a * b
mulSNum (SDouble a) (SRational b) = SRational $ fromIntegral (truncate a) * b
mulSNum (SDouble a) (SComplex b)  = SComplex  $ (a :+ 0) * b

mulSNum (SRational a) (SInt b)      = SRational $ a * fromIntegral b
mulSNum (SRational a) (SDouble b)   = SRational $ a * fromIntegral (truncate b)
mulSNum (SRational a) (SRational b) = SRational $ a * b
mulSNum (SRational a) (SComplex b)  = SComplex  $ calcRatioComplex (*) a b

mulSNum (SComplex a) (SInt b)      = SComplex $ a * fromIntegral b
mulSNum (SComplex a) (SDouble b)   = SComplex $ a * fromIntegral (truncate b)
mulSNum (SComplex a) (SRational b) = SComplex $ calcRatioComplex (*) b a
mulSNum (SComplex a) (SComplex b)  = SComplex $ a * b

calcRatioComplex :: Num a
                 => (a -> a -> a)
                 -> Rational
                 -> Complex Double
                 -> Complex Double
calcRatioComplex op ratio complex =
  let fracDbl = rationalToDouble ratio
      rPart   = realPart complex
      iPart   = imagPart complex
  in  rPart * fracDbl :+ iPart * fracDbl

rationalToDouble :: Rational -> Double
rationalToDouble = fromRational

signSNum :: SchemeNumber -> SchemeNumber
signSNum (SComplex a)  = SInt . getSign $ realPart a
signSNum (SInt a)      = SInt $ getSign a
signSNum (SDouble a)   = SInt $ getSign a
signSNum (SRational a) = SInt $ getSign a

getSign :: (Num a, Ord a) => a -> Integer
getSign x
  | x > 0 = 1
  | x < 0 = -1
  | otherwise = 0

absSNum :: SchemeNumber -> SchemeNumber
absSNum (SInt a)      = SInt $ abs a
absSNum (SDouble a)   = SDouble $ abs a
absSNum (SRational a) = SRational $ abs a
absSNum (SComplex a)  = SComplex $ abs a

negSNum :: SchemeNumber -> SchemeNumber
negSNum (SInt a)      = SInt $ negate a
negSNum (SDouble a)   = SDouble $ negate a
negSNum (SRational a) = SRational $ negate a
negSNum (SComplex a)  = SComplex $ negate a

instance Enum SchemeNumber where
  toEnum = SInt . fromIntegral

  fromEnum (SInt a)      = fromIntegral a
  fromEnum (SDouble a)   = truncate a
  fromEnum (SRational a) = truncate a
  fromEnum (SComplex a)  = truncate $ realPart a

instance Real SchemeNumber where
  toRational (SInt a)      = a % 1
  toRational (SDouble a)   = truncate a % 1
  toRational (SRational a) = a
  toRational (SComplex a)  = truncate (realPart a) % 1

instance Ord SchemeNumber where
  compare = undefined -- TODO

instance Integral SchemeNumber where
  quotRem = undefined -- TODO

  toInteger (SInt a)      = a
  toInteger (SDouble a)   = truncate a
  toInteger (SRational a) = truncate a
  toInteger (SComplex a)  = truncate $ realPart a
