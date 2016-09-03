{-# LANGUAGE TupleSections #-}

import Utils

-- Our usual CustomColor type to play with
data CustomColor = CustomColor {
  red   :: Int,
  green :: Int,
  blue  :: Int
} deriving (Eq, Show, Read)

{-
 - A new type that stores a name and a function.
 - The function takes an Int, applies some computation to it, and returns an
 - Int along with a CustomColor
 -}
data FuncRec = FuncRec {
  name      :: String,
  colorCalc :: Int -> (CustomColor, Int)
}

plus5func :: CustomColor -> Int -> (CustomColor, Int)
plus5func color x = (color, x + 5)

purple :: CustomColor
purple = CustomColor 255 0 255

plus5 :: FuncRec
plus5 = FuncRec {
  name      = "plus5",
  colorCalc = plus5func purple
}

always0 :: FuncRec
always0 = FuncRec {
  name      = "always0",
  colorCalc = const (purple, 0)
}

data FuncRec2 = FuncRec2 {
  name2      :: String,
  calc2      :: Int -> Int,
  namedCalc2 :: Int -> (String, Int)
}

mkFuncRec2 :: String -> (Int -> Int) -> FuncRec2
mkFuncRec2 name calcfunc = FuncRec2 {
  name2      = name,
  calc2      = calcfunc,
  namedCalc2 = calcfunc >>> (name,)
}

plus52 :: FuncRec2
plus52 = mkFuncRec2 "plus5" (+ 5)

always02 :: FuncRec2
always02 = mkFuncRec2 "always0" $ const 0
