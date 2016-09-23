module Evaluator (eval) where

import Definition
import Parser

eval :: LispVal -> LispVal
eval val@(LList lvs) = case lvs of
  [LAtom "quote",      vs] -> vs
  (LAtom "quasiquote": vs) -> LList vs

  (LAtom func : args)      -> apply func $ map eval args
  _                        -> val
eval val@_       = val

apply :: String -> [LispVal] -> LispVal
apply func args = maybe (LBool False) ($ args) $ lookup func primitives

primitives :: [(String, [LispVal] -> LispVal)]
primitives = [ ("+",         numericBinop (+) )
             , ("-",         numericBinop (-) )
             , ("*",         numericBinop (*) )
             , ("/",         numericBinop div )
             , ("mod",       numericBinop mod )
             , ("quotient",  numericBinop quot)
             , ("remainder", numericBinop rem )
             ]

numericBinop :: (SchemeNumber -> SchemeNumber -> SchemeNumber)
             -> [LispVal] -> LispVal
numericBinop op params = LNumber . foldl1 op $ map unpackNum params

unpackNum :: LispVal -> SchemeNumber
unpackNum (LNumber n) = n
unpackNum (LList [n]) = unpackNum n
unpackNum (LString n) =
  let parsed = reads n
  in  if null parsed then SInt 0 else SInt . fst $ head parsed
unpackNum _ = SInt 0
