module Monomorphism where

{-
 - Error!
 - myShow = show
 -
 - To resolve this monomorphism restriction, either:
 - 1) make the function's arguments explicit instead of implicit
 - 2) give the explicit type signature, instead of type inference by compiler
 - 3) use language extension 'NoMonomorphismRestriction'
 -}

myShow2 value = show value

myShow3 :: (Show a) => a -> String
myShow3 = show
