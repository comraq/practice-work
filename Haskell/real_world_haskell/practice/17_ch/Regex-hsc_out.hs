{-# LINE 1 "Regex-hsc.hs" #-}
{-# LANGUAGE CPP, ForeignFunctionInterface #-}
{-# LINE 2 "Regex-hsc.hs" #-}

module Regex where

import Foreign
import Foreign.C.Types


{-# LINE 9 "Regex-hsc.hs" #-}

{-
 - Since C uses regular integer flags which is translated to Cint be
 - default. However we can add an additional layer of statically checked
 - type safety via wrapping the appropriate flags in a Haskell 'newtype'.
 -
 - The following is a type for PCRE compile-time options. These are newtyped
 - CInts, which can be bitwise-or'd together using '(data.Bits..|.)'
 -}
newtype PCREOption = PCREOption { unPCREOption :: CInt }
  deriving (Show, Eq)

caseless :: PCREOption
caseless = PCREOption 1
{-# LINE 23 "Regex-hsc.hs" #-}

dollar_endonly :: PCREOption
dollar_endonly = PCREOption 32
{-# LINE 26 "Regex-hsc.hs" #-}

dotall :: PCREOption
dotall = PCREOption 4
{-# LINE 29 "Regex-hsc.hs" #-}

{-
 - 'caseless', 'dollar_endonly' and 'dotall' are three new Haskell
 - contsants, mapping to the corresponding C definitions, that's wrapped in
 - Haskell's 'newtype'
 -}
