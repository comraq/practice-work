{-# LANGUAGE CPP, ForeignFunctionInterface #-}

module Regex (PCREOption) where

import Foreign
import Foreign.C.Types

#include <pcre.h>

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

-- Alternative newtype wrapping via the '#enum' construct
#{enum PCREOption, PCREOption
  , caseless       = PCRE_CASELESS
  , dollar_endonly = PCRE_DOLLAR_ENDONLY
  , dotall         = PCRE_DOTALL
  }

-- Combine a list of options into a single option, using bitwise (.|.)
combineOptions :: [PCREOption] -> PCREOption
combineOptions = PCREOption . foldr ((.|.) . unPCREOption) 0
