module Regex
  ( PCREOption
  , caseless
  , dollar_endonly
  , dotall
  ) where

import Foreign
import Foreign.C.Types

#include <pcre.h>

newtype PCREOption = PCREOption { unPCREOption :: CInt }
  deriving (Show, Eq)

{-
 - 'caseless', 'dollar_endonly' and 'dotall' are three new Haskell
 - contsants, mapping to the corresponding C definitions, that's wrapped in
 - Haskell's 'newtype'
 -}
caseless :: PCREOption
caseless = PCREOption #const PCRE_CASELESS

dollar_endonly :: PCREOption
dollar_endonly = PCREOption #const PCRE_DOLLAR_ENDONLY

dotall :: PCREOption
dotall = PCREOption #const PCRE_DOTALL

