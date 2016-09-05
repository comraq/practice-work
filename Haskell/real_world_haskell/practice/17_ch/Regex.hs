{-# LINE 1 "Regex.hsc" #-}
module Regex
{-# LINE 2 "Regex.hsc" #-}
  ( PCREOption
  , caseless
  , dollar_endonly
  , dotall
  ) where

import Foreign
import Foreign.C.Types


{-# LINE 12 "Regex.hsc" #-}

newtype PCREOption = PCREOption { unPCREOption :: CInt }
  deriving (Show, Eq)

{-
 - 'caseless', 'dollar_endonly' and 'dotall' are three new Haskell
 - contsants, mapping to the corresponding C definitions, that's wrapped in
 - Haskell's 'newtype'
 -}
caseless :: PCREOption
caseless = PCREOption 1
{-# LINE 23 "Regex.hsc" #-}

dollar_endonly :: PCREOption
dollar_endonly = PCREOption 32
{-# LINE 26 "Regex.hsc" #-}

dotall :: PCREOption
dotall = PCREOption 4
{-# LINE 29 "Regex.hsc" #-}

