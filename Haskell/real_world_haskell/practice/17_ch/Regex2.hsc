{-# LANGUAGE CPP, ForeignFunctionInterface #-}

import Foreign
import Foreign.C.Types

#include <pcre.h>

-- Alternative newtype wrapping via the '#enum' construct
#{enum PCREOption, PCREOption
  , caseless       = PCRE_CASELESS
  , dollar_endonly = PCRE_DOLLAR_ENDONLY
  , dotall         = PCRE_DOTALL
  }
