{-# LINE 1 "Regex2.hsc" #-}
{-# LANGUAGE CPP, ForeignFunctionInterface #-}
{-# LINE 2 "Regex2.hsc" #-}

import Foreign
import Foreign.C.Types


{-# LINE 7 "Regex2.hsc" #-}

-- Alternative newtype wrapping via the '#enum' construct
caseless        :: PCREOption
caseless        = PCREOption 1
dollar_endonly  :: PCREOption
dollar_endonly  = PCREOption 32
dotall          :: PCREOption
dotall          = PCREOption 4

{-# LINE 14 "Regex2.hsc" #-}
