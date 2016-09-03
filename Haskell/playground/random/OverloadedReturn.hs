{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}

-- @link - http://stackoverflow.com/questions/7828072/how-does-haskell-printf-work

foo :: FooType a => a
foo = bar $ return ()

class FooType a where
  bar :: IO () -> a

instance FooType (IO ()) where
  -- bar :: IO () -> IO ()
  bar = id

instance (Show x, FooType r) => FooType (x -> r) where
  -- bar :: IO () -> (x -> r)
  bar s x = bar (s >> print x)



-- @link - http://chris-taylor.github.io/blog/2013/03/01/how-haskell-printf-works/
data UPrintf = UStr String
             | UInt Integer

uprintf :: String -> [UPrintf] -> String
uprintf ""         []     = ""
uprintf ""         _      = fmterror
uprintf ('%':_:_)  []     = argerror
uprintf ('%':c:cs) (u:us) = fmt c u ++ uprintf cs us
uprintf (c:cs)     us     = c : uprintf cs us


fmt :: Char -> UPrintf -> String
fmt 'd' u = asint u
fmt 's' u = asstr u

asint :: UPrintf -> String
asint (UInt i) = show i
asint (UStr _)  = typeerror "Integer" "String"

asstr :: UPrintf -> String
asstr (UStr s)  = s
asstr (UInt _) = typeerror "String" "Integer"

typeerror t1 t2 = error $ "Type error: expected " ++ t1 ++ ", got " ++ t2 ++ "."
fmterror        = error "Reached end of format string with args remaining."
argerror        = error "Insufficient args for format string."

class PrintfArg a where
  toUPrintf :: a -> UPrintf

instance PrintfArg String where
  toUPrintf = UStr

instance PrintfArg Integer where
  toUPrintf = UInt

class PrintfType t where
  spr :: String -> [UPrintf] -> t

instance PrintfType String where
  spr formatString args = uprintf formatString $ reverse args

instance PrintfType (IO ()) where
  spr formatString args = putStrLn $ uprintf formatString $ reverse args

instance (PrintfArg a, PrintfType t) => PrintfType (a -> t) where
  {-
   - The 'spr' function of this instance consumes an argument of type 'PrintfArg a'
   - and should return a function that is ready to consume the next
   - argument of type 'PrintfArg a' and will return another value of type
   - 'PrintfType t'.
   -   - ie: if the return type is another type of the current instance,
   -         this effectively becomes a pseudo variadic function
   -}
  spr fmt args a = spr fmt (toUPrintf a : args)

printf :: PrintfType t => String -> t
printf formatString = spr formatString []

{-
 - Examples:
 -  > printf "%s%d" "abc" 7777 :: String
 -  > "abc7777"
 -
 -  > printf "%s%d" "abc" 7777 :: IO ()
 -  > abc7777
 -}
