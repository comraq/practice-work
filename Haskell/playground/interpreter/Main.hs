module Main where

{-
 - Following an interpreter tutorial
 - @link - https://wiki.haskell.org/wikiupload/c/c6/ICMI45-paper-en.pdf
 -}

data Exp =  Constant Int
           | Variable String
           | Minus Exp Exp
           | Greater Exp Exp
           | Times Exp Exp
           deriving Show

data Com =  Assign String Exp
           | Seq Com Com
           | Cond Exp Com Com
           | While Exp Com
           | Declare String Exp Com
           | Print Exp
           deriving Show

s1 = Declare "x" (Constant 150)
       (Declare "y" (Constant 200)
         (Seq (While (Greater (Variable "x") (Constant 0))
                     (Seq (Assign "x" (Minus (Variable "x")
                                             (Constant 1)))
                          (Assign "y" (Minus (Variable "y")
                                             (Constant 1)))))
              (Print (Variable "y"))))

type Location = Int
type Index = [String]
type Stack = [Int]

-- Finds the 'Int' Location of name in the Index [String], starting from 1
position            :: String -> Index -> Location
position name index =  let pos n (nm:nms) = if name == nm
                                              then n
                                              else pos (n+1) nms
                       in  pos 1 index

-- Fetches the variable from the Stack when given a location
fetch          :: Location -> Stack -> Int
fetch n (v:vs)
  | n == 1    = v
  | otherwise = fetch (n-1) vs

-- Puts the Int value onto the stack at Location
put            :: Location -> Int -> Stack -> Stack
put n x (v:vs)
  | n == 1    = x:vs
  | otherwise = v:(put (n-1) x vs)

newtype M a = StOut (Stack -> (a, Stack, String))

instance Functor M where
  fmap f (StOut g) = StOut $ \n -> let (a, n1, s1) = g n
                                   in  (f a, n1, s1)

instance Applicative M where
  pure    = return
  f <*> a = f >>= \f' -> fmap f' a

instance Monad M where
  return x = StOut $ \n -> (x, n, "")
  e >>= f  = StOut $ \n -> let unStOut (StOut f) = f
                               (a, n1, s1) = (unStOut e) n
                               (b, n2, s2) = unStOut (f a) n1
                           in  (b, n2, s1 ++ s2)

-- Produces a StOut Monad instance with value at Location 'i' fetched from
-- the Stack
getFrom   :: Location -> M Int
getFrom i =  StOut $ \ns -> (fetch i ns, ns, "")

-- An effectful write of updating the Stack, yields no meaningful value
-- (unit ())
write     :: Location -> Int -> M ()
write i v =  StOut $ \ns -> ((), put i v ns, "")

-- Same as write excepct just adds the value to the top of the Stack
push   :: Int -> M ()
push x =  StOut $ \ns -> ((), x:ns, "")

-- Pops the last value from the top of the stack
pop :: M ()
pop =  StOut $ \ns -> let (n:ns') = ns
                      in  ((), ns', "")

-- Expression Evaluator
eval1           :: Exp -> Index -> M Int
eval1 exp index =  case exp of
  Constant n   -> return n
  Variable x   -> let loc = position x index
                  in  getFrom loc
  Minus x y    -> do 
                     a <- eval1 x index
                     b <- eval1 y index
                     return $ a - b
  Greater x y  -> do 
                     a <- eval1 x index
                     b <- eval1 y index
                     return $ if a > b then 1 else 0
  Times x y    -> do 
                     a <- eval1 x index
                     b <- eval1 y index
                     return $ a * b

interpret1            :: Com -> Index -> M ()
interpret1 stmt index =  case stmt of
  Assign name e     -> let loc = position name index
                       in  do
                              v <- eval1 e index
                              write loc v
  Seq s1 s2         -> do
                          interpret1 s1 index
                          interpret1 s2 index
                          return ()
  Cond e s1 s2      -> do
                          x <- eval1 e index
                          if x == 1
                            then interpret1 s1 index
                            else interpret1 s2 index
  While e b         -> let loop () =
                             do
                                v <- eval1 e index
                                if v == 0
                                  then return ()
                                  else do
                                          interpret1 b index
                                          loop ()
                       in  loop ()
  Declare nm e stmt -> do
                          v <- eval1 e index
                          push v
                          interpret1 stmt (nm:index)
                          pop
  Print e           -> do
                          v <- eval1 e index
                          output v

output   :: (Show a) => a -> M ()
output v =  StOut $ \n -> ((), n, show v)

test   :: Exp -> (Int, Stack, String)
test a =  let (StOut g) = eval1 a []
          in  g []

interp   :: Com -> ((), Stack, String)
interp a =  let (StOut g) = interpret1 a []
            in  g []
