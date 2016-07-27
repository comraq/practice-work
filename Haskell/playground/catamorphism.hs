{- Algebras -}

-- Iteration

type StepAlgebra b = (b, b -> b)
data Nat = Zero | Succ Nat

foldSteps                        :: StepAlgebra b -> (Nat -> b)
foldSteps (nil, next) Zero       =  nil
foldSteps (nil, next) (Succ nat) =  next $ foldSteps (nil, next) nat



-- List Fold

type ContainerAlgebra a b = (b, a -> b -> b)
data List a = Nil | Cons a (List a)

foldrList                          :: ContainerAlgebra a b -> (List a -> b)
foldrList (nil, merge) Nil         =  nil
foldrList (nil, merge) (Cons x xs) =  merge x $ foldrList (nil, merge) xs



-- Tree Fold

type TreeAlgebra a b = (a -> b, b -> b -> b)
data Tree a = Leaf a | Branch (Tree a) (Tree a)

foldTree                            :: TreeAlgebra a b -> (Tree a -> b) 
foldTree (f, g) (Leaf x)            =  f x
foldTree (f, g) (Branch left right) =  g (foldTree (f, g) left) (foldTree (f, g) right)

treeDepth :: TreeAlgebra a Integer
treeDepth =  (const 1, \i j -> 1 + max i j)

treeSum :: (Num a) => TreeAlgebra a a
treeSum =  (id, (+))



-- Cata

type Algebra f a = f a -> a
newtype Fix f = Iso { invIso :: f (Fix f) }
newtype Mu f  = InF { outF :: f (Mu f) }

cata     :: (Functor f) => Algebra f a -> (Fix f -> a)
cata alg =  alg . fmap (cata alg) . invIso

cata'   :: (Functor f) => Algebra f a -> Mu f -> a
cata' f =  f . fmap (cata' f) . outF

{-
 - cata f = hylo f outF
 - cata f = para (f . fmap fst)
 -}




{- Test Cases -}

testStep :: Nat
testStep =  (Succ . Succ . Succ . Succ $ Zero)

testStepAlgebra :: StepAlgebra String
testStepAlgebra =  ("go!", \s -> "wait... " ++ s)


testList :: List Int
testList =  (Cons 10 $ Cons 100 $ Cons 1000 Nil)

testListAlgebra :: ContainerAlgebra Int Int
testListAlgebra =  (3, \x y -> x * y)


testTree :: Tree Int
testTree =  (Branch (Leaf 1)
                    (Branch (Leaf 2)
                            (Branch (Leaf 3) (Leaf 4))))

testTreeAlgebra :: TreeAlgebra Int String
testTreeAlgebra =  (\x -> "Leaf " ++ show x, \x y -> x ++ " and " ++ y)


type Nat' = Fix Maybe

zero  :: Nat'
zero  =  Iso Nothing

succ' :: Nat' -> Nat'
succ' =  Iso . Just

pleaseWait          :: Algebra Maybe String
pleaseWait (Just s) =  "wait.. " ++ s
pleaseWait Nothing  =  "go!"

testNat' :: Nat'
testNat' =  succ' . succ' . succ' . succ' $ zero

{-
 - cata pleaseWait (succ' . succ' . succ' . succ' $ zero) == "wait.. wait.. wait.. wait.. go!"
 -
 - cata alg = alg . fmap (cata alg) . invIso
 -
 - alg . fmap (cata alg) . invIso $ zero
 - alg . fmap (cata alg) . invIso $ Iso Nothing
 - alg . fmap (cata alg) $ Nothing
 - alg $ Nothing
 - "go!"
 -
 - cata (succ' . succ' . succ' . succ' $ zero)
 - alg . fmap (cata alg) . invIso $ (succ' . succ' . succ' . succ' $ zero)
 - alg . fmap (cata alg) . invIso $ (succ' (succ' (succ' (succ' (zero)))))
 - alg . fmap (cata alg) . invIso $ (Iso . Just (succ' (succ' (succ' (zero)))))
 - alg . fmap (cata alg) . Just $ (succ' (succ' (succ' (zero))))
 - alg . Just $ (cata alg) (succ' (succ' (succ' (zero))))
 - alg . Just $ alg . fmap (cata alg) . invIso $ (succ' (succ' (succ' (zero))))
 - alg . Just $ alg . fmap (cata alg) . invIso $ (Iso . Just (succ' (succ' (zero))))
 - alg . Just $ alg . fmap (cata alg) . Just (succ' (succ' (zero)))
 - alg . Just $ alg . Just $ (cata alg) (succ' (succ' (zero)))
 - alg . Just $ alg . Just $ alg . fmap (cata alg) . invIso $ (succ' (succ' (zero)))
 - alg . Just $ alg . Just $ alg . fmap (cata alg) . invIso $ (Iso . Just (succ' (zero)))
 - alg . Just $ alg . Just $ alg . fmap (cata alg) . Just (succ' (zero))
 - alg . Just $ alg . Just $ alg . Just $ (cata alg) (succ' (zero))
 - alg . Just $ alg . Just $ alg . Just $ alg . fmap (cata alg) . invIso $ (succ' (zero))
 - alg . Just $ alg . Just $ alg . Just $ alg . fmap (cata alg) . invIso $ (Iso . Just (zero))
 - alg . Just $ alg . Just $ alg . Just $ alg . fmap (cata alg) . Just (zero)
 - alg . Just $ alg . Just $ alg . Just $ alg . Just $ (cata alg) zero
 - alg . Just $ alg . Just $ alg . Just $ alg . Just $ alg . fmap (cata alg) . invIso $ zero
 - alg . Just $ alg . Just $ alg . Just $ alg . Just $ alg . fmap (cata alg) . invIso $ Iso Nothing
 - alg . Just $ alg . Just $ alg . Just $ alg . Just $ alg . fmap (cata alg) Nothing
 - alg . Just $ alg . Just $ alg . Just $ alg . Just $ alg . Nothing
 - alg . Just $ alg . Just $ alg . Just $ alg . Just $ "go!"
 - alg . Just $ alg . Just $ alg . Just $ "wait.. " ++ "go!"
 - alg . Just $ alg . Just $ "wait.. " ++ "wait.. " ++ "go!"
 - alg . Just $ "wait.. " ++  "wait.. " ++ "wait.. " ++ "go!"
 - "wait.." ++ "wait.. " ++  "wait.. " ++ "wait.. " ++ "go!"
 - "wait.. wait.. wait.. wait.. go!"
 -}

data Tcon a b = TconL a | TconR b b
instance Functor (Tcon a) where
  fmap f (TconL x)   = TconL x
  fmap f (TconR y z) = TconR (f y) (f z)

type Tree' a = Fix (Tcon a)

end :: a -> Tree' a
end =  Iso . TconL

meet     :: Tree' a -> Tree' a -> Tree' a
meet l r =  Iso $ TconR l r

treeDepth'             :: Algebra (Tcon a) Int
treeDepth' (TconL x)   =  1
treeDepth' (TconR y z) =  1 + max y z

testTree' :: Tree' String
testTree' =  meet (end "X")
                  (meet (meet (end "YXX")
                              (end "YXY"))
                        (end "YY"))


data StrF x = Cons' Char x | Nil'
type Str    = Mu StrF

nil :: Str
nil =  InF Nil'

cons      :: Char -> Str -> Str
cons x xs =  InF (Cons' x xs)

instance Functor StrF where
  fmap f (Cons' a as) = Cons' a (f as)
  fmap f Nil'         = Nil'

lengthCata :: Str -> Int
lengthCata =  cata' phi where
  phi (Cons' a b) = 1 + b
  phi Nil'        = 0

-- lengthCata $ cons 'a' . cons 'a' . cons 'a' $ nil == 3
