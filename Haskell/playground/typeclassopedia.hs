import Prelude hiding ((**))

{-
 - Functor Laws
 -
 - 1) fmap id = id
 - 2) fmap (g . h) = (fmap g) . (fmap h)
 -}
class Functor' f where
  fmap' :: (a -> b) -> f a -> f b

instance Functor' (Either e) where
  fmap' _ (Left e)  = Left e
  fmap' f (Right x) = Right $ f x

instance Functor' ((->) e) where
  fmap' = (.)

data Pair a = Pair a a
  deriving (Show)

-- Functor only parametrized by the type of the second element in the tuple
instance Functor' ((,) e) where
  fmap' f ((,) a b) = (a, f b)

-- Functor parametrized by one type, applied to both elements (of the same
-- type) in the tuple Pair
instance Functor' Pair where
  fmap' f (Pair a b) = Pair (f a) (f b)

data ITree a = Leaf (Int -> a) | Node [ITree a]

instance Functor' ITree where
  fmap' f (Node xs) = Node $ fmap (fmap' f) xs
  fmap' f (Leaf g)  = Leaf $ f . g

-- N/A as a Functor
data NA a = NA (a -> Char)

comp   :: (Functor' f) => (a -> b) -> f (f a) -> f (f b)
comp f =  fmap' (fmap' f)

{-
 - fmap :: (a -> b) -> f a -> f b
 - fmap :: ((->) a b) -> ((->) (f a) (f b))
 - fmap :: ((->) ((->) a b) ((->) (f a) (f b)))
 - fmap :: ((->) (a -> b) (f a -> f b))
 -}


{-
 - Applicative Laws
 -
 - 1) Identity
 -    pure id <*> v = v
 -
 - 2) Homomorphism
 -    pure f <*> pure x = pure (f x)
 -
 - 3) Interchange
 -    u <*> pure y = pure ($ y) <*> u
 -
 - 4) Composition
 -    u <*> (v <*> w) = pure (.) <*> u <*> v <*> w
 -
 - Applicative + Functor
 -    fmap g x = pure g <*> x
 -}
class Functor' f => Applicative' f where
  pure' :: a -> f a
  ap'   :: f (a -> b) -> f a -> f b

{-
 - ($)        :: (a -> b) -> a -> b 
 - flip       :: (a -> b -> c) -> b -> a -> c
 - (flip ($)) :: a -> (a -> b) -> b
 -
 - pure (flip ($)) <*> x <*> pure f
 - pure (flip ($) x) <*> pure f
 - pure (flip ($) x f)
 - pure (f x)
 - pure f <*> x
 -}

instance Functor' Maybe where
  fmap' _ Nothing  = Nothing
  fmap' f (Just x) = Just $ f x

instance Applicative' Maybe where
  pure'          = Just

  Just f `ap'` x  = fmap f x
  Nothing `ap'` _ = Nothing

newtype ZipList a = ZipList { getZipList :: [a] }

instance Functor' ZipList where
  fmap' f (ZipList xs) = ZipList $ fmap f xs

instance Applicative' ZipList where
  pure' = ZipList . repeat
  (ZipList gs) `ap'` (ZipList xs) = ZipList (zipWith ($) gs xs)

{-
 - Monoidal Laws 
 -
 - 1) Left Identity
 -    unit ** v ~= v
 -
 - 2) Right Identity
 -    u ** unit ~= u
 -
 - 3) Associativity:
 -    u ** (v ** w) ~= (u ** v) ** w
 -
 - Note: where ~= refers to isomorphism, not equality hence:
 -       (x, ()) ~= x ~= ((), x)
 -       ((x, y), z) ~= (x, (y, z))
 -
 - Naturality
 -   fmap (g *** h) (u ** v) = fmap g u ** fmap h v
 -}
class Functor' f => Monoidal f where
  unit :: f ()
  (**) :: f a -> f b -> f (a, b)

mPure   :: (Monoidal f) => a -> f a
mPure a =  fmap' (const a) unit

mAp       :: (Monoidal f) => f (a -> b) -> f a -> f b
mAp mf ma =  fmap' (\(f, a) -> f a) (mf ** ma)
