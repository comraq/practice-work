import Prelude hiding ((**))
import Data.Maybe (fromJust)
import qualified Control.Arrow as A

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

data W a = W a deriving Show

class Applicative' m => Monad' m where
  return' :: a -> m a
  bind'   :: m a -> (a -> m b) -> m b

  join'   :: m (m a) -> m a
  join'   =  flip bind' id

instance Functor' W where
  fmap' f (W a) = W $ f a

instance Applicative' W where
  pure'         = W
  W f `ap'` W a = W $ f a

instance Monad' W where
  return'        = pure'
  W a `bind'` f  = f a

g      :: Int -> W Int -> W Int
g x wy =  wy `bind'` \y -> W $ y + x

h       :: W Int -> W Int -> W Int
h wx wy =  wx `bind'` \x -> wy `bind'` \y -> W $ x + y

{-
 - Monad Laws
 -
 - 1) Left Identity
 -    return x >>= \x -> f x === f x
 -
 - 2) Right Identity
 -    m >>= return === m
 -
 - 3) Associativity
 -    (m >>= f) >>= g === m >>= (\x -> f x >>= g)
 -}

{-
 - Monad Laws Proofs
 -
 - 1) return x >>= \x -> f x
 -    m x >>= \x -> f x
 -    \x -> f x
 -    f x
 -
 - 2) m a >>= return === m a
 -    \a -> return a
 -    m a
 -
 - 3) (m x >>= f) >>= g
 -    (\x -> f x) >>= g
 -    f x >>= g
 -
 -    m x >>= (\x -> f x >>= g)
 -    (\x -> f x >>= g) x
 -    f x >>= g
 -}

instance Functor' [] where
  fmap' = map

instance Applicative' [] where
  pure' x    = [x]
  f `ap'` xs = f `bind'` \f' -> fmap' f' xs

instance Monad' [] where
  return'          = pure'
  (x:xs) `bind'` f = (f x) ++ (xs `bind'` f) 
  [] `bind'` _     = []

data Free f a = Var a | NodeF (f (Free f a))

instance (Functor' f) => Functor' (Free f) where
  fmap' f (Var a)    = Var $ f a
  fmap' f (NodeF fa) = NodeF $ fmap' (fmap' f) fa 

instance (Functor' f) => Applicative' (Free f) where
  pure'     = Var
  f `ap'` a = f `bind'` \f' -> fmap' f' a

instance (Functor' f) => Monad' (Free f) where
  return'              = pure'
  Var a `bind'` f      = f a
  NodeF fa `bind'` f   = NodeF $ fmap' unwrap fa
    where unwrap = \free -> case free of
            (Var a)     -> f a
            m           -> m `bind'` f

bind''     :: (Monad' m) => m a -> (a -> m b) -> m b
bind'' a f =  join' $ fmap' f a

liftM'     :: (Monad' m) => (a -> b) -> m a -> m b
liftM' f m =  m `bind'` (return' . f)

{-
 - Left to Right Kleisli Composition (>=>) Laws
 -
 - 1) Left Identity
 -    return x >=> g = g
 -
 - 2) Right Identity
 -    m >=> return === g
 -
 - 3) Associativity
 -    (m >=> f) >=> g === m >=> (f >=> g)
 -}

class MonadTrans' t where
  -- Type Application Associates to the left so
  -- t m a = (t m) a
  lift :: (Monad' m) => m a -> t m a

{-
 - MonadTransformer Laws
 -
 - 1) Identity Law -- ("lifts" return from m to (t m))
 -    lift . return = return
 -
 - 2) Associativity Law -- ("lifts" (>>=) from m to (t m))
 -    lift (m >>= f) = lift m >>= (lift . f)
 -}

-- MonadTrans kind :: ((* -> *) -> *) -> *
--
-- :k MonadTrans'
-- MonadTrans' :: ((* -> *) -> * -> *) -> GHC.Prim.Constant

distribT :: (Monad' m, Monad' n) => n (m a) -> m (n a)
distribT =  undefined

joinT      :: (Monad' m, Monad' n) => m (n (m (n a))) -> m (n a)
joinT mnmn =  mnmn `bind'` \nmn -> fmap' join' $ distribT nmn 

fix'   :: (a -> a) -> a
fix' f =  f (fix' f)

class Monad' m => MonadFix' m where
  mfix' :: (a -> m a) -> m a

instance Monad' Maybe where
  return'            = pure'
  (Just x) `bind'` f = f x
  Nothing `bind'`  _ = Nothing

instance MonadFix' Maybe where
  mfix' f = ma
    where ma = f (fromJust ma)

instance MonadFix' [] where
  mfix' f = xs
    where xs = case fix' (f . head) of
            []     -> []
            (y:_)  -> y : mfix' (tail . f)

class Semigroup' a where
  (<>) :: a -> a -> a

class Semigroup' a => Monoid' a where
  unit' :: a
 
  concat' :: [a] -> a
  concat' =  foldr (<>) unit'

instance Monoid' e => Applicative' ((,) e) where
  pure' x             = (unit', x)
  (u, f) `ap'` (v, x) = (u <> v, f x)

class Applicative' f => Alternative' f where
  empty'  :: f a
  choice' :: f a -> f a -> f a

class Monad' m => MonadPlus' m where
  -- failure
  mzero' :: m a

  -- choice
  mplus' :: m a -> m a -> m a

{-
 - Laws for Alternative and MonadPlus
 -
 - empty <|> x     === x
 - x <|> empty     === x
 - (x <|> y) <|> z === x <|> (y <|> z)
 -
 - where empty ~= mzero
 -       (<|>) ~= mplus
 -
 - mzero >>= f === mzero
 - v >> mzero  === mzero
 -}

class Category' cat' where
  id'   :: cat' a a
  comp' :: cat' b c -> cat' a b -> cat' a c

newtype Kleisli' m a b = Kleisli' { runKleisli :: a -> m b }

instance Monad' m => Category' (Kleisli' m) where
  id'                           = Kleisli' return'
  Kleisli' a `comp'` Kleisli' b = Kleisli' $ \c -> b c `bind'` \b' -> a b'

class Category' arr' => Arrow' arr' where
  arr'      :: (b -> c) -> (arr' b c)

  first'    :: (arr' b c) -> (arr' (b, d) (c, d))
  first'    =  flip parallel' id'

  second'   :: (arr' b c) -> (arr' (d, b) (d, c))
  second'   =  parallel' id'

  parallel'     :: (arr' b c) -> (arr' b' c') -> (arr' (b, b') (c, c'))
  parallel' f g =  arr' swap `comp'` first' g `comp'` arr' swap `comp'` first' f
    where swap (x, y) = (y, x)

  fanout'     :: (arr' b c) -> (arr' b c') -> (arr' b (c, c'))
  fanout' f g =  parallel' f g `comp'` arr' (\b -> (b, b)) 

{-
 - parallel' :: (arr' b c) -> (arr' b' c') -> (arr' (b, b') (c, c'))
 - parallel' :: (b -> c) -> (b' -> c') -> ((b, b') -> (c, c'))
 -
 - parallel' f g = arr' swap `comp'` first' g `comp'` arr' swap `comp'` first' f
 -   where swap (x, y) = (y, x)
 -
 - (parallel' f g) (b, b')
 - (arr' swap `comp'` first' g `comp'` arr' swap `comp'` first' f) (b, b')
 - (arr' swap `comp'` first' g `comp'` arr' swap) (f b, b')
 - (arr' swap `comp'` first' g `comp'` arr' swap) (c, b')
 - (arr' swap `comp'` first' g) (b', c)
 - (arr' swap) (g b', c)
 - (arr' swap) (c', c)
 - (c, c')
 -}

{-
 - Arrow Laws
 -
 - 1) arr id === id
 -
 - 2) arr (h . g) === arr g >> arr h
 -
 - 3) first $ arr g === arr (g *** id)
 -
 - 4) first (g >>> h) === first g >>> first h
 -
 - 5) first g >>> arr (id *** h) === arr (id *** h) >>> first g
 -
 - 6) first g >>> arr fst === arr fst >>> g
 -
 - 7) first (first g) >>> arr assoc === arr assoc >>> first g
 -      where assoc ((x, y), z) = (x, (y, z))
 -}

class Arrow' arr' => ArrowZero' arr' where
  zeroArrow' :: (b `arr'` c)

class ArrowZero' arr' => ArrowPlus' arr' where
  arrowChoice' :: (b `arr'` c) -> (b `arr'` c) -> (b `arr'` c)

class Foldable' t where
  fold'    :: Monoid' m => t m -> m
  fold'    =  foldMap' id

  foldMap'   :: Monoid' m => (a -> m) -> t a -> m
  foldMap' f =  foldr'' ((<>) . f) unit'

  foldr''   :: (a -> b -> b) -> b -> t a -> b
  foldr'' f =  flip $ foldl'' (\g x -> \a -> g $ f x a) id

  foldl''   :: (a -> b -> a) -> a -> t b -> a
  foldl'' f =  flip $ foldr'' (\x g -> \a -> g $ f a x) id

instance Foldable' [] where
  foldMap' g = concat' . map g

data Tree' a = Empty' | Leaf' a | Node' (Tree' a) a (Tree' a)

instance Foldable' Tree' where
  foldMap' f Empty'        = unit'
  foldMap' f (Leaf' x)     = f x
  foldMap' f (Node' l k r) = foldMap' f l <> f k <> foldMap' f r

{-
 - foldMap . foldMap :: (Foldable t, Foldable t', Monoid m) => (a -> m) -> t' (t a) m
 -
 - foldMap . foldMap .foldMap ::
 -   (Foldable t, Foldable t', Foldable t'', Monoid m) => (a -> m) -> t'' (t' (t a)) m
 -
 - Traverses into any nested Foldable, maps its contents to a monoid and
 - folds down the resulting monoid
 -}

instance Semigroup' [a] where
  (<>) = (++)

instance Monoid' [a] where
  unit' = []

toList' :: (Foldable' t) => t a -> [a]
toList' =  foldr'' (:) []

concatF' :: (Foldable' t) => t [a] -> [a]
concatF' =  fold'

concatMapF' :: (Foldable' t) => (a -> [b]) -> t a -> [b]
concatMapF' =  foldMap'

newtype And' = And' { runAnd' :: Bool }
newtype Or'  = Or' { runOr' :: Bool }

instance Semigroup' And' where
  And' a <> And' b = And' $ a && b

instance Semigroup' Or' where
  Or' a <> Or' b = Or' $ a || b

instance Monoid' And' where
  unit' = And' True

instance Monoid' Or' where
  unit' = Or' False

class (Functor' t, Foldable' t) => Traversable' t where
  traverse'   :: (Applicative' f) => (a -> f b) -> t a -> f (t b)
  traverse' f =  sequenceA' . fmap' f

  sequenceA' :: (Applicative' f) => t (f a) -> f (t a)
  sequenceA' =  traverse' id

  mapM' :: (Monad' m) => (a -> m b) -> t a -> m (t b)
  mapM' =  traverse'

  sequence' :: (Monad' m) => t (m a) -> m (t a)
  sequence' =  sequenceA'

{-
 - Tree [a] -> [Tree a]
 -   traverse id
 -   sequenceA
 -
 - [Tree a] -> Tree [a]
 -
 - traverse . traverse :: (Applicative f, Traversable t, Traversable t') =>
 -   (a -> f b) -> t (t' a) -> f (t (t' b))
 -
 - Maps the value of a Traversable from a -> b, if b yields effects
 - (Applicative f), then the entire Traversable is encapsulated with
 - Applicative f
 -}

instance Functor' Tree' where
  fmap' _ Empty'        = Empty'
  fmap' f (Leaf' x)     = Leaf' $ f x
  fmap' f (Node' l k r) = Node' (fmap' f l) (f k) (fmap' f r)

instance Traversable' Tree' where
  traverse' g Empty'        = pure' Empty'
  traverse' g (Leaf' x)     = Leaf' `fmap'` g x
  traverse' g (Node' l k r) = Node' `fmap'` traverse' g l `ap'` g k `ap'` traverse' g r

newtype Id' a = Id' { runId' :: a }

instance Functor' Id' where
  fmap' f (Id' a) = Id' $ f a

instance Applicative' Id' where
  pure'                 = Id'
  (Id' f) `ap'` (Id' a) = Id' $ f a

newtype Const' a b = Const' { runConst' :: a }

instance Functor' (Const' a) where
  fmap' _ (Const' a) = Const' a

instance Monoid' m => Applicative' (Const' m) where
  pure' _                 = Const' unit'
  Const' f `ap'` Const' a = Const' $ (<>) f a

fmapT'     :: (Traversable' t) => (a -> b) -> t a -> t b
fmapT' f a =  runId' $ traverse' (Id' . f) a

foldMapT'     :: (Traversable' t, Monoid' m) => (a -> m) -> t a -> m
foldMapT' f a =  runConst' $ traverse' (Const' . f) a

{-
 - Traverable Laws
 -
 - 1) traverse Identity === Identity
 -    ( traversals cannot make up arbitrary effects )
 -
 - 2) traverse (Compose . fmap g . f) === Compose . fmap (traverse g) . traverse f
 -
 - 3) eta :: forall a f g. (Applicative f, Applicative g) => f a -> g a
 -    ex: eta (pure x) === pure x
 -        eta (x <*> y) === eta x <*> eta y
 -
 -    eta . traverse f === traverse (eta . f)
 -}

class Arrow' arr' => ArrowChoice' arr' where
  left'      :: (b `arr'` c) -> (Either b d `arr'` Either c d)
  right'     :: (b `arr'` c) -> (Either d b `arr'` Either d c)
  parallelC' :: (b `arr'` c) -> (b' `arr'` c') -> (Either b b' `arr'` Either c c')
  fanoutC'   :: (b `arr'` d) -> (c `arr'` d) -> (Either b c `arr'` d)

class Arrow' arr' => ArrowApply' arr' where
  app' :: (b `arr'` c, b) `arr'` c

instance Monad' m => Arrow' (Kleisli' m) where
  arr' f               = Kleisli' $ return' . f
  first' (Kleisli' f)  = Kleisli' $ \(b, d) -> f b `bind'` \c -> return' (c, d)
  second' (Kleisli' f) = Kleisli' $ \(d, b) -> f b `bind'` \c -> return' (d, c)

instance Monad' m => ArrowApply' (Kleisli' m) where
  app' = Kleisli' (\(Kleisli' f, x) -> f x)

newtype ArrowMonad' a b = ArrowMonad' (a () b)

instance Arrow' a => Functor' (ArrowMonad' a) where
  fmap' f (ArrowMonad' m) = ArrowMonad' $ arr' f `comp'` m

instance Arrow' a => Applicative' (ArrowMonad' a) where
  pure' x                           = ArrowMonad' (arr' $ const x)
  ArrowMonad' f `ap'` ArrowMonad' a = ArrowMonad' (arr' (uncurry id) `comp'` fanout' f a)

instance ArrowApply' a => Monad' (ArrowMonad' a) where
  return' = pure'
  (ArrowMonad' a) `bind'` k = ArrowMonad' $
    app' `comp'` arr' (\x -> let ArrowMonad' h = k x
                             in  (h, ()))
         `comp'` a

class Arrow' a => ArrowLoop' a where
  loop' :: a (b, d) (c, d) -> a b c

trace'     :: ((b, d) -> (c, d)) -> b -> c
trace' f b =  let (c, d) = f (b, d) in c

class ArrowLoop' arr' => ArrowCircuit' arr' where
  delay' :: b -> (b `arr'` b)

class Functor' w => Comonad' w where
  extract :: w a -> a

  duplicate :: w a -> w (w a)
  duplicate =  extend id

  extend   :: (w a -> b) -> w a -> w b
  extend f =  fmap' f . duplicate

data Stream' a = Cons' a (Stream' a)

instance Functor' Stream' where
  fmap' f (Cons' x xs) = Cons' (f x) $ fmap' f xs

instance Comonad' Stream' where
  extract (Cons' x _) = x
