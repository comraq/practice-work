{-
 - Nested 'Dots'
 -}

dot :: (b -> c) -> (a -> b) -> a -> c
dot =  (.)

dotdot :: (b -> c) -> (a -> a1 -> b) -> a -> a1 -> c
dotdot =  dot `dot` dot

dotdotdot :: (b -> c) -> (a -> a1 -> a2 -> b) -> a -> a1 -> a2 -> c
dotdotdot =  dot `dot` dotdot

class Functor' f where
  fmap' :: (a -> b) -> f a -> f b

class Functor' m => Applicative' m where
  pure' :: a -> m a
  ap'   :: m (a -> b) -> m a -> m b

class Applicative' m => Monad' m where
  return' :: a -> m a
  bind'   :: m a -> (a -> m b) -> m b


class Semigroup' a where
  (<>) :: a -> a -> a

class Semigroup' a => Monoid' a where
  unit' :: a
 
  concat' :: [a] -> a
  concat' =  foldr (<>) unit'

class Foldable' t where
  foldMap' :: Monoid' m => (a -> m) -> t a -> m

class (Functor' f, Foldable' f) => Traversable' f where
  traverse' :: Applicative' m => (a -> m b) -> f a -> m (f b)


newtype Identity a = Identity { runIdentity :: a }

instance Functor' Identity where
  fmap' f = Identity . f . runIdentity

instance Applicative' Identity where
  pure'   = Identity
  ap' f a = f `bind'` \f' -> fmap' f' a

instance Monad' Identity where
  return'   = pure'
  bind' a f = f $ runIdentity a

fmapDefault   :: (Traversable' f) => (a -> b) -> f a -> f b
fmapDefault f =  runIdentity . traverse' (Identity . f)

-- Note:
-- fmap :: Functor f => SEC (f a) (f b) a b
type SEC a b c d = (c -> d) -> a -> b

firstSEC          :: SEC (a, c) (b, c) a b
firstSEC f (a, b) =  (f a, b)

{-
 - over     :: ((a -> Identity b) -> s -> Identity t) -> (a -> b) -> s -> t
 - over l f =  runIdentity . l (Identity . f)
 -
 - over traverse' f = runIdentity . traverse' (Identity . f)
 -                  = fmapDefault f
 -                  = fmap'
 -}
type Setter s t a b = (a -> Identity b) -> s -> Identity t

over     :: Setter s t a b -> (a -> b) -> s -> t
over l f =  runIdentity . l (Identity . f)

mapped   :: (Functor' f) => Setter (f a) (f b) a b
mapped f =  Identity . fmap' (runIdentity . f)

{-
 - over mapped f = runIdentity . mapped (Identity . f)
 -               = runIdentity . (Identity . fmap' (runIdentity (Identity . f)))
 -               = runIdentity . Identity . fmap' (runIdentity . Identity . f)
 -               = fmap'
 -}

{-
 - Functor Laws:
 -   fmap id = id
 -   fmap f . fmap g = fmap (f . g)
 -
 - Setter Laws:
 -   over l id = id
 -   over l f . over l g = over l (f . g)
 -}

both          :: Setter (a, a) (b, b) a b
both f (a, b) =  (,) `fmap'` f a `ap'` f b

first          :: Setter (a, c) (b, c) a b
first f (a, c) =  fmap' (\b -> (b, c)) $ f a

instance Functor' [] where
  fmap' f xs = [ f x | x <- xs ]

{-# LANGUAGE RankNTypes #-}

type Traversal a b c d = forall f. Applicative' f => (c -> f d) -> a -> f b
-- type Setter a b c d = (c -> Identity d) -> a -> Identity b

{-
 - traverseT :: Traversable f => Traversal (f a) (f b) a b
 - traverseT . traverse :: (Traversable f, Traversable g) => Traversal (f (g a)) (f (g b)) a b
 -
 - over traverseT f = runIdentity . traverse (Identity . f)
 -                  = fmapDefault f
 -                  = fmap' f
 -
 -skell
 Traversable Laws:
 -   traverse pure = pure
 -   Compose . fmap (traverse f) . traverse g = traverse (Compose . fmap f. g)
 -
 - Traversal 'l' Laws:
 -   l pure = pure
 -   Compose . fmap (l f) . l g = l (Compose . fmap f . g)
 -}

bothT          :: Traversal (a, a) (b, b) a b
bothT f (a, b) =  (,) `fmap'` f a `ap'` f b

instance Functor' (Either a) where
  fmap' _ (Left l)  = Left l
  fmap' f (Right r) = Right $ f r

instance Applicative' (Either a) where
  pure'   = Right
  ap' f a = f `bind'` \f' -> fmap' f' a

instance Monad' (Either a) where
  return'             = pure'
  (Right r) `bind'` f = f r
  (Left l) `bind'` _  = Left l

traverseLeft             :: Traversal (Either a c) (Either b c) a b
traverseLeft f (Left a)  =  Left `fmap'` f a
traverseLeft _ (Right a) =  pure' $ Right a

traverseRight             :: Traversal (Either c a) (Either c b) a b
traverseRight f (Left a)  =  pure' $ Left a
traverseRight f (Right a) =  Right `fmap'` f a

-- Note: 
-- traverse . both :: Traversal (Either (a, a) c) (Either (b, b) c) a b

{-
 - foldMap :: (Foldable f, Monoid m) => (a -> m) -> f a -> m
 - foldMap . foldMap :: (Foldable f, Foldable g, Monoid m) => (a -> m) -> f (g a) -> m
 - foldMap . foldMap . foldMap :: (Foldable f, Foldable g, Foldable h, Monoid m) => (a -> m) -> f (g (h a)) -> m
 -}

foldMapDefault   :: (Traversable' f, Monoid' m) => (a -> m) -> f a -> m
foldMapDefault f =  getConst . traverse' (Const . f)

newtype Const m a = Const { getConst :: m }

instance Functor' (Const m) where
  fmap' _ (Const m) = Const m

instance Monoid' m => Applicative' (Const m) where
  pure' _ = Const unit'
  Const m `ap'` Const n = Const (m <> n)

foldMapOf     :: ((b -> Const m c) -> a -> Const c d) -> (b -> m) -> a -> c
foldMapOf l f =  getConst . l (Const . f)

type Fold s a = forall m. Monoid' m => (a -> Const m a) -> s -> Const m s

{-
 - folded :: (Foldable' f) => Fold (f a) a
 - folded . folded :: (Foldable' f, Foldable' g) => Fold (f (g a)) a
 - folded . folded . folded :: (Foldable' f, Foldable g', Foldable' h) => Fold (f (g a)) a
 -}

folded :: (Foldable' f) => Fold (f a) a
folded f = Const . foldMap' (getConst . f)

type Getting r a b c d = (c -> Const r d) -> a -> Const r b

view   :: Getting c a b c d -> a -> c
view l =  getConst . l Const

type Getter a c = forall r. (c -> Const r c) -> a -> Const r a

-- to :: (a -> c) -> (c -> Const r c) -> a -> Const r a
to     :: (a -> c) -> Getter a c
to f g =  Const . getConst . g . f

{-
 - view (to f) = getConst . to f Const
 -             = getConst . Const . getConst . Const . f
 -             = f
 -}

type Lens a b c d = forall f. Functor' f => (c -> f d) -> a -> f b

toListOf   :: Getting [c] a b c d -> a -> [c]
toListOf l =  getConst . l (\c -> Const [c])

_1          :: Lens (a, c) (b, c) a b
_1 f (a, c) =  fmap' (\b -> (b, c)) $ f a
