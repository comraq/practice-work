{-
 - Cofree Comonad and the Expression Problem
 - @link - comonad.com/reader/2008/the-cofree-comonad-and-the-expression-problem
 -}

{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
import Control.Monad
import Control.Arrow ((&&&), (***), (+++), (|||))

newtype Identity a = Identity { runIdentity :: a }

instance Functor Identity where
  fmap f = Identity . f . runIdentity

instance Applicative Identity where
  pure    = Identity
  f <*> a = f >>= \f' -> fmap f' a

instance Monad Identity where
  return  = pure
  m >>= f = f . runIdentity $ m

class Dual f g | f -> g, g -> f where
  zap :: (a -> b -> c) -> f a -> g b -> c

(>$<) :: Dual f g => f (a -> b) -> g a -> b
(>$<) =  zap id

instance Dual Identity Identity where
  zap f (Identity a) (Identity b) = f a b

data (f :+: g) a = Inl (f a) | Inr (g a)
data (f :*: g) a = Prod (f a) (g a)

instance (Functor f, Functor g) => Functor (f :+: g) where
  fmap f (Inl x) = Inl (fmap f x)
  fmap f (Inr y) = Inr (fmap f y)

instance (Functor f, Functor g) => Functor (f :*: g) where
  fmap f (Prod x y) = Prod (fmap f x) (fmap f y)

instance (Dual f f', Dual g g') => Dual (f :+: g) (f' :*: g') where
  zap op (Inl f) (Prod a _) = zap op f a
  zap op (Inr g) (Prod _ b) = zap op g b

instance (Dual f f', Dual g g') => Dual (f :*: g) (f' :+: g') where
  zap op (Prod a _) (Inl f') = zap op a f'
  zap op (Prod _ b) (Inr g') = zap op b g'

class BiDual p q | p -> q, q -> p where
  bizap :: (a -> c -> e) -> (b -> d -> e) -> p a b -> q c d -> e

(>>$<<) :: BiDual p q => p (a -> c) (b -> c) -> q a b -> c
(>>$<<) =  bizap id id

instance BiDual (,) Either where
  bizap l r (f, g) (Left a)  = l f a
  bizap l r (f, g) (Right b) = r g b

instance BiDual Either (,) where
  bizap l r (Left f) (a, b)  = l f a
  bizap l r (Right g) (a, b) = r g b

ten :: Int
ten =  ((*2), id) >>$<< Left 5

four :: (Fractional a) => a
four =  Left (/2) >>$<< (8.0, True)

eleven :: Int
eleven =  (const 100, id) >>$<< Right 11

newtype Cofree f a = Cofree { runCofree :: (a, f (Cofree f a)) }
newtype Free f a   = Free { runFree :: Either a (f (Free f a)) }

instance Functor f => Functor (Cofree f) where
  fmap f = Cofree . (f *** fmap (fmap f)) . runCofree

instance Functor f => Functor (Free f) where
  fmap f = Free . (f +++ fmap (fmap f)) . runFree

anaC :: Functor f => (a -> f a) -> a -> Cofree f a
anaC t = Cofree . (id &&& fmap (anaC t) . t)

instance Functor f => Applicative (Free f) where
  pure    = return
  f <*> a = f >>= \f' -> fmap f' a

instance Functor f => Monad (Free f) where
  return  = Free . Left
  m >>= k = (k ||| (inFree . fmap (>>= k))) (runFree m)

inFree :: f (Free f a) -> Free f a
inFree =  Free . Right

instance Dual f g => Dual (Cofree f) (Free g) where
  zap op (Cofree fs) (Free as) = bizap op (zap (zap op)) fs as

instance Dual f g => Dual (Free f) (Cofree g) where
  zap op (Free fs) (Cofree as) = bizap op (zap (zap op)) fs as

type Nat a = Free Identity a

type Stream a = Cofree Identity a

suc :: Nat a -> Nat a
suc =  inFree . Identity

ints :: Stream Int
ints =  anaC (return . (+1)) 0

-- two == 2
two :: Int
two =  suc (suc (return id)) >$< ints

-- three == 3
three :: Int
three =  suc (suc (suc (return id))) >$< ints

{-
 - Comonads as Spaces
 - @link - blog.functorial.com/posts/2016-08-07-Comonads-As-Spaces.html
 -}
curry'       :: ((a, b) -> c) -> a -> b -> c
curry' f a b =  f (a, b)

uncurry'          :: (a -> b -> c) -> (a, b) -> c
uncurry' f (a, b) =  f a b

cocurry            :: (a, b -> c) -> (a -> b) -> c
cocurry (a, bc) ab =  bc (ab a)

type Pairing f g = forall a b. f (a -> b) -> g a -> b

uncurryP :: Pairing ((->) a) ((,) a)
uncurryP =  undefined

cocurryP :: Pairing ((,) a) ((->) a)
cocurryP =  undefined

data Product f g a = Product (f a) (g a)

data Coproduct f g a = Inleft (f a) | Inright (g a)

pair :: Pairing f1 f2 -> Pairing g1 g2 -> Pairing (Product f1 g1) (Coproduct f2 g2)
pair f _ (Product f1 _) (Inleft f2)  = f f1 f2
pair _ g (Product _ g1) (Inright g2) = g g1 g2

pair' :: Pairing f1 f2 -> Pairing g1 g2 -> Pairing (Coproduct f1 g1) (Product f2 g2)
pair' f _ (Inleft f1)  (Product f2 _) = f f1 f2
pair' _ g (Inright g1) (Product _ g2) = g g1 g2

data Compose f g a = Compose (f (g a))

pair'' :: Pairing f1 f2 -> Pairing g1 g2 -> Pairing (Compose f1 g1) (Compose f2 g2)
pair'' =  undefined

newtype Co w a = Co { runCo :: forall r. w(a -> r) -> r }

pairCo       :: Pairing f (Co f)
pairCo f cof =  runCo cof f

class Functor w => Comonad w where
  extract :: w a -> a

  duplicate :: w a -> w (w a)
  duplicate =  extend id

  extend   :: (w a -> b) -> w a -> w b 
  extend f =  fmap f . duplicate

moving :: Comonad w => Pairing m w -> m (a -> b) -> w a -> w b
moving pair m =  extend (pair m)

move :: (Comonad w, Functor m) => Pairing m w -> m () -> w a -> w a
move pair m = moving pair ((\_ -> id) <$> m)

{-
 - moveStore :: State s () -> Store s a -> Store s a
 - moveStore =  undefined
 -}

data Zipper a = Zipper [a] a [a]

instance Functor Zipper where
  fmap f (Zipper xs a ys) = Zipper (f <$> xs) (f a) (f <$> ys)

instance Comonad Zipper where
  extract (Zipper _ a _) = a

left :: Co Zipper ()
left =  Co $ \(Zipper (f:_) _ _) -> f ()

right :: Co Zipper ()
right =  Co $ \(Zipper _ _ (f:_)) -> f ()
