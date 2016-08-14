curry'       :: ((a, b) -> c) -> a -> b -> c
curry' f a b =  f (a, b)

uncurry'          :: (a -> b -> c) -> (a, b) -> c
uncurry' f (a, b) =  f a b

cocurry            :: (a, b -> c) -> (a -> b) -> c
cocurry (a, bc) ab =  bc (ab a)

{-# LANGUAGE RankNTypes #-}
type Pairing f g = forall a b. f (a -> b) -> g a -> b

uncurryP :: Pairing ((->) a) ((,) a)
uncurryP =  undefined

cocurryP :: Pairing ((,) a) ((->) a)
cocurryP =  undefined

data Product f g a = Product (f a) (g a)

data Coproduct f g a = Inl (f a) | Inr (g a)

pair :: Pairing f1 f2 -> Pairing g1 g2 -> Pairing (Product f1 g1) (Coproduct f2 g2)
pair f _ (Product f1 _) (Inl f2) = f f1 f2
pair _ g (Product _ g1) (Inr g2) = g g1 g2

pair' :: Pairing f1 f2 -> Pairing g1 g2 -> Pairing (Coproduct f1 g1) (Product f2 g2)
pair' f _ (Inl f1) (Product f2 _) = f f1 f2
pair' _ g (Inr g1) (Product _ g2) = g g1 g2

data Composer f g a = Composer (f (g a))
