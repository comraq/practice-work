data TypeRep = TyCon String | TyApp TypeRep TypeRep

data Proxy a

class Typeable a where
  typeOf :: a -> TypeRep

data T f a = MkT (f a)
