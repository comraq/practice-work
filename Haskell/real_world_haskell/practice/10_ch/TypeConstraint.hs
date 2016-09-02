{-# LANGUAGE DatatypeContexts #-}

data (Ord a) => OrdStack a = Bottom | Item a (OrdStack a)
  deriving (Show)

isIncreasing :: (Ord a) => OrdStack a -> Bool
isIncreasing (Item a rest@(Item b _))
  | a < b     = isIncreasing rest
  | otherwise = False
isIncreasing _ = True

{-
 - Due to the misused 'Ord a' enforcement during the type definition stage,
 - we wind up the contraint in situations where it isn't needed
 -
 - ex: In the following function 'push', if the 'Ord a' constraint is
 -     removed, then the function would fail to typecheck
 -}
push :: Ord a => a -> OrdStack a -> OrdStack a
push a s = Item a s

{-
 - Restrain from putting the 'Ord' constraints on type definitions and
 - functions such as 'push', thus only 'isIncreasing' needs the constraint
 -}
