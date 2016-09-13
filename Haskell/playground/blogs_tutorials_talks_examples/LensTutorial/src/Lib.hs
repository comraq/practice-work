{-# LANGUAGE TemplateHaskell
           , DeriveFunctor
           , DeriveFoldable
           , DeriveTraversable
 #-}

module Lib where

import Control.Lens hiding (element)

{-
 - Some of the types within the Lens library:
 -
 - type Lens s t a b = forall (f :: * -> *). Functor f
 -                   => (a -> f b)
 -                   -> s
 -                   -> f t
 -
 - type Lens' s a = Lens s s a a
 -
 - type Getter s a = forall (f :: * -> *). (Contravariant f, Functor f)
 -                 => (a -> f a)
 -                 -> s
 -                 -> f s
 -
 - type Getting r s a = (a -> Const r a) -> s -> Const r s
 -
 - class (Applicative f, Traversable f, Data.Distributive.Distributive f)
 -    => Settable (f :: * -> *) where
 -    untainted    ::                 f a       -> a
 -    untaintedDot :: Profunctor p => p a (f b) -> p a b
 -    taintedDot   :: Profunctor p => p a b     -> p a (f b)
 -
 -    untaintedDot = dimap id untainted
 -    taintedDot   = dimap id pure
 -
 - type Setter s t a b = forall (f :: * -> *). Settable f
 -                     => (a -> f b)
 -                     -> s
 -                     -> f t
 -
 - type Setting (p :: * -> * -> *) s t a b = p a (Identity b)
 -                                         -> s
 -                                         -> Identity t
 -
 - type ASetter s t a b = (a -> Identity b) -> s -> Identity t
 -}

lensTutorial :: IO ()
lensTutorial = putStrLn "lens tutorial"

data Atom = Atom {
  _element :: String
, _point   :: Point
} deriving Show

data Point = Point {
  _x :: Double
, _y :: Double
} deriving Show

shiftAtomXNormal :: Atom -> Atom
shiftAtomXNormal (Atom e (Point x y)) = Atom e $ Point (x + 1) y

makeLenses ''Atom
makeLenses ''Point

shiftAtomX :: Atom -> Atom
shiftAtomX = over (point . x) (+ 1)

data Molecule = Molecule {
  _atoms :: [Atom]
} deriving Show

makeLenses ''Molecule

shiftMoleculeX :: Molecule -> Molecule
shiftMoleculeX = over (atoms . traverse . point . x) (+ 1)

{-
 - What is the type of a lens?
 -
 - > point   :: Lens' Atom Point
 - > element :: Lens' Atom String
 -
 - > x :: Lens' Point Double
 - > y :: Lens' Point Double
 -}

{-
 - Lens can be created by 'makeLenses' using TemplateHaskell pragma or the
 - helper 'lens' function:
 -   lens :: (a -> b) -> (b -> a -> a) -> Lens' a b
 -
 -   where
 -     - the first argument is a function to extract 'b' from 'a'
 -     - the second argument is a function to set 'b' in 'a'
 -
 - Example:
 -   point :: Lens' Atom Point
 -   point = lens _point (\newPoint atom -> atom { _point = newPoint })
 -
 - Lens' can also be made manually without using the helper 'lens' function
 -
 - point :: Lens' Atom -> Point
 - point :: Functor f => (Point -> f Point) -> Atom -> f Atom
 - point k atom = fmap (\newPoint -> atom { _point = newPoint })
 -                     (k $ _point atom))
 -}

{-
 - Lenses can be composed via the ordinary (.) function composition
 - operator. Since 'Lens' is defined as:
 -   type Lens s t a b = forall (f :: * -> *). Functor f
 -                     => (a -> f b)
 -                     -> s
 -                     -> f t
 -
 - when using (.) to compose two 'Lens' functions, we have:
 -   (.) :: Functor f
 -       => ((b -> f b) -> a -> f a) -- First  Argument
 -       -> ((c -> f c) -> b -> f b) -- Second Argument
 -       -> ((c -> f c) -> a -> f a) -- Result Function Type
 -
 - Example::
 -   point . x :: Lens' Atom Double
 -   point . x :: Functor f => (Double -> f Double) -> Atom -> f Atom
 -}

{-
 - Consuming (using) 'Lens':
 -
 - over ::                    ASetter s t a b -> (a -> b) -> s -> t
 - view :: MonadReader s m => Getting a s a   -> m a
 - set  ::                    ASetter s t a b -> b        -> s -> t
 -
 - Note: Type synonyms such as 'Getting', 'Setting', 'ASetter' and
 -       etc... are simply specific Lenses with different 'Functors' to
 -       achieve the get/set operations.
 -}

{-
 - Since lenses are "first class" values, we can manipulate them using
 - ordinary functional programming techniques. Such as passing them as input
 - to functions, output from return values, embed in data structures and
 - etc...
 -}

-- Example: instead of 'shiftAtomX' and 'shiftMoleculeX', we define:
shift :: Num b => ASetter s t b b -> s -> t
shift lens = over lens (+1)

atomX :: Lens' Atom Double
atomX = point . x

shiftAtomX' :: Atom -> Atom
shiftAtomX' = shift atomX

moleculeX :: Traversal' Molecule Double
moleculeX = atoms . traverse . point . x

shiftMoleculeX' :: Molecule -> Molecule
shiftMoleculeX' = shift moleculeX

-- We also get the following for free!
setAtomX :: Double -> Atom -> Atom
setAtomX = set atomX

setMoleculeX :: Double -> Molecule -> Molecule
setMoleculeX = set moleculeX

viewAtomX :: Atom -> Double
viewAtomX = view atomX

-- Cant use 'view' for 'Traversal''
viewMoleculeX :: Molecule -> [Double]
viewMoleculeX = toListOf moleculeX

{-
 - 'Traversal' has the following type signatures:
 -
 - type Traversal s t a b =  forall (f :: * -> *). Applicative f
 -                        => (a -> f b)
 -                        -> s
 -                        -> f t
 - type Traversal' s a = Traveral s s a a
 -
 - 'Traversal' lets us get/set an arbitrary number of parametrized values,
 - where as 'Lens' only allows to get/set exactly one value.
 -
 - Since the 'Molecule' type has '[Atom]' has a field, 'Traversal' must be
 - used as the Lens for '_atom'.
 -
 - Note: The difference between 'Lens' and 'Traversal' is only the Functor
 -       constraint over the Applicative constraint. This has the
 -       implication that any 'Traveral' is automatically a valid 'Lens' as
 -       'Functor' is a superclass of 'Applicative'. Thus, the auto defined
 -       functions from 'makeLenses' are also Traversals:
 -         atoms   :: Traversal' Molecule [Atom]
 -         element :: Traversal' Atom     String
 -         point   :: Traversal' Atom     Point
 -         x       :: Traversal' Point    Double
 -         y       :: Traversal' Point    Double
 -}

data Pair a = Pair a a
  deriving (Show, Functor, Foldable, Traversable)

modPair :: (a -> a) -> Pair a -> Pair a
modPair = over traverse

{-
 - Example:
 - > modPair (+ 1) (Pair 3 4)
 - > Pair 4 5
 -}

{-
 - Traversals can also be composed via regular function composition operator
 - (.):
 -
 - (.) :: Applicative f
 -     => ((b -> f b) -> a -> f a)
 -     -> ((c -> f c) -> b -> f b)
 -     -> ((c -> f c) -> a -> f a)
 -
 - Note:
 - > atoms                        :: Traversal' Molecule [Atom]
 - > atoms . traverse             :: Traversal' Molecule Atom
 - > atoms . traverse . point     :: Traversal' Molecule Point
 - > atoms . traverse . point . x :: Traversal' Molecule Double
 -
 - Examples:
 - > over (atoms . traverse . point . x)
 -   :: (Double -> Double) -> Molecule -> Molecule
 - > toListOf (atoms . traverse . point . x)
 -   :: Molecule -> [Double]
 -
 - The type of 'toListOf':
 -   toListOf :: Traversal' a b -> a -> [b]
 -}

{-
 - 'over', 'view' and 'Traversal's:
 - - 'over' and 'set' depends on the 'Identity' functor,
 -   however since 'Identity' implements both 'Functor' and 'Applicative',
 -   both 'over' and 'set' can be used on 'Traversal'
 -
 - - 'view' depdns on the 'Const' functor, although 'Const' also implements
 -   'Applictive', the constraint is that the first parametrized type must be
 -   a 'Monoid', as defined as follows:
 -     instance Monoid b => Applicative (Const b) where
 -       ...
 -
 -    - Thus, 'view' can be used on 'Traversal' if the value to be extracted
 -      is a 'Monoid'
 -
 - Example:
 -
 - let atom1 = Atom {
 -   _element = "C"
 - , _point   = Point { _x = 1.0, _y = 2.0 }
 - }
 - let atom2 = Atom {
 -   _element = "O"
 - , _point   = Point { _x = 3.0, _y = 4.0 }
 - }
 - let molecule = Molecule { _atoms = [atom1, atom2] }
 -
 - > view (atoms . traverse . element) molecule
 - > "CO" -- 'view' works as the extracted value is of type 'String'
 -
 - > view (atoms . traverse . element) (Molecule { _atoms = [] })
 - > ""   -- 'mempty' of the 'String Monoid'
 -
 - Explanation:
 - The reason why using 'view' on 'Traversal' requires the value to be a
 - 'Monoid' is that 'Traversal' can potentially "point" to multiple values.
 - 'Monoid' has the 'mappend' method defined to combine these results. In
 - addition, if the 'Traversal' results in less than one value, 'Monoid'
 - instances provide the 'mempty' value.
 -}
