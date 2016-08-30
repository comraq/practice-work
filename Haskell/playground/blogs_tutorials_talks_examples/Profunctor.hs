{-# LANGUAGE RankNTypes, ImpredicativeTypes, TupleSections, ConstraintKinds #-}
module Profunctor where

import Utils

------- Functor (Covariant) -------

{-
 - class Functor f where
 -   fmap :: (a -> b) -> f a -> f b
 -}

newtype Burrito filling = Tortilla { getFilling :: filling }

instance Functor Burrito where
  fmap = (Tortilla .) . (. getFilling)
  -- fmap = (. getFilling) >>> (Tortilla .)
  -- fmap f = Tortilla . f . getFilling


------- Contravariant -------
{-
 - Customers only eat 'cooked' food.
 -
 - Customers can only be fed when cooked food (f b) is available.
 - Suppose now there is a way to 'cook' food (f :: a -> b)
 -   (ie: uncooked food -> cooked food).
 -
 - Customers can now be fed when there is uncooked food (f a).
 -}

class Contravariant f where
  cmap :: (a -> b) -> f b -> f a

newtype Customer filling = Customer { eat :: filling -> IO () }

instance Contravariant Customer where
  cmap = (Customer .) . (. eat) . (>>>)
  -- cmap f = Customer . (. f) . eat
  -- cmap g (Customer f) = Customer $ f . g

{-
 - Op is the function arrow where the parameter is fixed on the return type.
 - Input argument is the variant.
 -}
newtype Op b a = Op { getOp :: a -> b }

instance Contravariant (Op b) where
  cmap = (Op .) . (. getOp) . (>>>)

{-
 - Comparison between 'Covariant Functors' and 'Contravariants':
 -
 - - Type arguments of 'Covariant Functors' appear only in the positive
 -   position
 - - Type arguments of 'Contravariant' appear only in the the negative
 -   position
 -
 -   ex: (negative positions are marked with primed "'" and does not
 -        indicate a difference in type)
 -     - a
 -     - a' -> a
 -     - a' -> a' -> a
 -       Note: the first 2 'a's are negative, think of a -> a -> a as uncurried:
 -             (a, a) -> a
 -     - (a -> a') -> a
 -       Note: the second 'a' is still negative but the first 'a' is positive,
 -             because we need to travese 2 function arrows from the
 -             rightmost 'a' to the leftmost 'a', in which we encounter
 -             2 inversions of polarity, ~ . ~ === id
 -               where (~) === negation
 -     - ((a' -> a) -> a') -> a
 -
 -   Note: If the type argument is only in positive positions, then the type
 -         argument varies 'covariantly' (we can instantiate a 'covariant
 -         Functor').
 -         If only in negative positions, then the type argument varies
 -         'contravariantly' (we can instantiate a 'contravariant').
 -         If the type argument are in both positive and negative positions,
 -         then no 'covariant functors' nor 'contravariants' can be
 -         instantiated
 -
 -
 - - cmap over contravariant (-> z) 'extends' the function on its input/argument
 -   type.
 -   - fmap (a -> b) >>> fmap (b -> c)
 -   - f a -> f b -> f c and ...
 -
 - - fmap over covariant ((->) r) 'extends' the function on its output/return
 -   type.
 -   - cmap (b -> c) >>> cmap (a -> b)
 -   - f c -> f b -> f a and ...
 -}

class Invariant f where
  imap :: (b -> a) -> (a -> b) -> f a -> f b

newtype Endo a = Endo { getEndo :: a -> a }

instance Invariant Endo where
  imap = ((Endo .) . (. getEndo)) .* dimap
  -- imap f g (Endo e) = Endo (g . e . f)

-- Isomorphism, a pair of functions that are 'inverses' (dual) to each other
data Iso a b = Iso (a -> b) (b -> a)

iso :: Invariant f => Iso a b -> f a -> f b
iso (Iso to from) = imap from to

class Profunctor p where
  dimap :: (a -> b) -> (c -> d) -> p b c -> p a d

instance Profunctor (->) where
  dimap = (>>>) >>> (*.)
  -- dimap f g k = g . k . f

{-
 - Dimap maps 'contravariantly' with its first argument
 - - 'extends' the input/argument from b to a with (a -> b)
 -
 - Dimap then maps 'covariantly' with its second argument
 - - 'extends' the output/return from c to d with (c -> d)
 -}

isoP :: Profunctor p => Iso a b -> p a a -> p b b
isoP (Iso from to) = dimap to from

{-
 - For Profunctor (->), isoP transforms a (a -> a) to (b -> b)
 - - extends the argument from a to b with (b -> a)
 - - extends the return type from a to b with (a -> b)
 -}

swapping :: Profunctor p => p (a, b) (x, y) -> p (b, a) (y, x)
swapping = dimap swap swap

assoc :: Profunctor p => p ((a, b), c) ((x, y), z) -> p (a, (b, c)) (x, (y, z))
assoc = dimap assocLeft assocRight

assocLeft :: (a, (b, c)) -> ((a, b), c)
assocLeft = ((((,) . fst)
  >>> (. fst)
  >>> (&&& snd)) &&& snd)
  >>> app

assocRight :: ((a, b), c) -> (a, (b, c))
assocRight = (fst &&& ((flip (,) . snd)
  >>> (. snd)
  >>> (fst &&&)))
  >>> swap
  >>> app

{-
 - Both 'swapping' and 'assoc' are isomorphic as the input and output are
 - both of the same Profunctor structure. (ie: They can be composed)
 -}

{-
 - Types of Compositions:
 -
 - swapping . swapping :: Profunctor p => p (a, b) (x, y) -> p (a, b) (x, y)
 -
 - assoc . swapping :: Profunctor p =>
 -   p (a, (b, c)) (x, (y, z)) -> p (b, (c, a)) (y, (z, x))
 -}

newtype Forget r a b = Forget { runForget :: a -> r }

instance Profunctor (Forget r) where
  dimap f _ (Forget forget) = Forget (forget . f)

newtype Star f a b = Star { runStar :: a -> f b }

instance Functor f => Profunctor (Star f) where
  dimap f g (Star star) = Star $ fmap g . star . f

{-
 - Note: Star is named after the literature notation of lifting a function
 -       into a 'Functor' in 'f^*'
 - Note: As regular function arrows, the parameters 'a' and 'b' of 'Star'
 -       are still in the same positive/negative positions as regular
 -       functions.
 -       ie: a is negative (contravariant)
 -           b is positive (covariant)
 - Note: 'f' being a 'Functor' does not alter but rather preserves the
 -       original polarity of its arguments (hence a is still positive)
 -
 - ex: let starJ = Star Just
 -         plusOneOnArgumentAndShowResult = dimap (+ 1) show starJ
 -     in runStar $ plusOneOnArgumentAndShowResult 10
 -     -- Just "11"
 -        Note: the result is a Just containing the string 11 due to the
 -              follwing:
 -              - (+ 1) to the input argument // (10 + 1) -> 11
 -              - wraps the value in Just     // Just 11
 -              - fmap show over (Just 11)    // Just "11"
 -}

newtype Costar f a b = Costar { runCostar :: f a -> b }

instance Functor f => Profunctor (Costar f) where
  dimap f g (Costar costar) = Costar $ g. costar . fmap f

{-
 - ex: let costarSum = Costar (sum :: [Int] -> Int)
 -         mapPlusOneToListAndSumThenShow = dimap (+ 1) show costarSum
 -     in runCostar $ mapPlusOneToListAndSumThenShow [ 1, 2, 3 ]
 -     -- "9"
 -        Note: Like the 'Star' exmaple, we map over the list with '(+ 1)',
 -              then apply the 'sum' operation in Costar and finally
 -              'show' the results
 -}

{-
 - Note: For 'Fold':
 -       - m is usally a monoid (the first argument, a higher order
 -         function, mostly likely returns a monoidal value)
 -       - b is still covariant
 -       - a is still contravariant
 -}
newtype Fold m a b = Fold { runFold :: (b -> m) -> a -> m }

instance Profunctor (Fold m) where
  dimap f g (Fold fold) = Fold $ \k -> fold (k . g) . f

newtype Mealy a b = Mealy { runMealy :: a -> (b, Mealy a b) }

instance Profunctor Mealy where
  dimap f g = go
    where go (Mealy mealy) = Mealy $ (g *** go) . mealy . f

{-
 - 'Strong' is the class that operates on 'Tuples'
 - - In contrast with 'Choice', 'Strong' acts on "Product Types"
 -}
class Profunctor p => Strong p where
  -- Default implementation for first' unless overwritten
  first' :: p a b -> p (a, x) (b, x)
  first' = swapping . second'

  -- Default implementation for second' unless overwritten
  second' :: p a b -> p (x, a) (x, b)
  second' = swapping . first'

instance Strong (->) where
  first'  = first
  second' = second

{-
 - Instances of the 'Strong' "subclass" for the previously defined
 - 'Profunctor' instances:
 -   - instance Strong (Forget r)
 -   - instance Functor f => Strong (Star f)
 -   - instance Comonad w => Strong (Costar w)
 -   - instance Strong (Fold m)
 -   - instance Mealy
 -}

{-
 - Compositions of 'first'' and 'second''
 - - first' . first'  :: Strong p => p a b -> p ((a, x), y) ((b, x), y)
 - - first' . second' :: Strong p => p a b -> p ((x, a), y) ((x, b), Y)
 -
 - "Methods" in 'Strong' also compose with those in 'Profunctor'!
 - - assoc . first'    :: Strong p => p (a, b) (x, y)  -> p (a, (b, z)) (x, (y, z))
 - - first' . swapping :: Strong p => p (a, b) (x1, y) -> p ((b, a), x) ((y, x1), x)
 - - swapping . first' === second'
 -}

type IsoP s t a b  = forall p. Profunctor p => p a b -> p s t
type LensP s t a b = forall p. Strong p     => p a b -> p s t

{-
 - Note: Every Iso is automatically a Lens!
 - We want Profunctor Lens to be Isomorphsims (as all the Profunctor
 - functions are isomorphic).
 -}

-- Every profunctor lens has a normal form
nf :: (s -> (a, x)) -> ((b, x) -> t) -> LensP s t a b
nf = (. first') .* dimap
-- nf f g pab = dimap f g (first' pab)

{-
 - Explanation of 'nf' (normal form):
 - Given to functions (f :: s -> (a, x)) and (g :: (b, x) -> t), 'nf' can
 - return a LensP s t a b (ie: Profunctor p => p a b -> p s t)
 - - ex: If the profunctor is the regular function arrow, we get
 -       the original function (a -> b) is transformed into (s, t)
 -       - This can be explained by dimap using 'f' and 'g'
 -       - 'a' is contravariant, dimap extends the argument with 'f'
 -       - 'b' is covariant, dimap extends the return type with 'g'
 -       - hence, p a b -> p s t
 -
 - Gist: 'Strong' provide access to big data structures by tupling up the big
 -       data structures ('s' and 't') with smaller data structures ('a' and 'b') with
 -       some non-relevant 'x'
 -}

appIsoP :: Profunctor p => (IsoP s t a b, p a b) -> p s t
appIsoP (iso, f) = iso f

appLensP :: Strong p => (LensP s t a b, p a b) -> p s t
appLensP (lens, f) = lens f

iso2lens :: IsoP s t (a, x) (b, x) -> LensP s t a b
iso2lens = curry $ appIsoP . (id *** first')
-- iso2lens iso pab = iso $ first' pab

lens2iso :: LensP s t a b -> IsoP s t (a, x) (b, x)
lens2iso = undefined

instance Strong (Forget r) where
  -- first' :: Strong p => p a b -> p (a, x) (b, x)
  first' = Forget . fst .* first . runForget
  -- 1) 'runForget' to first pull out the 'forget' function (a -> r)
  -- 2) pass that to 'Arrow.first' (which lifts the (a -> r) to operate on an
  --    arbitrary tuple of (a, x) -> (r, x)
  -- 3) compose the result of the lifted function with 'fst' (to return r as
  --    specified by the definition of 'Forget')
  -- 4) rewrap with 'Forget'

  -- Likewise for second'
  second' = Forget . snd .* second . runForget

{-
 - We use the 'Forget id' profunctor for 'get'
 - This is because we do not want to extend the argument input (the data
 - structure) before we pul out the value from it. Thus 'Forget id' will
 - yield the correct result
 -}

-- view
get :: LensP s t a b -> s -> a
get = runForget . appLensP . (, Forget id)
-- get lens = runForget (lens (Forget id))

-- set
set :: LensP s t a b -> b -> s -> t
set = curry $ appLensP . (id *** const)
-- set lens b = lens (const b)

-- over
modify :: LensP s t a b -> (a -> b) -> s -> t
modify = appLensP .* (,)
-- modify lens f = lens f

class Profunctor p => Choice p where
  left'  :: p a b -> p (Either a x) (Either b x)
  right' :: p a b -> p (Either x a) (Either x b)

{-
 - Explanation/Intuition:
 - - left':
 -   - given a profunctor 'p a b', returns a profunctor that can take an
 -     'Either a x' and return 'Either b x'
 -     ie: if Either is Left, 'Either a _' will be transformed to 'Either b _'
 -         otherwise (if EIther is Right), just return 'Either _ x'
 - - likewise for right'
 -}

{-
 - Comparison:
 -
 - 'Choice is the class that operates on Either'
 - - In contrast with 'Strong', 'Choice' acts on "Sum Types"
 -
 - Gist: 'Choice' provide access to big data structures by pairing up big
 -       data structures ('s' and 't') with smaller data structures ('a' and 'b') with
 -       an either non-relevant 'x'
 -
 - Note: This is similar to 'Prism' from standard Lens
 -}
instance Choice (->) where
  left'  = left
  right' = right
  -- left'  f (Left a)  = Left (f a)
  -- left'  _ (Right x) = Right x
  -- right' _ (Left x)  = Left x
  -- right' f (Right a) = Right (f a)

{-
 - Examples of Choice Profunctor instances:
 -   - instance Monoid m      => Choice (Forget m)
 -     - Note: Need monoid 'unit' value if choice dosn't match
 -
 -   - instance Applicative f => Choice (Star f)
 -   - instance Comonad w     => Choice (Costar w)
 -   - instance Monoid m      => Choice (Fold m)
 -   - instance                  Choice Mealy
 -}

{-
 - Compositions of Choice operations:
 - left' . left'  :: Choice p => p a b -> p (Either (Either a x) y)
 -                                          (Either (Either b x) y)
 - left' . right' :: Choice p => p a b -> p (Either (Either x a) y)
 -                                          (Either (Either x b) y)
 -}

type Prism s t a b = forall p. Choice p => p a b -> p s t

{-
 - 0-1 Traversal (Exactly 1 occurence or 0)
 -
 - ex:
 -     first' . left' :: (Strong p, Choice p) => p a b ->
 -                                               p (Either a x, y) (Either b x, y)
 -}
type AffineTraversal s t a b = forall p. (Strong p, Choice p) => p a b -> p s t

iso2prism :: IsoP s t (Either a x) (Either b x) -> Prism s t a b
iso2prism = curry $ appIsoP . (id *** left')
-- iso2prism iso pab = iso (left' pab)

{-
 - A prism asserts that the family of types given by 's' and 't' is
 - (uniformly) isomorphic to a "sum"
 -
 - Prisms identities a big data structure as a "sum" of smaller data
 - structures (ex: either left or right)
 -}

{-
 - Defining Profunctor Arrows and its Instances:
 -
 - class (Strong a, Category a) => Arrow a
 -
 - instance                  Arrow (->)
 - instance Applicative f => Arrow (Star f)
 - instance Comonad w     => Arrow (Costar w)
 - instance Monoid m      => Arrow (Fold m)
 - instance                  Arrow Mealy
 -
 - assoc :: Arrow a => a b c -> a (b, x) (c, x)
 - assoc arr = proc (b, x) -> do
 -               c -< arr -< b
 -               returnA -< (c, x)
 -}

type Optic c s t a b = forall p. c p => p a b -> p s t

{-
 - Generalizing the Profunctors:
 - - type Iso       = Optic Profunctor
 - - type Lens      = Optic Strong
 - - type Prism     = Optic Choice
 - - type Traversal = Optic Traversing
 - - type Grate     = Optic Closed
 - - type SEC       = Optic ((->) ~)
 -
 - where '~' means "is equal to"
 -}

{-
 - Traversing generalizes over both 'Lens' and 'Prism'
 - by enforcing the profunctor to be both an instance of 'Strong' and
 - 'Choice'
 -}
class (Strong p, Choice p) => Traversing p where
  traversing :: Traversable t => p a b -> p (t a) (t b)

{-
 - Closed class returns a profunctor that provides mapping from a keyspace
 - 'x -> a' to 'x -> b'
 -
 - Intuition: Shaped like a cone (mapping from 'x -> a', then transformed to
 -            mapping from 'x -> b')
 -}
class Profunctor p => Closed p where
  closed :: p a b -> p (x -> a) (x -> b)

{-
 - Notion:
 - - Iso             | Profunctor     | s ~ a
 - - Lens            | Strong         | s ~ (a, x)
 - - Prism           | Choice         | s ~ Either a x
 - - Traversal       | Traversing     | s ~ t a
 - - Grate           | Closed         | s ~ x -> a
 - - AffineTraversal | Strong, Choice | s ~ Either (a, x) y
 -}

{-
 - Real World Example:
 -
 - data UI b a = UI { runUI :: (a -> IO()) -> b -> IO Widget }
 -
 - instance Profunctor UI ...
 - instance Strong UI ...
 - instance Choice UI ...
 - instance Traversing UI ...
 -
 - type UI' state = UI state state
 -
 - animate :: UI' state -> state -> IO ()
 - animate ui state = do
 -   widget <- runUI ui (animate ui) state
 -   render widget
 -
 - By defining 'UI' state' as a Profunctor, we effectively get:
 -   first'     :: UI' state -> UI' (state, x)
 -   left'      :: UI' state -> UI' (Either state x)
 -   traversing :: UI' state -> UI' [state]
 -}
