import Prelude hiding (Either, Left, Right)
import qualified Data.Map as Map

data TypeRep = TyCon String | TyApp TypeRep TypeRep

data Proxy a

class Typeable a where
  typeOf :: a -> TypeRep

data T f a = MkT (f a)

data Shape = Circle Point Float | Rectangle Point Point
  deriving Show

data Point = Point Float Float
  deriving Show

surface                                         :: Shape -> Float
surface (Circle _ r)                            =  pi * r ^ 2
surface (Rectangle (Point x1 y1) (Point x2 y2)) =  (abs $ x2 - x1) * (abs $ y2 - y1)

nudge                                             :: Shape -> Float -> Float -> Shape
nudge (Circle (Point x y) r) a b                  =  Circle (Point (x+a) (y+b)) r
nudge (Rectangle (Point x1 y1) (Point x2 y2)) a b =  Rectangle (Point (x1+a) (y1+b)) (Point (x2+a) (y2+b))

baseCircle   :: Float -> Shape
baseCircle r =  Circle (Point 0 0) r

baseRect     :: Float -> Float -> Shape
baseRect w h =  Rectangle (Point 0 0) (Point w h)

data Person = Person {
                       firstName   :: String,
                       lastName    :: String,
                       age         :: Int
                     } deriving (Show, Eq, Read)
data Car = Car {
                 company :: String,
                 model   :: String,
                 year    :: Int
               } deriving Show

data Vector a = Vector a a a
  deriving Show

vplus                               :: (Num t) => Vector t -> Vector t -> Vector t
vplus (Vector i j k) (Vector l m n) =  Vector (i+l) (j+m) (k+n)

vectMult                  :: (Num t) => Vector t -> t -> Vector t
vectMult (Vector i j k) m =  Vector (i*m) (j*m) (k*m)

scalarMult                               :: (Num t) => Vector t -> Vector t -> t
scalarMult (Vector i j k) (Vector l m n) =  i*l + j*m + k*n

data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday   
  deriving (Eq, Ord, Show, Read, Bounded, Enum)  

type PhoneNumber = String
type Name = String
type PhoneBook = [(Name,PhoneNumber)]

phoneBook :: PhoneBook
phoneBook =
  [
    ("betty","555-2938")
  , ("bonnie","452-2928")
  , ("patsy","493-2928")
  , ("lucille","205-2928")
  , ("wendy","939-8282")
  , ("penny","853-2492")
  ]

type AssocList k v = [(k, v)]
type IntMap = Map.Map Int

data Either a b = Left a | Right b
  deriving (Eq, Ord, Read, Show)

data LockerState = Taken | Free
  deriving (Show, Eq)

type Code = String
type LockerMap = Map.Map Int (LockerState, Code)

lockerLookup                  :: Int -> LockerMap -> Either String Code
lockerLookup lockerNumber map =  case Map.lookup lockerNumber map of
  Nothing            -> Left $ "Locker number " ++ show lockerNumber ++ " not found!"
  Just (state, code) -> if state /= Taken
                          then Right code
                          else Left $ "Locker: " ++ show lockerNumber ++ " is taken!"

lockers :: LockerMap  
lockers = Map.fromList   
  [
    ( 100, (Taken,"ZD39I") )  
  , ( 101, (Free,"JAH3I")  )  
  , ( 103, (Free,"IQSA9")  )  
  , ( 105, (Free,"QOTSA")  )  
  , ( 109, (Taken,"893JJ") )  
  , ( 110, (Taken,"99292") )  
  ]  

infixr 5 :::
data MyList a = Empty | a ::: (MyList a)
  deriving (Show, Read, Eq, Ord)

infixr 5 +++
(+++)               :: MyList a -> MyList a -> MyList a 
(+++) Empty ys      =  ys
(+++) (x ::: xs) ys =  x ::: xs +++ ys

data Tree a = Leaf | Node a (Tree a) (Tree a)
  deriving (Show, Read, Eq)

singleton   :: a -> Tree a
singleton x =  Node x Leaf Leaf

treeInsert                       :: (Ord a) => a -> Tree a -> Tree a
treeInsert x Leaf                =  Node x Leaf Leaf
treeInsert x (Node y left right)
  | x < y     = Node y (treeInsert x left) right
  | x > y     = Node y left (treeInsert x right)
  | otherwise = Node x left right

treeElem                       :: (Ord a) => a -> Tree a -> Bool
treeElem _ Leaf                =  False
treeElem x (Node y left right)
  | x < y     = treeElem x left
  | x > y     = treeElem x right
  | otherwise = True

data TrafficLight = Red | Yellow | Green

instance Eq TrafficLight where
  Red    == Red    = True
  Green  == Green  = True
  Yellow == Yellow = True
  _      == _      = False

instance Show TrafficLight where
  show Red    = "Red TrafficLight"
  show Green  = "Green TrafficLight"
  show Yellow = "Yellow TrafficLight"

class YesNo a where
  yesno :: a -> Bool

instance YesNo Int where
  yesno 0 = False
  yesno _ = True

instance YesNo [a] where
  yesno [] = False
  yesno _  = True

instance YesNo Bool where
  yesno = id

instance YesNo (Maybe a) where
  yesno (Just _) = True
  yesno Nothing  = False

instance YesNo (Tree a) where
  yesno Leaf = False
  yesno _    = True

instance YesNo TrafficLight where
  yesno Red = False
  yesno _   = True

yesnoif                      :: (YesNo p) => p -> a -> a -> a
yesnoif p yesResult noResult =  if yesno p then yesResult else noResult

instance Functor Tree where
  fmap _ Leaf         = Leaf
  fmap f (Node x l r) = Node (f x) (fmap f l) (fmap f r)

instance Functor (Either a) where
  fmap _ (Left x)  = Left x
  fmap f (Right x) = Right $ f x

class Tofu t where
  tofu :: j a -> t a j

data Frank a b = Frank { frankField :: b a }
  deriving Show

instance Tofu Frank where
  tofu x = Frank x

data Barry t k p = Barry { yabba :: p, dabba :: t k }

instance Functor (Barry t k) where
  fmap f (Barry { yabba = x, dabba = y }) = Barry { yabba = f x, dabba = y }
