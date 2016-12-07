{-# LANGUAGE TypeOperators
           , TypeFamilies
           , ScopedTypeVariables
 #-}

module JSONTemplate where

import Control.Monad.Free (Free(..))
import qualified Control.Monad.Trans.Free as CMTF
import Data.Function (fix)
import Data.Functor.Foldable
import qualified Data.Map as M
import Data.Maybe (fromMaybe)

import JSONParser


-- The coproduct of pattern functors f and g
data (f :+: g) r = Inl (f r) | Inr (g r)

-- The product of pattern functors f and g
data (f :*: g) r = (f r) :*: (g r)

{-
   -- The free monad pattern functor
   data FreeF f a r = FreeF (f r) | PureF a
     deriving (Show, Functor)

   -- The cofree monad pattern functor
   data CofreeF f a r = CofreeF (f r) a
-}

-- Note that Free is already defined as an instance of Base
--   type instance Base (Free f a) = FreeF f a
type Ctx = Free

term :: f (Ctx f a) -> Ctx f a
term = Free

hole :: a -> Ctx f a
hole = Pure

type instance Base JSValue = JSValueF

instance Recursive JSValue where
  project JSNull       = JSNullF
  project (JSBool b)   = JSBoolF b
  project (JSNumber n) = JSNumberF n
  project (JSString s) = JSStringF s
  project (JSArray xs) = JSArrayF xs
  project (JSObject o) = JSObjectF o


rProject :: Base JSValue JSValue -> JSValue
rProject JSNullF       = JSNull
rProject (JSBoolF b)   = JSBool b
rProject (JSNumberF n) = JSNumber n
rProject (JSStringF s) = JSString s
rProject (JSArrayF xs) = JSArray xs
rProject (JSObjectF o) = JSObject o

type Name       = String
type JSTemplate = Ctx JSValueF Name

fillHoles :: forall f a. Functor f => (a -> Fix f) -> Ctx f a -> Fix f
fillHoles g = cata alg where
  alg :: Base (Ctx f a) (Fix f) -> Fix f
  alg (CMTF.Free t) = Fix t
  alg (CMTF.Pure a) = g a

-- fillHoles' :: forall t t1. (Base t1 ~ CMTF.FreeF JSValueF t, Recursive t1) => (t -> JSValue) -> t1 -> JSValue
fillHoles' :: forall a. (a -> JSValue) -> Ctx JSValueF a -> JSValue
fillHoles' g = cata alg where
  alg :: Base (Ctx JSValueF a) JSValue -> JSValue
  alg (CMTF.Free f) = rProject f
  alg (CMTF.Pure a) = g a

pJSValue :: CharParser () JSValue
pJSValue = fix $ \p -> rProject <$> pJSValueF p

pVar :: CharParser () Name
pVar = char '$' *> between (char '{') (char '}') (many alphaNum)

pJSTemplate :: CharParser () JSTemplate
pJSTemplate = fix $ \p -> term <$> pJSValueF p <|> hole <$> pVar

-- > ex1
-- > JSArray [JSObject [("foo",JSNumber 42.0)]]

env1  = M.fromList [("a", JSNumber 42)]
temp1 = parse' pJSTemplate "[{\"foo\":${a}}]"
ex1   = fillHoles' (vlookup env1) temp1

vlookup :: Ord a => M.Map a JSValue -> a -> JSValue
vlookup env = fromMaybe JSNull . (`M.lookup` env)
