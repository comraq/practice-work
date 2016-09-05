module StackStack where

import UglyStack

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer

{-
 - In the framework that 'mtl' provides, each monad transformer in the stack
 - makes the API of a lower level available by providing instances of a host
 - of typeclasses. Suppose we create our own custom monad transformer:
 -   newtype CustomT m a = ...
 -
 - If we follow the same pattern, we would have to write a good number of
 - boilerplat instances:
 -   instance MonadReader r m => MonadReader r (Custom T m) where
 -     ...
 -
 -   instance MonadIO m => MonadIO (CustomT m) where
 -     ...
 -
 - Thus, if the underlying monad was an instance of 'MonadReader', we would
 - have to write a 'MonadReader' instance for 'CustomT' in which each
 - function in the API passes through to the corresponding function in the
 - underlying instance. This would allow higher level code to only care that
 - the stack as a whole is an instance of 'MonadReader', without knowing or
 - caring about which layer provides the "real implementation".
 -
 - Instead of relying on all of these typeclass instances to work for us
 - behind the scenes, we can be explicit by using a function (defined in
 - 'MonadTrans') called 'lift':
 -
 - > :m +Control.Monad.Trans
 - > :info MonadTrans
 - > class MonadTrans t where lift :: Monad m => m a -> t m a
 -}

-- Say we wanted to access the 'AppState' carried by 'StateT' in 'App'
implicitGet :: App AppState
implicitGet = get

{-
 - We can be explicit about getting the 'AppState' from 'App' by "lifting"
 - 'get' from 'StateT' into 'ReaderT'
 -}
explicitGet :: App AppState
explicitGet = lift get

{-
 - Note: A case in which we must explicitly use 'lift' is when we have a
 -       monad transformer stack in which instances of the same typeclass
 -       appear at multiple levels
 -}
type Foo = StateT Int (State String)

outerPut :: Int -> Foo ()
outerPut = put

innerPut :: String -> Foo ()
innerPut = lift . put

-- If we want to access a monad more than one level down the stack...
type Bar = ReaderT Bool Foo

barPut :: String -> Bar ()
barPut = lift . lift . put
