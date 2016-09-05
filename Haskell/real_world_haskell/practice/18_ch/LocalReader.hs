{-
 - The signature of the 'MonadReader' typeclass is as follows:
 -
 - class Monad m => MonadReader r m | m -> r where
 -   ask   :: m r
 -   local :: (r -> r) -> m a -> m a
 -
 - The type variable 'r' represents the immutabe state that the reader monad
 - carries around. The 'Reader r' monad is an instance of the MonadReader
 - class, as is the 'ReaderT r m' monad transformer.
 -
 - Note: 'local' is a a function that temporarily modifes the environment
 - via the function '(r -> r)' and then executes its contained action in the
 - modified environment.
 -}

import Control.Monad.Reader
import Control.Monad.Identity

myName :: String -> ReaderT String Identity String
myName step = do
  name <- ask
  return $ step ++ ", I am " ++ name

{-
 - Note that 'ReaderT r Identity' is a 'ReaderT' representation of a plain
 - 'Reader' monad.
 -
 - The 'r' type parameter is a string as the environment is of type 'String'.
 -
 - The final 'String' type parameter of the 'Identity' monad indicates that
 - the contained value of 'myName' is also of type 'String'.
 -}

localExample :: Reader String (String, String, String)
localExample = do
  a <- myName "First"
  b <- local (++"dy") (myName "Second")
  c <- myName "Third"
  return (a, b, c)

{-
 - Example Usage:
 - > runReader localExample "Fred"
 - > ("First, I am Fred", "Second, I am Freddy", "Third, I am Fred")
 -
 - Note that the environment is just "Fred" for "First" and "Third", but
 - became "Freddy" for "Second", due to the usage of 'local' appending
 - '(++dy)' to the passed in environment "Fred".
 -}

{-
 - Typical Monad Transformers from 'mtl' are also instances of other
 - typeclasses such as 'Functor', 'MonadIO', 'MonadPlus' and etc...
 -}
