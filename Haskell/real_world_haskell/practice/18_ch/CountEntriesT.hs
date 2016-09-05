{-
 - Monad Transformers Motivation:
 -
 - Recall in chapter 10 (Parsing a Binary Data Format) a composite monad
 - was used where in addition to the standard 'State' monad, the inner
 - value was also wrapped in an 'Either' monad, allowing for the
 - possibility of a parsing failure.
 -
 - Normal 'State' monad only carries state, if failure, it will call 'fail',
 - which calls 'error', throwing an exception that cannot be caught unless
 - in the 'IO' monad.
 -
 - Monad Transformers from the 'mtl' library provide ways to construct
 - custom monads, stacking the effects of individual monads. Instead of
 - providing standalone entity monads, the 'mtl' library provides
 - 'transformer' equivalents of common monads with the same name appended
 - with a 'T', which adds the monad effect to its underlying monad.
 -
 - ex:
 -   - 'StateT' adds mutable state to an underlying monad
 -   - 'WriterT' allows writing data when stacked on top of another monad
 -}

module CountEntriesT (listDirectory, countEntries) where

import CountEntries (listDirectory)
import System.Directory (doesDirectoryExist)
import System.FilePath ((</>))
import Control.Monad (forM_, when)
import Control.Monad.Trans (liftIO)
import Control.Monad.Writer (WriterT, tell)

{-
 - Normal 'Writer' monad type parameters:
 -   Writer w a
 -
 -   where 'w' is the type of values to be recorded
 -         'a' is the value type
 -
 - 'WriterT' transformer type parameters:
 -   WriterT w m
 -
 -   where 'w' is the type of values to be recorded
 -         'm' is the underlying monad
 -}

countEntries :: FilePath -> WriterT [(FilePath, Int)] IO ()
countEntries path = do
  contents <- liftIO . listDirectory $ path
  tell [(path, length contents)]
  forM_ contents $ \name -> do
    let newName = path </> name
    isDir <- liftIO . doesDirectoryExist $ newName
    when isDir $ countEntries newName

{-
 - There exists the following for 'WriterT':
 -   runWriterT  :: WriterT w m a -> m (a, w)
 -   execWriterT :: Monad m => WriterT w m a -> m w
 -
 - Both functions removes the 'WriterT' wrapper and give a result that is
 - wrapped in the underlying monad. The 'runWriterT' function gives both the
 - result of the action and whatever was recorded as it ran, while
 - 'execWriterT' throws away the result and just gives back what was
 - recorded.
 -}

{-
 - Note that since there is no way to directly get a value out from the 'IO'
 - monad (excluding unsafePerfromIO), there does not exist an 'IO' monad
 - transformer. Thus, the 'IO' monad will always be at the bottom of a monad
 - transformer stack.
 -}
