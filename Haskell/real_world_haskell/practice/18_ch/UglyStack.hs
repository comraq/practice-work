{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module UglyStack
  ( App
  , AppConfig
  , AppState
  , AppLog
  ) where

import Utils
import CountEntries (listDirectory)

import System.Directory
import System.FilePath
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer

{-
 - Since stacking a monad transformer on top of a normal monad yields a new
 - monad. We can stack monad transformers on top of stacked monads. Typical
 - use cases of stacks of monad transformers:
 -
 - - If we need to talk to the outside world, we will have an 'IO' monad at
 -   the base of the stack, otherwise we will have some normal monad (such
 -   as 'Maybe')
 -
 - - If we add a 'ReaderT' layer, we give ourselves access to read-only
 -   configuration information
 -
 - - Add a 'StateT' layer, and we gain some global state that we can modify
 -
 - - Should we need the ability to log events, we can add a 'WriterT' layer
 -}

{-
 - Below is an example of 'countEntries' where we will provide a max
 - recursion depth environment config (via 'ReaderT') and record of the
 - depth reached (via 'StateT')
 -}
data AppConfig = AppConfig {
  cfgMaxDepth :: Int
} deriving (Show)

data AppState = AppState {
  stDeepestReached :: Int
} deriving (Show)

{-
 - Transformer Stack:
 -   ReaderT
 -   StateT
 -   IO
 -
 - Since even a small stack of monad transformers can quickly develop an
 - unwieldy type name, we often use 'type alias' to reduce the lengths of
 - the type signatures.
 -}
type App = ReaderT AppConfig (StateT AppState IO)

{-
 - Note that the following type synonym can also be used:
 -   type App2 a = ReaderT AppConfig (StateT AppState IO) a
 -
 - The difference between 'App' and 'App2' is that when we try to construct
 - another type from either 'App' or 'App2' type synonym.
 -
 - For example:
 -   WriterT [String] App  a --> will work
 -   WriterT [String] App2 a --> rejected by the compiler
 -
 - This is because Haskell does not allow us to partially apply a type
 - synonym. Since 'App' does not take a type parameter, it does not pose a
 - problem. However, because 'App2' takes a type parameter, we must supply
 - some type for that parameter if we want to use 'App2' to create another
 - type.
 -
 - Also note that this restriction is only limited to type synonyms. If we
 - created our monad transformer using 'newtype', we will not have this
 - problem.
 -}

runApp :: App a -> Int -> IO (a, AppState)
runApp app maxDepth =
  let config = AppConfig maxDepth
      state  = AppState 0
  in runStateT (runReaderT app config) state

runApp' :: App a -> Int -> IO (a, AppState)
runApp' =
  let state     = AppState 0
      getconfig = AppConfig
  in curry
     $   (runReaderT *** getconfig)
     >>> app
     >>> (`runStateT` state)

{-
 - 'runApp' takes an app instance, 'Int' for 'AppConfig'. First unwraps
 - 'ReaderT' with 'runReaderT', then unwraps 'StateT' with 'runStateT',
 - finally returning the inner 'IO'
 -}

constrainedCount :: Int -> FilePath -> App [(FilePath, Int)]
constrainedCount curDepth path = do
  contents <- liftIO . listDirectory $ path
  config   <- ask
  rest     <- forM contents $ \name -> do
                let newPath = path </> name
                isDir <- liftIO $ doesDirectoryExist newPath

                if isDir && curDepth < cfgMaxDepth config
                then do
                  let newDepth = curDepth + 1
                  state <- get
                  when (stDeepestReached state < newDepth) $
                    put state { stDeepestReached = newDepth }
                  constrainedCount newDepth newPath

                else return []
  return $ (path, length contents) : concat rest

newtype MyApp a = MyA {
  runA :: ReaderT AppConfig (StateT AppState IO) a
} deriving (Functor, Applicative, Monad, MonadIO,
            MonadReader AppConfig, MonadState AppState)

runMyApp :: MyApp a -> Int -> IO (a, AppState)
runMyApp app maxDepth =
  let config = AppConfig maxDepth
      state  = AppState 0
  in runStateT (runReaderT (runA app) config) state

{-
 - By using 'newtype' and export only the 'MyApp' type constructor and
 - 'runMyApp' execution function, client code will not be able to tell that
 - the internals of the monad is a stack of monad transformers.
 -}

type AppSwapped = StateT AppState (ReaderT AppConfig IO)

runAppSwapped :: AppSwapped a -> Int -> IO (a, AppState)
runAppSwapped =
  let state               = AppState 0
      getconfig           = AppConfig
      runReaderWithConfig = flip runReaderT . getconfig
  in curry
     $   ((`runStateT` state) *** runReaderWithConfig)
     >>> app . swap

constrainedCountSwapped :: Int -> FilePath -> AppSwapped [(FilePath, Int)]
constrainedCountSwapped curDepth path = do
  contents <- liftIO . listDirectory $ path
  config   <- ask
  rest     <- forM contents $ \name -> do
                let newPath = path </> name
                isDir <- liftIO $ doesDirectoryExist newPath

                if isDir && curDepth < cfgMaxDepth config
                then do
                  let newDepth = curDepth + 1
                  state <- get
                  when (stDeepestReached state < newDepth) $
                    put state { stDeepestReached = newDepth }
                  constrainedCountSwapped newDepth newPath

                else return []
  return $ (path, length contents) : concat rest

data AppLog = AppLog {
  filePath :: FilePath
, count    :: Int
} deriving (Show)

type AppWithWriter = WriterT [AppLog] App

runAppWithWriter :: AppWithWriter a -> Int -> IO ([AppLog], AppState)
runAppWithWriter app maxDepth =
  let config = AppConfig maxDepth
      state  = AppState 0
  in runStateT (runReaderT (execWriterT app) config) state

runAppWithWriter' :: AppWithWriter a -> Int -> IO ([AppLog], AppState)
runAppWithWriter' =
  let state              = AppState 0
      getconfig          = AppConfig
      runWriterAndReader = runReaderT . execWriterT
  in curry
     $   (runWriterAndReader *** getconfig)
     >>> app
     >>> (`runStateT` state)

constrainedCountWithWriter :: Int -> FilePath -> AppWithWriter ()
constrainedCountWithWriter curDepth path = do
  contents <- liftIO . listDirectory $ path

  let logEntry = AppLog path $ length contents
  tell [logEntry]

  config   <- ask
  forM contents $ \name -> do
    let newPath = path </> name
    isDir <- liftIO $ doesDirectoryExist newPath

    if isDir && curDepth < cfgMaxDepth config
    then do
      let newDepth = curDepth + 1
      state <- get
      when (stDeepestReached state < newDepth) $
        put state { stDeepestReached = newDepth }
      constrainedCountWithWriter newDepth newPath

    else return ()

  return ()
