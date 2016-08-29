{-# LANGUAGE GeneralizedNewtypeDeriving, TupleSections #-}

import Utils
import Control.Monad.Trans
import Control.Monad.Trans.Either
import Control.Monad.Trans.Except
import Control.Exception (catch, IOException)
import Data.Char (toUpper)

import Control.Monad.Reader
import Control.Monad.Except

------- Catching Exceptions -------

{-
 - readFile :: FilePath -> IO String
 -
 - Note: Not in type signature but the 'readFile' function may throw
 -       exception!
 -       ie: unexpected bahaviour, failure is invisible from the type
 -}

------- Without Monad Transformers -------

safeLoadFile :: FilePath -> IO (Either IOException String)
safeLoadFile file = (Right <$> readFile file) `catch` (pure . Left)

safeLoadFile' :: FilePath -> IO (Either IOException String)
safeLoadFile' = (`catch` (pure . Left)) . fmap Right . readFile

fileChars :: FilePath -> IO (Either IOException Int)
fileChars = fmap (fmap length) . safeLoadFile

safePrintFile :: FilePath -> IO (Either IOException ())
safePrintFile file = safeLoadFile file >>= traverse putStrLn

safePrintFile' :: FilePath -> IO (Either IOException ())
safePrintFile' = (>>= traverse putStrLn) . safeLoadFile

{-
 - Too Many Layers! Unwrap every layer at every step.
 -
 - Use Monad Transformers!
 -
 - ex: EitherT :: * -> (* -> *) -> * -> *
 -     EitherT :: m (Either e a) -> EitherT e m a
 -     runEitherT :: EitherT e m a -> m (Either e a)
 -     - where m can be any * -> *
 -}

step1 :: Applicative m => EitherT String m a
step1 = EitherT (pure . Left $ "failure message")

step2 :: String -> EitherT e IO ()
step2 = EitherT . fmap Right . putStrLn

step3 :: EitherT String IO ()
step3 = step1 >>= step2

failStep :: IO (Either String ())
failStep = runEitherT step3

step4 :: Applicative m => EitherT e m String
step4 = EitherT (pure . Right $ "success message")

step4a :: Monad m => EitherT e m String
step4a = pure "success message" -- Can also be 'return "success message"'

step5 :: EitherT e IO ()
step5 = step4 >>= step2

step5a :: EitherT e IO ()
step5a = step4a >>= step2

successStep :: IO (Either e ())
successStep = runEitherT step5

successStepa :: IO (Either e ())
successStepa = runEitherT step5a

{-
 - EitherT . fmap Right :: Functor m => m a -> EitherT e m a
 -   ==
 - lift :: (MonadTrans t, Monad m) => m a -> t m a
 -
 - Note: MonadTrans t :: (* -> *) -> * -> *
 -
 - instance MonadTrans (EitherT e) where
 -   lift = EitherT . liftM Right
 -}

step2a :: String -> EitherT e IO ()
step2a = lift . putStrLn

{-
 - Because IO cannot be promoted into a Monad Transformer (contents cannot be
 - pulled out until runtime), the IO monad will always live in the bottom of
 - the Monad Transformer stack.
 -
 - Thus, 'liftIO' is a specialized 'lift' which can lift the IO value from
 - an arbitrary height stack of monad transformers, pulling out the IO
 - value.
 -
 - The advantage of using 'liftIO' vs 'lift' (even when the stack only
 - consists of 1 monad transformer on top of IO) is that liftIO can
 - successfully lift the IO action regardless of the height of the monad
 - transformer stack (ie: changes to the stack will not affect the results
 - of 'liftIO' whereas the depths/numbers of 'lift' necessary will have to
 - be tuned accordingly to the current monad transformer stack composition)
 -
 - ex: type T = ReaderT Int (WriterT String IO) Bool
 -
 -     > :t \x -> (lift x :: T)
 -     \x -> (lift x :: T) :: WriterT String IO Bool -> T
 -
 -     > :t \x -> (liftIO x :: T)
 -     \x -> (liftIO x :: T) :: IO Bool -> T
 -
 - @link - http://stackoverflow.com/questions/3921237/haskell-lift-vs-liftio
 -}

step2b :: String -> EitherT e IO ()
step2b = liftIO . putStrLn

{-
 - ReaderT :: * -> (* -> *) -> * -> *
 - ReaderT :: (r -> m a) -> ReaderT r m a
 - runReaderT :: ReaderT r m a -> r -> m a
 - - where m can be any * -> *
 -}

step6 :: ReaderT String IO ()
step6 = ReaderT putStrLn

step7 :: ReaderT String IO ()
step7 = ReaderT (putStrLn . fmap toUpper)

-- Since value of step 6 is (), step 7 can drop the value with '>>'
step8 :: ReaderT String IO ()
step8 = step6 >> step7

step9 :: String -> IO ()
step9 = runReaderT step8

{-
 - ExceptT can be used in a similar manner as EitherT
 -
 - ie: newtype App e c a = App (EitherT e (ReaderT c IO) a)
 - Stack:
 -        EitherT e
 -        ReaderT c
 -        IO
 -
 - ExceptT :: * -> (* -> *) -> * -> *
 - ExceptT :: m (Either e a) -> ExceptT e m a
 - runExceptT :: ExceptT e m a -> m (Either e a)
 -
 -}
data AppError = AppDbError DbError | AppLogicError LogicError

data AppEnv = AppEnv {
  appEnvDb    :: DbEnv,
  appEnvLogic :: LogicEnv
}

newtype App a = App { unApp :: ExceptT AppError (ReaderT AppEnv IO) a}
  deriving
  ( Functor
  , Applicative
  , Monad
  , MonadReader AppEnv
  , MonadError AppError
  , MonadIO
  )

runApp :: AppEnv -> App a -> IO (Either AppError a)
runApp e = flip runReaderT e . runExceptT . unApp

runApp' :: AppEnv -> App a -> IO (Either AppError a)
runApp' = (. (runExceptT . unApp)) . flip runReaderT

{-
 - ask :: MonadReader r m => m r
 - asks :: MonadReader r m => (r -> a) -> m a
 -}

exampleAsk :: Monad m => String -> m (String, String)
exampleAsk = runReaderT $ do
  r      <- ask
  rUpper <- asks (fmap toUpper)
  pure (r, rUpper)

exampleAsk' :: Monad m => String -> m (String, String)
exampleAsk' = runReaderT $
  ask >>= (asks (fmap toUpper) >>=) . ((pure .) . (,))

{-
 - throwError :: MonadError e m => e -> m a
 - catchError :: MonadError e m => m a -> (e -> m a) -> m a
 -}

exampleThrow :: Monad m => String -> m (Either String a)
exampleThrow = runExceptT . throwError

exampleCatch :: String -> IO (Either String String)
exampleCatch e = runExceptT . catchError (throwError e) $
  \msg -> do
    liftIO (putStrLn $ "ERROR! MESSAGE: " ++ msg)
    pure "All OK Now!"

exampleCatch' :: String -> IO (Either String String)
exampleCatch' = runExceptT . (throwError >>>
  flip catchError (liftIO . putStrLn . ("ERROR! MESSAGE: " ++)) >>> (>> pure "All OK Now!"))

type DbEnv = String
type DbError = String
newtype Db a = DB {
  unDb :: ExceptT DbError (ReaderT DbEnv IO) a
}
runDb :: DbEnv -> Db a -> IO (Either DbError a)
runDb = undefined

type LogicEnv = String
type LogicError = String
newtype Logic a = Logic {
  unLogic :: ExceptT LogicError (Reader LogicEnv) a
}
runLogic :: LogicEnv -> Logic a -> Either LogicError a
runLogic = undefined

liftLogic :: Logic a -> App a
liftLogic l = do
  c <- asks appEnvLogic
  either (throwError . AppLogicError) pure $ runLogic c l

liftLogic' :: Logic a -> App a
liftLogic' = (asks appEnvLogic >>=) . (either (throwError . AppLogicError) pure .) . flip runLogic
