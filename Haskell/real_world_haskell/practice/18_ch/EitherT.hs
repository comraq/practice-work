{-# LANGUAGE MultiParamTypeClasses
           , FlexibleInstances
           , UndecidableInstances
  #-}

module EitherT where

import Utils

import Control.Monad.Trans
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer

newtype EitherT e m a = EitherT {
  runEitherT :: m (Either e a)
}

bindET :: Monad m => EitherT e m a -> (a -> EitherT e m b) -> EitherT e m b
x `bindET` f = EitherT $
  runEitherT x >>= either (return . Left) (runEitherT . f)

returnET :: Monad m => a -> EitherT e m a
returnET = EitherT . return . Right

failET :: Monad m => e -> EitherT e m a
failET = EitherT . return . Left

instance Monad m => Functor (EitherT e m) where
  -- fmap f a = a >>= return . f
  fmap = curry $ ((return .) *** (>>=)) >>> swap >>> app

instance Monad m => Applicative (EitherT e m) where
  pure    = return
  -- f <*> a = f >>= (<$> a)
  (<*>) = curry $ ((>>=) *** flip fmap) >>> app

instance Monad m => Monad (EitherT e m) where
  return = returnET
  (>>=)  = bindET

instance MonadTrans (EitherT e) where
  -- lift m = EitherT (Right <$> m)
  lift = EitherT . (Right <$>)

instance MonadIO m => MonadIO (EitherT e m) where
  -- liftIO m = lift (liftIO m)
  liftIO = lift . liftIO

instance MonadState s m => MonadState s (EitherT e m) where
  get = lift get

  -- put k = lift $ put k
  put = lift . put

instance MonadReader r m => MonadReader r (EitherT e m) where
  -- ask :: EitherT e m r
  ask = lift ask

  -- local :: (s -> s) -> EitherT e m a -> EitherT e m a
  -- local f m = EitherT . local f . runEitherT $ m
  local = curry $ EitherT . ((local *** runEitherT) >>> app)

instance MonadWriter w m => MonadWriter w (EitherT e m) where
  -- tell :: w -> EitherT e m ()
  tell = lift . tell

  -- listen :: EitherT e m a -> EitherT e m (a, w)
  listen m = EitherT $ do
    (val, log) <- listen . runEitherT $ m
    case val of
      Left e  -> return $ Left e
      Right v -> return $ Right (v, log)


  -- pass :: EitherT m (a, w -> w) -> EitherT m a
  pass m = EitherT $ do
    val <- runEitherT m
    case val of
      Left e       -> return $ Left e
      Right (v, f) -> pass $ return (Right v, f)
