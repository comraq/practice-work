{-# LANGUAGE MultiParamTypeClasses
           , FlexibleInstances
           , UndecidableInstances
  #-}

module MaybeT where

import Utils

import Control.Monad.Trans
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer

newtype MaybeT m a = MaybeT {
  runMaybeT :: m (Maybe a)
}

bindMT :: Monad m => MaybeT m a -> (a -> MaybeT m b) -> MaybeT m b
x `bindMT` f = MaybeT $ do
  unwrapped <- runMaybeT x
  case unwrapped of
    Nothing -> return Nothing
    Just y  -> runMaybeT (f y)

-- Note that we are relying on the (>>=) of the underlying 'Monad m'
altBindMT :: Monad m => MaybeT m a -> (a -> MaybeT m b) -> MaybeT m b
x `altBindMT` f = MaybeT $
  runMaybeT x >>= maybe (return Nothing) (runMaybeT . f)

-- The inner monad contains a Maybe value of type 'a'
returnMT :: Monad m => a -> MaybeT m a
returnMT = MaybeT . return . Just

-- The inner monad contains a Maybe of Nothing with type 'a'
failMT :: Monad m => t -> MaybeT m a
failMT _ = MaybeT . return $ Nothing

instance Monad m => Functor (MaybeT m) where
  -- fmap f a = a >>= return . f
  fmap = curry $ ((return .) *** (>>=)) >>> swap >>> app

instance Monad m => Applicative (MaybeT m) where
  pure    = return
  -- f <*> a = f >>= (<$> a)
  (<*>) = curry $ ((>>=) *** flip fmap) >>> app

instance Monad m => Monad (MaybeT m) where
  return = returnMT
  (>>=)  = bindMT
  fail   = failMT

instance MonadTrans MaybeT where
  -- lift m = MaybeT (Just <$> m)
  lift = MaybeT . (Just <$>)

{-
 - To implement MonadTrans 'lift', we just need to 'fmap' the inner value of
 - the monad below with the 'Just' constructor, then wrap the entire monad
 - with 'MaybeT' newtype wrapper
 -}

instance MonadIO m => MonadIO (MaybeT m) where
  -- liftIO m = lift (liftIO m)
  liftIO = lift . liftIO

instance MonadState s m => MonadState s (MaybeT m) where
  get = lift get

  -- put k = lift $ put k
  put = lift . put

instance MonadReader r m => MonadReader r (MaybeT m) where
  -- ask :: MaybeT m r
  ask = lift ask

  -- local :: (s -> s) -> MaybeT m a -> MaybeT m a
  -- local f m = MaybeT . local f . runMaybeT $ m
  local = curry $ MaybeT . ((local *** runMaybeT) >>> app)

instance MonadWriter w m => MonadWriter w (MaybeT m) where
  -- tell :: w -> MaybeT m ()
  tell = lift . tell

  -- listen :: MaybeT m a -> MaybeT m (a, w)
  listen m = MaybeT $ do
    (val, log) <- listen . runMaybeT $ m
    case val of
      Nothing -> return Nothing
      Just v  -> return $ Just (v, log)

  -- pass :: MaybeT m (a, w -> w) -> MaybeT m a
  pass m = MaybeT $ do
    val <- runMaybeT m
    case val of
      Nothing     -> return Nothing
      Just (v, f) -> pass $ return (Just v, f)
