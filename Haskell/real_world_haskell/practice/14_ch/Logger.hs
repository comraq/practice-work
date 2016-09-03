{-# LANGUAGE TupleSections #-}

module Logger
  ( Logger
  , Log
  , runLogger
  , record
  ) where

import Utils

type Log = [String]
newtype Logger a = Logger { execLogger :: (a, Log) }

instance Functor Logger where
  fmap f (Logger (a, xs)) = Logger (f a, xs)

instance Applicative Logger where
  pure    = return
  f <*> a = f >>= (<$> a)

instance Monad Logger where
  return a = Logger (a, [])

  m >>= k = let (a, w) = execLogger m
                n      = k a
                (b, x) = execLogger n
            in Logger (b, w ++ x)

{-
 - Adds an entry to the log, hence no useful return value
 -
 - Note: usually, a monad will provide one or more helper functions like
 -       'record'. These are the means for accessing the special behaviours of
 -       that monad.
 -}
record :: String -> Logger ()
-- record s = Logger ((), [s])
record = Logger . ((),) . (:[])

runLogger :: Logger a -> (a, Log)
runLogger = execLogger

globToRegex :: String -> Logger String
globToRegex cs =
  globToRegex' cs >>= \ds ->
  return ('^':ds)

globToRegexPF :: String -> Logger String
globToRegexPF = globToRegex' >>> (>>= return . ('^':))

globToRegex' :: String -> Logger String
globToRegex' ""       = return "$"
globToRegex' ('?':cs) =
  record "any" >>
  globToRegex' cs >>= \ds ->
  return $ '.':ds

globToRegex' ('*':cs) = do
  record "kleene star"
  ds <- globToRegex' cs
  return $ ".*" ++ ds

globToRegex' ('[':'!':c:cs) =
  record "character class, negative" >>
  charClass cs >>= \ds ->
  return $ "[^" ++ c:ds

globToRegex' ('[':c:cs) =
  record "character class" >>
  charClass cs >>= \ds ->
  return $ "[" ++ c:ds

globToRegex' ('[':_) =
  fail "Unterminated character class"

charClass_wordy :: String -> Logger String
charClass_wordy (']':cs) =
  globToRegex' cs >>= \ds ->
  return $ ']':ds
charClass_wordy (c:cs) =
  charClass_wordy cs >>= \ds ->
  return $ c:ds

liftM :: Monad m => (a -> b) -> m a -> m b
liftM f m = m >>= \i ->
  return $ f i

liftM' :: Monad m => (a -> b) -> m a -> m b
liftM' = curry $ ((return .) *** (>>=)) >>> swap >>> app

charClass :: String -> Logger String
charClass (']':cs) = (']':) `liftM` globToRegex' cs
charClass (c  :cs) = (c  :) `liftM` charClass    cs

liftM2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
liftM2 f m1 m2 =
  m1 >>= \a ->
  m2 >>= \b ->
  return $ f a b
