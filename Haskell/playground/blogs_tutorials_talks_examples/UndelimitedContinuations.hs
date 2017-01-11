{-# LANGUAGE RankNTypes #-}

module UndelimitedContinutations where

-- @link - http://okmij.org/ftp/continuations/undelimited.html

import Control.Monad (void)
import Control.Monad.ST
import Data.STRef
import Data.Void

newtype CPS1 a = CPS1 { unCPS1 :: forall w. (a -> w) -> w }
type K1 a = forall w. a -> w

{-
 - polymorhpic w in CPS1 and K1 cannot be matched by the type checker
 -
 - callCC1 :: (K1 a -> CPS1 a) -> CPS1 a
 - callCC1 f = CPS1 $ \k -> unCPS1 (f k) k
 -}

-- CPS2 type checks but cannot be called since a function from a to bottom
-- cannot be constructed (initial continuation cannot be constructed)
newtype CPS2 a = CPS2 { unCPS2 :: (a -> Void) -> Void }
type K2 a = a -> Void


-- Obtain result through a monad m
newtype CPS3 m a = CPS3 { unCPS3 :: (a -> m Void) -> m Void }
type K3 m a = a -> m Void

-- Instantiates the monad with (ST s)
runCPS3 :: (forall m. CPS3 m a) -> a
runCPS3 m = runST (do
  res <- newSTRef (error "Continuation has escaped!")
  void $ unCPS3 m (\v -> writeSTRef res v >> return undefined)
  readSTRef res)

newtype Cont w a = Cont { unCont :: (a -> w) -> w }
type K w a = a -> w

callCC :: (K w a -> Cont w a) -> Cont w a
callCC f = Cont $ \k -> unCont (f k) k

throw :: K w a -> a -> Cont w b
throw k x = Cont $ \_ -> k x

{-
 - Note: The signature
 -     runCont :: Cont w a -> a
 -   will not work because we pass the id as the initial continuation.
 -   This means that the input Cont variable m must have type
 -     runCont :: Cont a a -> a
 -
 -   However, this limits us to only using runCont where continuations
 -   must have the same input and output types.
 -}
runCont :: (forall w. Cont w a) -> a
runCont m = unCont m id

newtype C a = C { unC :: forall w. (a -> w) -> w }
type K' a = forall w. a -> w

runC :: C a -> a
runC m = unC m id

throwC :: K' a -> a -> C b
throwC k x = C $ \_ -> k x

-- Cannot construct throwC as there is no way to call it with a polymorphic
-- argument of type = K' a with non-bottom type (callers of throwC cannot
-- know the type of w, thus C can only be ran for effects, not its return
-- value)

ex :: C b
ex = throwC (const undefined) "any value"
