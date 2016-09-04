{-# LANGUAGE MultiParamTypeClasses
           , FunctionalDependencies
           , FlexibleInstances
  #-}

module SupplyClass
  ( MonadSupply(..)
  , S.Supply
  , S.runSupply      -- Note: we are re-exporting an imported function
  ) where

import qualified Supply as S

{-
 - Explanation of Pragmas used:
 -
 - > MultiParamTypeClasses allows typeclass definitions to be parametrized
 -   by multiple parameters
 -
 - > FunctionalDependencies tells the typechecker that for the class
 -   declaration: (MonadSupply s m | m -> s)
 -}
class Monad m => MonadSupply s m | m -> s where
  next :: m (Maybe s)

{-
 - Note that the original signature for 'next' defined for the monad
 - 'Supply s' is:
 -
 - > next :: Supply s (Maybe s)
 -
 - This shows the necessity of 'FunctionalDependencies' as knowing 'm', (in
 - which case is parametrized by the parameter 's'), should uniquely
 - identify 's'
 -
 - ie: When the typechecker sees the type 'S.Supply s', (which is the full
 -     type of the monad instance), it is able to correctly identify the
 -     parametrized type MonadSupply s
 -
 - Note: If the functional dependency clause is omitted, (ie: '| m -> s' is
 -       omitted), the source code would still compile but the error would
 -       not arrise until the first use case
 -}

instance MonadSupply s (S.Supply s) where
  next = S.next

{-
 - Concrete Explanation:
 - - Consider the type 'S.Supply Int' that is declared as an instance of
 -   'MonadSupply s'. Without the declaration of 'FunctionalDependencies',
 -   the compiler/type checker would not be able to safely deduce that the
 -   parameter 's' in 'MonadSupply s' should match the type 'Int' in
 -   'S.Supply Int'
 -}

showTwo_class :: (Show s, Monad m, MonadSupply s m) => m String
showTwo_class = do
  a <- next
  b <- next
  return $ show "a: " ++ show a ++ ", b: " ++ show b
