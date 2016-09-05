{-# LANGUAGE FlexibleContexts #-}

{-
 - Transformer stack order is important!
 -
 - If the stack just contains 'ReaderT' and 'StateT', the order may not seem
 - significant. However, when 'StateT' is stacked on top of 'State', the
 - order can make a difference for example types:
 -   'StateT Int (State String)'
 -   'StateT String (State Int)'
 -
 - may carry the same information, but they cannot be used interchangeably.
 - The order determines when and where to use functions such as 'lift'.
 -}

import Control.Monad.Writer
import MaybeT

problem :: MonadWriter [String] m => m ()
problem = do
  tell ["this is where i fail"]
  fail "oops"

type A = WriterT [String] Maybe

type B = MaybeT (Writer [String])

a :: A ()
a = problem

b :: B ()
b = problem

{-
 - Running the above examples:
 - > runWriterT a
 - > Nothing
 -
 - > runWriter $ runMaybeT b
 - > (Nothing, ["this is where i fail"])
 -
 - This can be explained by the types of the execution functions:
 - > runWriterT :: WriterT w m a -> m (a, w)
 -
 - > runWriter . runMaybeT :: MaybeT (Writer w) a -> (Maybe a, w)
 -
 - 'WriterT' on 'Maybe' stack has 'Maybe' as the underlying monad, so the
 - result must be completely wrapped in the 'Maybe' type, thus we will only
 - be able to see the log if nothing actually went wrong! (if error, 'Maybe'
 - will just yield 'Nothing', without any additional information)
 -
 - 'MaybeT' on 'Writer' stack has 'Writer' as the underlying monad, so the
 - result is actually a 'Writer' monad, with only the value wrapped in
 - 'MaybeT'. Thus, the logs are unaffected regardless of the value of
 - 'Maybe'.
 -}

{-
 - Due to the development of 'Prelude' before standard support for monads,
 - there is a great divide between pure/monadic functions/values. For
 - modules that have been developed with greater abstractions, fully
 - embracing mondas, check out 'Data.Traversable' and 'Data.Foldable'.
 -}
