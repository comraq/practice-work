{-
 - Jargon:
 - > "Monadic" simply means "pertaining to monads". A monadic type is an
 -   instance of the 'Monad' typeclass; a monadic value has a monadic type
 -
 - > When we say that a type "is a monad", this is really a shorthand way of
 -   saying that it's an instance of the 'Monad' typeclass. Being an instance
 -   of 'Monad' gives us the necessary monadic triple of type constructor,
 -   injection function (return), and chaining function (bind)
 -
 - > In the same way, a reference to "the Foo monad" implies that we are
 -   talking about the type named 'Foo', and that it's an instance of 'Monad'
 -
 - > An "action" is another name for a monadic value. This use of the word
 -   probably originated with the introduction of monads for I/O, where a
 -   monadic value like 'print "foo"' can have an observable side effect. A
 -   function with a monadic return type might also be referred to as an
 -   action, though this is a little less common.
 -}

{-
 - "Controlled Escape"
 -
 - The 'Monad' typeclass doesn't provide any means for values to escape
 - their monadic shackles. We can inject a value into a monad using
 - 'return'. We can extract a value from a monad using (>>=) but the
 - function on the right, which can see an unwrapped value, has to wrap its
 - own result back up again.
 -
 - Most monads have one or more 'run-' like functions, which extracts the
 - contained value within a monad out. However, the notable exception is of
 - course 'IO', which we usually only escape from by exiting a program.
 -
 - A monad execution function runs the code inside the monad and unwraps its
 - result. Such functions are usually the only means provided for a value to
 - escape from its monadic wrapper. The author of a monad thus has complete
 - control over both:
 -   - what happens inside the monadic context
 -   - what values may escape from that context
 -
 - Some monads have several execution functions. In a case of a logger, we
 - can imagine a few alternatives to 'runLogger':
 -   - only return the log messages
 -   - only return the result value, dropping the log messages
 -   - and etc...
 - log messages
 -
 -}

{-
 - Laws:
 -   fmap id        === id
 -   fmap (f . g)   === fmap f . fmap g
 -
 -   return x >>= f          === f x
 -   m >>= return            === m
 -   m >>= (\x -> f x >>= g) === (m >>= f) >>= g
 -
 -   m >>= f        === join $ fmap f xs
 -   join x         === x >>= id
 -}