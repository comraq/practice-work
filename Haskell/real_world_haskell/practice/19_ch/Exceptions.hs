-- In Haskell, Exceptions can only be caught in the IO Monad

{-
 - > :m +Control.Exception
 - > let x = 5 `div` 0
 - > let y = 5 `div` 1
 -
 - > print x
 - > *** Exception: divide by zero
 -
 - > print y
 - > 5
 -
 - > try (print x)
 - > Left divide by zero
 -
 - > try (print y)
 - > 5
 - > Right ()
 -
 - Notice that there are two lines of output from 'try (print y)'. The first
 - line was produced by 'print', which displayed the digit 5 on the
 - terminal. The second was produced by ghci, which is showing that 'print
 - y' return () and didn't throw an exception.
 -}

{-
 - Laziness and Exception Handling:
 -
 - > :m +Control.Exception
 - > let x = 5 `div` 0
 -
 - > result <- try (return x)
 - > Right *** Exception: divide by zero
 -
 - > let z = undefined
 - > try (print z)
 - > Left Prelude.undefined
 -
 - > result <- try (return z)
 - > Right *** Exception: Prelude.undefined
 -
 - Explanation:
 -
 - The reason why 'try (return undefined)' would result in 'Right undefined'
 - is because 'return' is lazy and does not force evaluation but only wraps
 - it up. Thus, 'try' sees an expression involving 'Right' instead of an
 - actual exception being thrown. Compared to 'try (print x)', 'print'
 - requires the value to be evaluated and thus the 'exception' was detected
 - and caught within 'try'.
 -
 - To resolve this issue, 'Control.Exception' module defines an 'evaluate'
 - function, which behaves just like 'return', but forces its argument to be
 - evaluated immediately.
 -
 - > :m +Control.Exception
 - > let x = 5 `div` 0
 -
 - > let z = undefined
 - > try (evaluate z)
 - > Left Prelude.undefined
 -
 - > result <- try (evaluate z)
 - > Left divide by zero
 -}

{-
 - Performing Actions Depending on the Exception:
 -
 - > :m Control.Exception
 - > let x = 5 `div` 0
 - > let y = 5 `div` 1
 -
 - > handle (const $ putStrLn "Error calculating result") (print x)
 - > Error calculating result
 -
 - > handle (const $ putStrLn "Error calculating result") (print y)
 - > 5
 -}
