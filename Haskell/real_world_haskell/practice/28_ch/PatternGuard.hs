{-# LANGUAGE PatternGuards #-}

{-
 - Pattern guardds allows more concise guard expressions. A pattern guard
 - has three components:
 -   - a pattern
 -   - a '<-' symbol
 -   - an expression
 -
 - The expression is evaluated and matched against the pattern. If it
 - matches, any variables present in the pattern are bound. We can mix
 - pattern guards and normal 'Bool' guard expressions in a single guard by
 - separating them with commas.
 -}

{-
 - A sample pattern guard.
 -
 - In this example, we return a value from an association list 'xs'
 - if its associated key 'x' is present, provided that the value is greater
 - than 3.
 -}
testme x xs | Just y <- lookup x xs, y > 3 = 3
            | otherwise                    = 0

-- An equivalent to the above without pattern guards
testme_noguards x xs = case lookup x xs of
  Just y | y > 3 -> y
  _              -> 0

{-
 - The benefit is that 'Pattern Guards' let us "collapse" a collection of
 - guards and 'case' expressions into a single guard, allowing us to write
 - more succinct and description guards.
 -}
