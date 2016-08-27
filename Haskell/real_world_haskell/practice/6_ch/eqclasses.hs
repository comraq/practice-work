data Color = Red | Green | Blue

class BasicEq a where
  isEqual :: a -> a -> Bool
  isEqual a = not . isNotEqual a

  isNotEqual :: a -> a -> Bool
  isNotEqual a = not . isEqual a

instance BasicEq Bool where
  isEqual True  True  = True
  isEqual False False = True
  isEqual _     _     = False

instance BasicEq Color where
  isEqual Red   Red   = True
  isEqual Green Green = True
  isEqual Blue  Blue  = True
  isEqual _     _     = False

instance Show Color where
  show Red   = "Red"
  show Blue  = "Blue"
  show Green = "Green"

instance Read Color where
  -- readsPrec is the main function for parsing input
  readsPrec _ value = tryParse [("Red", Red), ("Green", Green), ("Blue", Blue)]
    {-
     - We pass tryParse a list of pairs. Each pair has a string and the
     - desired return value. tryParse will try to match the input to one of
     - these strings.
     -}
    where tryParse [] = []
          tryParse ((attempt, result):xs) =
            let v = dropWhile (==' ') value
            in  -- Compare the start of the string to be parsed to the text we
                -- are looking for
                if (take (length attempt) v) == attempt

                -- If we have a match, return the result and the remaining input
                then [(result, drop (length attempt) v)]

                -- If we don't have a match, try the next pair in the list of
                -- attempts
                else tryParse xs
