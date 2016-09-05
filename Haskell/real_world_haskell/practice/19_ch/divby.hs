{-
 - The following function would crash as long as the input list has the
 - number 0
 -}
divBy :: Integral a => a -> [a] -> [a]
divBy numerator = map (numerator `div`)

divByMaybe :: Integral a => a -> [a] -> Maybe [a]
divByMaybe _ []    = Just []
divByMaybe _ (0:_) = Nothing
divByMaybe numerator (denom:xs) =
  case divByMaybe numerator xs of
    Nothing      -> Nothing
    Just results -> Just ((numerator `div` denom) : results)

safeTail :: [a] -> Maybe [a]
safeTail []     = Nothing
safeTail (_:xs) = Just xs

divByMaybe' :: Integral a => a -> [a] -> Maybe [a]
divByMaybe' numerator denominators =
  mapM (numerator `safeDiv`) denominators
  where safeDiv _ 0 = Nothing
        safeDiv x y = return $ x `div` y

divByMaybe'' :: Integral a => a -> [a] -> [Maybe a]
divByMaybe'' numerator denominators = map worker denominators
  where worker 0 = Nothing
        worker x = Just (numerator `div` x)

divByMaybe4 :: Integral a => a -> [a] -> Maybe [a]
divByMaybe4 _ []    = return []
divByMaybe4 _ (0:_) = fail "division by zero in divByMaybe4"
divByMaybe4 numerator (denom:xs) = do
  next <- divByMaybe4 numerator xs
  return $ (numerator `div` denom) : next

divByMaybe5 :: Integral a => a -> [a] -> Maybe [a]
divByMaybe5 = divByGeneric

divByGeneric :: (Monad m, Integral a) => a -> [a] -> m [a]
divByGeneric _ []    = return []
divByGeneric _ (0:_) = fail "division by zero in divByGeneric"
divByGeneric numerator (denom:xs) = do
  next <- divByGeneric numerator xs
  return $ (numerator `div` denom): next

divByEither :: Integral a => a -> [a] -> Either String [a]
divByEither _ []    = Right []
divByEither _ (0:_) = Left "divByEither: division by 0"
divByEither numerator (denom:xs) =
  case divByEither numerator xs of
    Left x        -> Left x
    Right results -> Right ((numerator `div` denom) : results)

data DivByError a = DivBy0
                  | ForbiddenDenominator a
                    deriving (Eq, Read, Show)

divByError :: Integral a => a -> [a] -> Either (DivByError a) [a]
divByError _ []     = Right []
divByError _ (0:_)  = Left DivBy0
divByError _ (10:_) = Left (ForbiddenDenominator 10)
divByError _ (20:_) = Left (ForbiddenDenominator 20)
divByError numerator (denom:xs) =
  case divByError numerator xs of
    Left x        -> Left x
    Right results -> Right ((numerator `div` denom) : results)

{-
 - Note that both 'Maybe' and 'Either' examples of 'divBy*' suffer from lack
 - of laziness due to having to evaluate the entire division expression to
 - determine whether the result is an error or successful value.
 -}
