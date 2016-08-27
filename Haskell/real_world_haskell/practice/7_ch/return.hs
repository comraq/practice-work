import Data.Char (toUpper)

isGreen :: IO Bool
isGreen = do
  putStrLn "Is green your favourite colour?"
  inStr <- getLine
  return $ (toUpper . head $ inStr) == 'Y'

isGreen' :: IO Bool
isGreen' =
  putStrLn "Is green your favourite colour?" >>
  getLine >>=
  return . (==) 'Y' . toUpper . head

isYes :: String -> Bool
isYes = (==) 'Y' . toUpper . head

isGreen2 :: IO Bool
isGreen2 =
  putStrLn "Is green your favourite colour?" >>
  getLine >>=
  return . isYes

returnTest :: IO ()
returnTest = do
  one <- return 1
  let two = 2
  putStrLn $ show (one + two)

returnTest2 :: IO ()
returnTest2 = (+) <$> pure 1 <*> pure 2 >>= putStrLn . show
