main' :: IO ()
main' = do
  contents <- getContents
  print (sumFile contents)
  where sumFile = sum . map read . words

main :: IO ()
main = getContents >>= print . sumFile
  where sumFile = sum . map read . words
