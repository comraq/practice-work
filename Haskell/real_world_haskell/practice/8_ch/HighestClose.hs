module HighestClose where

import qualified Data.ByteString.Lazy.Char8 as L

import Control.Arrow

closing :: L.ByteString -> Maybe Int
closing = readPrice . (!! 4) . L.split ','

readPrice :: L.ByteString -> Maybe Int
readPrice str = case L.readInt str of
  Nothing              -> Nothing
  Just (dollars, rest) -> case L.readInt $ L.tail rest of
    Nothing         -> Nothing
    Just (cents, _) -> Just $ dollars * 100 + cents

-- Cons Nothing to the list of prices because 'maximum' throws an exception
-- when list is empty (ie: no data in file read)
highestClose :: L.ByteString -> Maybe Int
highestClose = maximum . (Nothing:) . map closing . L.lines

hcFromPath :: FilePath -> IO ()
hcFromPath path = do
  contents <- L.readFile path
  print $ highestClose contents




closing' :: L.ByteString -> Maybe Int
closing' = readPrice' . (!! 4) . L.split ','

readPrice' :: L.ByteString -> Maybe Int
readPrice' = (>>= parsePrice') . L.readInt

parsePrice' :: (Int, L.ByteString) -> Maybe Int
parsePrice' =
  let calcPrice  = (+) . (* 100)
      maybeCents = fmap fst . L.readInt . L.tail
  in uncurry fmap . (calcPrice *** maybeCents)

highestClose' :: L.ByteString -> Maybe Int
highestClose' = maximum . (Nothing:) . map closing' . L.lines

hcFromPath' :: FilePath -> IO ()
hcFromPath' = (>>= print . highestClose') . L.readFile
