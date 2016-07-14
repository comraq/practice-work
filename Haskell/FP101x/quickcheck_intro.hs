import Data.Char
import Test.QuickCheck
import qualified Countdown

deepCheck prop = quickCheckWith stdArgs { maxSuccess = 10000 } prop

getList :: IO String
getList =  fmap take5 getContents

take5 :: String -> String
take5 =  take 5 . filter (`elem` ['a'..'e'])

prop_revapp       :: [Int] -> [Int] -> Bool
prop_revapp xs ys =  reverse (xs++ys) == reverse xs ++ reverse ys

prop_split_length    :: [a] -> Bool
prop_split_length xs =  length xs >= (length $ Countdown.split xs)

join_tuple          :: ([a], [a]) -> [a]
join_tuple (xs, ys) =  xs ++ ys

prop_split_join    :: (Eq a) => [a] -> Bool
prop_split_join xs =  all (\x -> join_tuple x == xs) $ Countdown.split xs
