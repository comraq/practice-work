module FoldTraverse where

import Control.Applicative

ls :: [Int]
ls = [1, 2, 3]

t1 = traverse (\x -> [x, x + 1]) ls
-- [[1,2,3], [1,2,4], [1,3,3], [1,3,4], [2,2,3], [2,2,4], [2,3,3], [2,3,4]]

t2 = traverse (const [True, False]) ls

transpose :: [[a]] -> [[a]]
transpose = getZipList . traverse ZipList

-- traverse' f (a:as) = (:) <$> f a <*> traverse' f as
-- traverse' _ [] = pure []
