doubleMe x = x + x

doubleUs x y = x*2 + y*2

lucky :: (Integral a) => a -> String
lucky 7 = "lucky number seven!"
lucky x = "Sorry, you're out of luck, pal!"

sayMe :: (Integral a) => a -> String
sayMe 1 = "One!"  
sayMe 2 = "Two!"  
sayMe 3 = "Three!"  
sayMe 4 = "Four!"  
sayMe 5 = "Five!"  
sayMe x = "Not between 1 and 5"

fact :: (Integral a) => a -> a
fact 0 = 1
fact n = n * fact (n - 1)

sum' :: (Num a) => [a] -> a  
sum' [] = 0  
sum' (x:xs) = x + sum' xs  

initials :: String -> String -> String  
initials (f:fs) (l:ls) = [f] ++ ". " ++ [l] ++ "." 

myMax :: (Ord a) => [a] -> a
myMax [] = error "myMax cannot be applied to an empty list!"
myMax [x] = x
myMax (x:xs)
  | x > myMax xs = x
  | otherwise = myMax xs

replicate' :: (Num a, Ord a) => a -> b -> [b]
replicate' x y
  | x < 1 = []
  | otherwise = y : replicate' (x-1) y

myTake :: (Num a, Ord a) => a -> [b] -> [b]
myTake n xs
  | n < 1 = []
myTake n (x:xs) = x : myTake (n - 1) xs

myZip :: [a] -> [b] -> [(a, b)]
myZip [] _ = []
myZip _ [] = []
myZip (x:xs) (y:ys) = (x, y): myZip xs ys

isIn :: (Eq a) => a -> [a] -> Bool
isIn _ [] = False
isIn y (x:xs)
  | x == y = True
  | otherwise = isIn y xs

myFlip :: (a -> b -> c) -> (b -> a -> c)
myFlip f x y = f y x

chain :: (Integral i, Ord i) => i -> [i]
chain i
  | i < 2 = [1]
  | odd i = i : chain (i * 3 + 1)
  | even i = i : chain (div i 2)

gt15 :: [[Int]]
gt15 = filter (\xs -> length xs > 15) (map chain [1..100])
