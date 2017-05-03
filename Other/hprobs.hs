import Data.List

myLast :: [a] -> a
myLast (a:[]) = a
myLast (a:as) = myLast as

myButLast :: [a] -> a
myButLast (a:b:[]) = a
myButLast (a:b:cs) = myButLast (b:cs)

elementAt :: [a] -> Int -> a
elementAt (a:as) n = if n == 1 then a
                               else elementAt as (n-1)

myLength :: [a] -> Int
myLength as = myLength' as 0

myLength' :: [a] -> Int -> Int
myLength' (a:[]) n = n+1
myLength' (a:as) n = myLength' as (n+1)

myReverse :: [a] -> [a]
myReverse as = myReverse' as []

myReverse' :: [a] -> [a] -> [a]
myReverse' (a:[]) bs = a:bs
myReverse' (a:as) bs = myReverse' as (a:bs)

isAPalindrome :: Eq a => [a] -> Bool
isAPalindrome as | myReverse as == as  = True
                 | otherwise           = False

--compress :: Eq a => [a] -> [a]
-- compress []     = []
-- compress (x:xs) = x : (compress $ dropWhile (== x) xs)

compress xs = map head $ group xs

