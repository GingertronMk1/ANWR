import Data.List
import Data.Char

insertionAcca :: Ord a => [a] -> [a]
insertionAcca xs = insertionAcca' xs []
                   where insertionAcca' as bs = case as of []     -> bs
                                                           (c:cs) -> insertionAcca' cs (myInsert c bs)

myInsert :: Ord a => a -> [a] -> [a]
myInsert x [] = [x]
myInsert x (y:ys) | x < y     = x:y:ys
                  | otherwise = y:(myInsert x ys)



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

nameVals :: [(Char, Int)]
nameVals = nameVals' ['A'..'Z'] 1
           where nameVals' [] _ = []
                 nameVals' (c:cs) n = (c,n):(nameVals' cs (n+1))

names = readFile "prob22names.txt"

valueOfName :: [Char] -> Int
valueOfName [] = 0
valueOfName (c:cs) = (valueOfName' c) + (valueOfName cs)

valueOfName' c = snd (head (filter (\x -> fst x == c) nameVals))

problem22' [] _ = 0
problem22' (n:ns) i = ((valueOfName n) * i) + (problem22' ns (i+1))





latticePaths = [(a,b) | a <- [0..20], b <- [0..20]]

summers n = [(a,n-a) | a <- [1..n-1]]

divisors :: Int -> [Int]
divisors n = [a | a <- [1..n-1], mod n a == 0]

abundant n = sum (divisors n) > n

abundantSummer n = or $ map abundantSummer' (summers n)
abundantSummer' (a,b) = abundant a && abundant b

 = sum $ filter (\x -> not (abundantSummer x)) [1..n]
