HASKELL UTILITIES:

> import Data.List
> import Data.Char
> import System.Random
> import Data.Maybe
> import System.IO.Unsafe

Starting with some Fibonacci stuff:

> fibs :: [Int]
> fibs = 1 : 1 : zipWith (+) fibs (tail fibs)

> fibsTo :: Int -> [Int]
> fibsTo n = takeWhile (<n) fibs

> nFibs :: Int -> [Int]
> nFibs n = take n fibs

Let's sort something:

> quickSort :: (Ord a) => [a] -> [a]
> quickSort [] = []
> quickSort (x:xs) = let smallerSorted = quickSort [a | a <- xs, a <= x]
>                        biggerSorted = x:quickSort [a | a <- xs, a > x]
>                    in smallerSorted ++ biggerSorted

> randomSort :: (Ord a) => [a] -> [a]
> randomSort [] = []
> randomSort xs = let smallerSorted = quickSort [a | a <- xs, a <= pivot]
>                     biggerSorted = quickSort [a | a <- xs, a > pivot]
>                 in smallerSorted ++ [pivot] ++ biggerSorted
>                 where pivotIndex = unsafePerformIO $ randomRIO (0, (length xs)-1)
>                       pivot = xs !! pivotIndex

Random Listing Stuff

> genList :: [Int]
> genList = concat $ reverse $ sequence $ replicate 2 [15,14..0]

> factorial :: Int -> Int
> factorial n | n < 2 = 1
>             | otherwise = n * factorial (n-1)

Factors

> isFactor :: Int -> Int -> Bool
> isFactor a b = mod b a == 0

> factorsOf :: Int -> [Int]
> factorsOf n = filter (`isFactor` n) [1..n] -- generates factors using a lambda function

> isPrime :: Int -> Bool
> isPrime n = factorsOf n == [1,n]

> primesTo :: Int -> [Int]
> primesTo n = [x | x <- [0..n], isPrime x]

Line by line: if `n` has no factors other than 1 and itself, it's prime and as such return a list of just it.
Otherwise, add to the list of factors (bar 1 and itself) the prime factors of n divided by each factor in turn

> primeFactors :: Int -> [Int]
> primeFactors n = case factors of [] -> [n]
>                                  _  -> factors ++ primeFactors (div n (head factors))
>                  where factors = take 1 [x | x <- factorsOf n, x /= 1 && x /= n]


> intToList :: Int -> [Int]
> intToList n = intToList' n []
> intToList' :: Int -> [Int] -> [Int]
> intToList' 0 xs = xs
> intToList' n xs = intToList' (div n 10) $ (mod n 10):xs

> sumDigits :: Int -> Int
> sumDigits n = sum (intToList n)

> sumsTo :: Int -> [Int]
> sumsTo n = [x | x <- [0..10^12], sumDigits x == n]


I saw this on a YouTube video:

> swedish :: [Char] -> [Char]
> swedish = intersperse 'f'

> greeting :: String
> greeting = "Hello!"

> triangleNumbers :: [Int]
> triangleNumbers = scanl1 (+) [1..]



> mod13 :: Int -> Int
> mod13 n = 1 + mod n 13




> testReplace :: [Int] -> Int -> ([Int], Int)
> testReplace [] x = ([],x)
> testReplace (n:ns) x = if n == 0 then (n', ns')
>                                       where n' = x
>                                             ns' = 

A utility function to determine whether or not a list is sorted

> mySorted :: Ord a => [a] -> Bool
> mySorted []     = True
> mySorted (a:[]) = True
> mySorted (a:b:as) = if a > b then False else mySorted (b:as)

Implementing bubble-sort in Haskell

> bubble :: Ord a => [a] -> [a]
> bubble ns = if mySorted lastRound then lastRound
>                                   else bubble lastRound
>             where lastRound = bubble' ns

> bubble' :: Ord a => [a] -> [a]
> bubble' []        = []
> bubble' (x:[])    = [x]
> bubble' (x:y:xs)  = if x > y then y:(bubble' (x:xs))
>                     else x:(bubble' (y:xs))

Implementing insertion-sort in Haskell

> insertion :: Ord a => [a] -> [a]
> insertion ns = insertion' ns []

Needs a couple of helper functions; the first is insertion', which just sees if
it's at the end of the list, and calls either itself recursively, or ends the function

> insertion' :: Ord a => [a] -> [a] -> [a]
> insertion' (x:[]) ys = insertion'' x ys
> insertion' (x:xs) ys = insertion' xs (insertion'' x ys)

The second is insertion'', which is the actual 'insert' part of the sort
It looks through the accumulator list and inserts the new value in place

> insertion'' :: Ord a => a -> [a] -> [a]
> insertion'' x [] = [x]
> insertion'' x (y:ys) = if x > y then y:(insertion'' x ys) else x:y:ys

> unsorted :: [Int]
> unsorted = [1,5,6,3,8,7,2,4,9]
