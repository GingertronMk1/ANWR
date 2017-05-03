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
