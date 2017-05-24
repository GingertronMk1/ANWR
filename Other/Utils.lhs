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

> quickSort :: Ord a => [a] -> [a]
> quickSort [] = []
> quickSort (x:xs) = smallerSorted ++ [x] ++ biggerSorted
>                    where smallerSorted = quickSort [a | a <- xs, a <= x]
>                          biggerSorted  = quickSort [a | a <- xs, a > x]

> randomSort :: Ord a => [a] -> [a]
> randomSort [] = []
> randomSort xs = smallerSorted ++ biggerSorted
>                 where smallerSorted = randomSort [a | a <- xs, a <= pivot]
>                       biggerSorted  = randomSort [a | a <- xs, a > pivot]
>                       pivot = xs !! (unsafePerformIO $ randomRIO (0, (length xs)-1))

Random Listing Stuff

> genList :: [Int]
> genList = concat $ reverse $ sequence $ replicate 2 [15,14..0]

> factorial :: Int -> Int
> factorial n | n < 2 = 1
>             | otherwise = n * factorial (n-1)

> factorialAcca :: Int -> Int
> factorialAcca n | n < 2 = 1
>                 | otherwise = factorialAcca' n 1

> factorialAcca' :: Int -> Int -> Int
> factorialAcca' 1 n = n
> factorialAcca' x y = factorialAcca' (x-1) (x*y)

Factors

> factorsOf :: Int -> [Int]
> factorsOf n = filter (\x -> mod n x == 0) [1..n] -- generates factors using a lambda function

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
>                   bubble' x = case x of [] -> []
>                                         (m:[]) -> [m]
>                                         (m:n:ms) -> if m > n then n:(bubble' (m:ms))
>                                                              else m:(bubble' (n:ms))

Implementing insertion-sort in Haskell
In-Place

> insertionInPlace :: Ord a => [a] -> [a]
> insertionInPlace []      = []
> insertionInPlace [x]     = [x]
> insertionInPlace (n:ns)  = myInsert n (insertionInPlace ns)

Using an accumulator

> insertionAcca :: Ord a => [a] -> [a]
> insertionAcca xs = insertionAcca' xs []
>                    where insertionAcca' as bs = case as of []     -> bs
>                                                            (c:cs) -> insertionAcca' cs (myInsert c bs)

> myInsert :: Ord a => a -> [a] -> [a]
> myInsert x [] = [x]
> myInsert x (y:ys) | x < y = x:y:ys
>                 | otherwise = y:(myInsert x ys)

> unsorted :: [Int]
> unsorted = [1,5,6,3,8,7,2,4,9]

Let's try merge-sort

> merge :: Ord a => [a] -> [a] -> [a]
> merge xs [] = xs
> merge [] ys = ys
> merge (x:xs) (y:ys) = if x < y then x:(merge xs (y:ys))
>                                else y:(merge (x:xs) ys)

> mergesort :: Ord a => [a] -> [a]
> mergesort [] = []
> mergesort [x] = [x]
> mergesort xs = merge (mergesort f) (mergesort s)
>                where f = take (div xslength 2) xs
>                      s = drop (div xslength 2) xs
>                      xslength = length xs
