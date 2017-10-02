-- HASKELL UTILITIES:

import Data.List
import Data.Char
import System.Random
import Data.Maybe
import System.IO.Unsafe

-- Starting with some Fibonacci stuff:

fibs :: [Int]
fibs = 1 : 1 : zipWith (+) fibs (tail fibs)

fibsTo :: Int -> [Int]
fibsTo n = takeWhile (<n) fibs

nFibs :: Int -> [Int]
nFibs n = take n fibs

-- Random Listing Stuff

genList :: [Int]
genList = concat $ reverse $ sequence $ replicate 2 [15,14..0]

factorial :: Int -> Int
factorial n | n < 2 = 1
            | otherwise = n * factorial (n-1)

factorialAcca :: Int -> Int
factorialAcca n | n < 2 = 1
                | otherwise = factorialAcca' n 1

factorialAcca' :: Int -> Int -> Int
factorialAcca' 1 n = n
factorialAcca' x y = factorialAcca' (x-1) (x*y)

-- Factors

factorsOf :: Int -> [Int]
factorsOf n = [f | f <- [1..n], mod n f == 0]

isPrime :: Int -> Bool
isPrime n = factorsOf n == [1,n]

primesTo :: Int -> [Int]
primesTo n = [x | x <- [0..n], isPrime x]

-- Line by line: if `n` has no factors other than 1 and itself, it's prime and as such return a list of just it.
-- Otherwise, add to the list of factors (bar 1 and itself) the prime factors of n divided by each factor in turn

primeFactors :: Int -> [Int]
primeFactors n = case factors of [] -> [n]
                                 _  -> factors ++ primeFactors (div n (head factors))
                 where factors = take 1 [x | x <- factorsOf n, x /= 1 && x /= n]


intToList :: Int -> [Int]
intToList n = intToList' n []
intToList' :: Int -> [Int] -> [Int]
intToList' 0 xs = xs
intToList' n xs = intToList' (div n 10) $ (mod n 10):xs

sumDigits :: Int -> Int
sumDigits n = sum (intToList n)

sumsTo :: Int -> [Int]
sumsTo n = [x | x <- [0..10^12], sumDigits x == n]

-- Quick 'n' Dirty function to get the closest int to a number's square root (rounding up at all times)

intSqrt :: Int -> Int
intSqrt n = intSqrt' n 0

intSqrt' :: Int -> Int -> Int
intSqrt' n rt = if (rt*rt) < n then intSqrt' n (rt+1)
                               else rt

-- Sorting Algorithms
-- So many `Ord a` constraints

-- A utility function to determine whether or not a list is sorted

mySorted :: Ord a => [a] -> Bool
mySorted []       = True
mySorted [a]      = True
mySorted (a:b:as) = if a > b then False else mySorted (b:as)

-- Bubble-sort

bubble :: Ord a => [a] -> [a]
bubble ns = if mySorted lastRound then lastRound
                                  else bubble lastRound
            where lastRound = bubble' ns
                  bubble' x = case x of [] -> []
                                        (m:[]) -> [m]
                                        (m:n:ms) -> if m > n then n:(bubble' (m:ms))
                                                             else m:(bubble' (n:ms))


-- First, a test case
unsorted :: [Int]
unsorted = [1,10,2,9,3,8,4,7,5,6]

-- Insertion sort
insertion :: Ord a => [a] -> [a]
insertion []      = []
insertion [x]     = [x]
insertion (n:ns)  = myInsert n (insertion ns)

-- Selection sort
selection :: Ord a => [a] -> [a]
selection xs = selection' xs []
               where selection' as bs = case as of []     -> bs
                                                   (c:cs) -> selection' cs (myInsert c bs)

-- The actual insertion bit
myInsert :: Ord a => a -> [a] -> [a]
myInsert x [] = [x]
myInsert x (y:ys) | x < y     = x:y:ys
                  | otherwise = y:(myInsert x ys)


-- Speaking of whick...
mergesort :: Ord a => [a] -> [a]
mergesort [] = []
mergesort [x] = [x]
mergesort xs = merge (mergesort f) (mergesort s)
               where f = take (div xsLength 2) xs
                     s = drop (div xsLength 2) xs
                     xsLength = length xs

merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys) = if x < y then x:(merge xs (y:ys))
                               else y:(merge (x:xs) ys)

-- Quick sort
quickSort :: Ord a => [a] -> [a]
quickSort [] = []
quickSort (x:xs) = smallerSorted ++ [x] ++ biggerSorted
                   where smallerSorted = quickSort (filter (<= x) xs)
                         biggerSorted  = quickSort (filter (> x) xs)

-- Randomized-pivot quick sort, for those edge cases where the first element is always the smallest
randomSort :: Ord a => [a] -> [a]
randomSort []   = []
randomSort [x]  = [x]
randomSort xs   = smallerSorted ++ biggerSorted
                  where smallerSorted = randomSort [a | a <- xs, a <= pivot]
                        biggerSorted  = randomSort [a | a <- xs, a > pivot]
                        pivot = xs !! unsafePerformIO (randomRIO (0, (length xs)-1))

-- Image Compression Techniques

type Value = Int
type Length = Int

testRun :: [Value]
testRun = [1,1,2,2,2,3,3,4]


runlength :: [Value] -> [(Value, Length)]
runlength [] = []
runlength r@(x:xs) = runlength' r x 0

runlength' :: [Value] -> Value -> Length -> [(Value, Length)]
runlength' [] v l = [(v,l)]
runlength' (a:as) v l = if a == v then runlength' as v (l+1)
                                  else (v,l):(runlength' as a 1)

unrunlength :: [(Value, Length)] -> [Value]
unrunlength [] = []
unrunlength (t:ts) = (replicate l v) ++ (unrunlength ts)
                     where (v,l) = t

rltest :: [Int] -> [Int]
rltest = unrunlength . runlength

dpcm :: [Int] -> [Int]
dpcm ns = dpcm' ns 0

dpcm' :: [Int] -> Int -> [Int]
dpcm' [] _ = []
dpcm' (n:ns) v = (n-v):(dpcm' ns n)

undpcm :: [Int] -> [Int]
undpcm ns = undpcm' ns 0

undpcm' :: [Int] -> Int -> [Int]
undpcm' [] _ = []
undpcm' (n:ns) v = newN:(undpcm' ns newN)
                   where newN = n + v

dpcmtest = undpcm . dpcm

-------------------------------

--sieve :: Int -> [Int]
--sieve n = [1..intSqrt n]

--sieve n = removedups [a | ass <- sieve' n, a <- ass]
--sieve' n = map (\x -> takeWhile (<n) [x*i | i <- [x+1..n]]) [1..intSqrt n]

removedups xs = removedups' xs []
removedups' [] ys = ys
removedups' (a:as) ys = if elem a ys then removedups' as ys
                                     else removedups' as (a:ys)

sieve n = sieve' [1..n]
sieve' (x:xs) = if elem x xs then sieve' xs else xs


screenDims :: (Double, Double) -> Double -> (Double, Double)
screenDims (x, y) d = (a , a*r)
                      where r = x/y
                            a = sqrt (d*d / (1 + r*r))

{-
a^2 + (a*r)^2 = d ^ 2
a^2 * (1 + r ^ 2) = d ^ 2
a^2 = d^2 / (1 + r^2)
a = sqrt(d^2 / (1+r^2))

-}
