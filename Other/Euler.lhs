> prob1 :: Int
> prob1 = sum [x | x <- [1..999], rem x 3 == 0 || rem x 5 == 0]

> prob2 :: Int
> prob2 = sum [x | x <- fibsTo 4000000, even x]

> fibs :: [Int]
> fibs = 1 : 1 : zipWith (+) fibs (tail fibs)

> fibsTo :: Int -> [Int]
> fibsTo n = takeWhile (<n) fibs
