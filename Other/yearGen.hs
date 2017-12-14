currYear :: Int
currYear = 2017

yearsDone :: [Int]
yearsDone = [1979,1968,1977,1983,
             1970,2003,1971,2012,
             1974,2008,1976,1964,
             1978,1965,1975,1995,
             1973,1972,2006,1967,
             1994,1986,1991,1969,
             1988,1997,1966,1987,
             1982,2009,1990,2016,
             2007,1959,2001,2013]

allYears :: [Int]
allYears = [currYear-55..currYear]

main' :: [Int]
main' = [a | a <- allYears, not (elem a yearsDone)]

main :: IO ([Int])
main = return main'

mainLength = length main'
