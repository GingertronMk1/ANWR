import Data.List
import System.Directory
import System.IO.Unsafe
import Data.Char

data Tree a = Node a [Tree a] deriving Show
type Actor = String

treeLim :: Int
treeLim = 4

showsPath :: String
showsPath = "/Users/Jack/Git/history-project/_shows/"

escapePath :: String
escapePath = showsPath ++ "17_18/escape_for_dummies_lakeside.md"

me :: Actor
me = "Jack Ellis"

ian :: Actor
ian = "Ian Sheard"

omid :: Actor
omid = "Omid Faramarzi"

sosborne :: Actor
sosborne = "Sam Osborne"

jamie :: Actor
jamie = "Jamie Drew"

testTree :: Tree Actor
testTree = Node ian [Node omid [], Node sosborne [Node me []]]

limitTree :: Int -> Tree Actor -> Tree Actor
limitTree 0 (Node x _) = Node x []
limitTree n (Node x ts) = Node x [limitTree (n-1) t | t <- ts]

treeGen :: Actor -> Tree Actor
treeGen actorName = Node actorName [treeGen a | a <- allFellow actorName]

limitedTree actorName = limitTree treeLim $ treeGen actorName

treeCheck target (Node a []) = if a == target then Node 1 [] else Node 1000 []
treeCheck target (Node a as) = if a == target then Node 1 [] else Node (1 + (minimum ns)) []
                                              where ns = map (nodeVal . (treeCheck target)) as

sixDegs target base = if calcVal > treeLim then 1000
                                           else calcVal
                      where calcVal = nodeVal (treeCheck target (limitedTree base)) - 1



nodeVal :: Tree Int -> Int
nodeVal (Node n _) = n

getVal :: Tree Int -> Int
getVal (Node n []) = n
getVal (Node n ns) = maximum (map getVal ns)

names s = do filecontent <- readFile s
             let peopleNames = map stripEndSpace $ map (drop 2) $ map (dropWhile (/= ':')) $ filterPeople $ lines filecontent
                               where filterPeople ss = filter peopleFilter ss
                                     peopleFilter s = isInfixOf " name:" s
             return $ peopleNames

actorNames s = do filecontent <- readFile s
                  let actorNames = map stripEndSpace $ map (drop 2) $ map (dropWhile (/= ':')) $ filterPeople $ actorFilter $ lines filecontent
                                   where filterPeople ss = filter peopleFilter ss
                                         peopleFilter s = isInfixOf " name:" s
                  return actorNames

actorFilter ss = takeWhile (\s -> (not (isInfixOf "crew:" s))) ss

stripEndSpace :: [Char] -> [Char]
stripEndSpace (c:' ':[]) = [c]
stripEndSpace (c:[]) = [c]
stripEndSpace (c:cs) = c:(stripEndSpace cs)

getAllShows :: IO [FilePath]
getAllShows = do showsDirs <- getDirectoryContents showsPath
                 return showsDirs

allShowsEver :: [String]
allShowsEver = flatten $ allShowsBuilder getAllShows

showsPlusPathFun :: [String] -> [String]
showsPlusPathFun s = map (showsPath++) s

filterDots :: [String] -> [String]
filterDots l = filter dotsFilter l
               where dotsFilter f = head f /= '.'

getAllContents :: [FilePath] -> [IO [FilePath]]
getAllContents s = map getDirectoryContents s

unsafeMap :: [IO a] -> [a]
unsafeMap x = map unsafePerformIO x

filterDotsMap :: [[[Char]]] -> [[[Char]]]
filterDotsMap ls = map filterDots ls

allShowsBuilder :: IO [[Char]] -> [[[Char]]]
allShowsBuilder = filterDotsMap . (map dirPrepend) . (map allContentswDir) . filterDots . showsPlusPathFun . unsafePerformIO

allContentswDir :: [Char] -> [[Char]]
allContentswDir s = s:(unsafePerformIO $ getDirectoryContents s)

dirPrepend :: [[Char]] -> [[Char]]
dirPrepend ss = map prepender $ filterDots $ tail ss
                where prepender s = head ss ++ "/" ++ s

namess = filter (/= []) $ map unsafePerformIO $ map names $ drop 102 allShowsEver
actorss = filter (/= []) $ map unsafePerformIO $ map actorNames $ drop 102 allShowsEver

rmdups :: Ord a => [a] -> [a]
rmdups = map head . group . sort

allFellow :: String -> [String]
allFellow n = filter (/= n) $ rmdups [a | as <- allFellow' n, a <- as]
              where allFellow' n = filter (elem n) actorss

flatten :: [[a]] -> [a]
flatten ass = [a | as <- ass, a <- as]

everyone :: [String]
everyone = rmdups $ flatten $ namess

allActors :: [String]
allActors = rmdups $ flatten $ actorss

getName :: String -> IO String
getName prompt = do putStr prompt
                    name <- getLine
                    return name

main :: IO()
main = do a1 <- getLine
          a2 <- getLine
          let sixDegsChar = intToDigit $ sixDegs a1 a2
          putStrLn $ sixDegsChar:[]
