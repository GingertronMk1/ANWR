import Data.List
import System.Directory
import System.IO.Unsafe
import Data.Char

data ActorTree a = Node a [ActorTree a] deriving Show

treeGen :: String -> String -> [String] -> ActorTree String
treeGen actorTarget actorBase actorsDone = if actorTarget == actorBase || elem actorBase actorsDone then Node actorBase []
                                                                                                    else Node actorBase [treeGen actorTarget b (actorBase:actorsDone) | b <- allFellow actorBase]

limitTree :: Int -> ActorTree String -> ActorTree String
limitTree 0 (Node x _) = Node x []
limitTree n (Node x ts) = Node x [limitTree (n-1) t | t <- ts]

sixDegrees'' actorTarget actorBase = limitTree 4 $ treeGen actorTarget actorBase []

--sixDegrees :: String -> Int -> ActorTree String -> Int
sixDegrees' actorTarget n (Node p []) = 99
sixDegrees' actorTarget n (Node p ps) = if p == actorTarget then n
                                                            else minimum $ map (sixDegrees' actorTarget (n+1)) ps

sixDegrees actorTarget actorBase = sixDegrees' actorTarget 1 (sixDegrees'' actorTarget actorBase)

sixDegsTest = sixDegrees "Jamie Drew" "Ian Sheard"

showsPath :: String
showsPath = "/Users/Jack/Git/history-project/_shows/"

--names :: String -> IO [String]
names s = do filecontent <- readFile s
             let peopleNames = map (drop 2) $ map (dropWhile (/= ':')) $ filterPeople $ lines filecontent
                               where filterPeople ss = filter peopleFilter ss
                                     peopleFilter s = (take 5 (dropWhile (/= 'n') s)) == "name:"
                               --where filterPeople ss = filter peopleFilter ss
                                     --peopleFilter s = (take 10 s) == "    name: "
             return peopleNames



getAllShows = do showsDirs <- getDirectoryContents showsPath
                 return showsDirs

allShowsEver  = flatten $ allShowsBuilder getAllShows

showsPlusPathFun :: [[Char]] -> [[Char]]
showsPlusPathFun s = map (showsPath++) s

filterDots :: [[Char]] -> [[Char]]
filterDots l = filter dotsFilter l
               where dotsFilter f = (f /= "." && f /= "..")

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

rmdups :: Ord a => [a] -> [a]
rmdups = map head . group . sort

allFellow n = filter (/= n) $ rmdups [a | as <- allFellow' n, a <- as]
              where allFellow' n = filter (elem n) namess

flatten ass = [a | as <- ass, a <- as]
