import Data.List
import System.Directory
import System.IO.Unsafe
import Data.Char

data ActorTree a = Node a [ActorTree a] deriving Show

showsPath :: String
showsPath = "/Users/Jack/Git/history-project/_shows/"

me :: String
me = "Jack Ellis"

limitTree :: Int -> ActorTree String -> ActorTree String
limitTree 0 (Node x _) = Node x []
limitTree n (Node x ts) = Node x [limitTree (n-1) t | t <- ts]

treeGen' actorName actorsDone = if elem actorName actorsDone then Node actorName []
                                                             else Node actorName [treeGen' a (actorName:actorsDone) | a <- allFellow actorName, notElem a actorsDone]

treeGen actorName = treeGen' actorName []

limitedTree actorName = limitTree 5 $ treeGen actorName


names s = do filecontent <- readFile s
             let peopleNames = map (drop 2) $ map (dropWhile (/= ':')) $ filterPeople $ lines filecontent
                               where filterPeople ss = filter peopleFilter ss
                                     peopleFilter s = isInfixOf " name:" s
             return peopleNames

getAllShows :: IO [FilePath]
getAllShows = do showsDirs <- getDirectoryContents showsPath
                 return showsDirs

allShowsEver :: [String]
allShowsEver = flatten $ allShowsBuilder getAllShows

showsPlusPathFun :: [[Char]] -> [[Char]]
showsPlusPathFun s = map (showsPath++) s

filterDots :: [[Char]] -> [[Char]]
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

rmdups :: Ord a => [a] -> [a]
rmdups = map head . group . sort

allFellow :: String -> [String]
allFellow n = filter (/= n) $ rmdups [a | as <- allFellow' n, a <- as]
              where allFellow' n = filter (elem n) namess

flatten :: [[a]] -> [a]
flatten ass = [a | as <- ass, a <- as]

everyone :: [String]
everyone = rmdups $ flatten $ namess
