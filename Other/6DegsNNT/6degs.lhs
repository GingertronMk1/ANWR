----------------------------------------------------------------------------------------------------------------------------------------------------------------
We're first going to import some things:
- Data.List for isInfixOf, sort, and group
- System.Directory so we can muck about with files and dirs
- System.IO.Unsafe so we can do things we really oughtn't
- And finally Data.Char for intToDigit
----------------------------------------------------------------------------------------------------------------------------------------------------------------

> import Data.List
> import System.Directory
> import System.IO.Unsafe
> import Data.Char
> import qualified Data.Text as T
> import qualified Data.Text.IO as TIO

--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
Now defining some data types:
- A Tree for a recursive data type
- And Actor and ShowName for type clarity
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

> data Tree a = Node a [Tree a] deriving Show
> type Actor = String
> type ShowName = String
> type Detail = (FilePath, ShowName, [Actor])
> type Details = [Detail]

--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
A few test variables now:
- treeLim so we can change the depth of a tree here rather than having to muck about in functions way down
- showsPath is where the shows are in my copy of the history-project repo
- escapePath is where the Lakeside performance of Escape For Dummies is, as a test show
- And myself and some people as test cases for the actual degree-finder
- Finally, a test tree for demonstrating printing things
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

> treeLim :: Int
> treeLim = 4
> showsPath :: String
> showsPath = "/Users/Jack/Git/history-project/_shows/"
> escapePath :: String
> escapePath = showsPath ++ "17_18/escape_for_dummies_lakeside.md"
> me :: Actor
> me = "Jack Ellis"
> ian :: Actor
> ian = "Ian Sheard"
> omid :: Actor
> omid = "Omid Faramarzi"
> sosborne :: Actor
> sosborne = "Sam Osborne"
> jamie :: Actor
> jamie = "Jamie Drew"
> rose :: Actor
> rose = "Rose Edgeworth"

--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
First we need to build a list of all of the shows that have records on the history site, step by step.
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

First within this, build the first level of directories: the years

> baseDirs :: IO [FilePath]
> baseDirs = do baseDir <- getDirectoryContents showsPath
>               return $ map (\s -> showsPath ++ s ++ "/") $ drop 2 baseDir

Then we take that, and extract the contents, prepending the containing directory

> dirBuilder = do baseDir <- baseDirs
>                 return $ map getDirContentsPrep baseDir
>                 where getDirContentsPrep s = do contents <- getDirectoryContents s
>                                                 return $ map (s++) (drop 2 contents)

Finally, we use `sequence` to pull all the IO out to the front so we can work on the list as a pure type within other `do` blocks

> allShowsDo :: IO [FilePath]
> allShowsDo = do allDirs' <- dirBuilder
>                 allDirs <- sequence allDirs'
>                 return $ filter showFilter $ flatten allDirs
>                 where showFilter s = isInfixOf ".md" s && not (isInfixOf "freshers_fringe" s)

--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
Now that we've got a list of all of the shows, we need to extract from it a list of all actors.
First we're going to extract just the actors from a single show, as such:
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

> showDetails :: FilePath -> IO Detail
> showDetails s = do fileContents <- strictReadFile s
>                    let fileLines = lines fileContents
>                    return (s, getTitle fileLines, peopleNames fileLines)

> strictReadFile :: FilePath -> IO String
> strictReadFile = fmap T.unpack . TIO.readFile

> actorFilter :: [String] -> [String]
> actorFilter ss = takeWhile (\s -> not (isInfixOf "crew:" s)) ss

> dropUntilCast :: [String] -> [String]
> dropUntilCast ss = dropWhile (\s -> not (isInfixOf "cast:" s)) ss

> filterPeople :: [String] -> [String]
> filterPeople ss = filter (isInfixOf " name:") $ dropUntilCast $ actorFilter ss

> stripEndSpace :: String -> String
> stripEndSpace (c:' ':[]) = [c]
> stripEndSpace (c:[]) = [c]
> stripEndSpace (c:cs) = c:(stripEndSpace cs)

> peopleNames :: [String] -> [Actor]
> peopleNames ss = map (stripEndSpace . drop 2 . dropWhile (/= ':')) $ filterPeople ss

> getTitle :: [String] -> String
> getTitle = stripEndSpace . drop 2 . dropWhile (/= ':') . head . filter (isInfixOf "title:")

> allShowDetails :: IO Details
> allShowDetails = do showNames <- allShowsDo
>                     showDetailsIO <- sequence $ map showDetails showNames
>                     return showDetailsIO

Helpers to extract various bits of info from the show details (Haskell gets shaky around 3-tuples)

> getFirst :: (a,b,c) -> a
> getFirst (a,b,c) = a
> getSecond :: (a,b,c) -> b
> getSecond (a,b,c) = b
> getThird :: (a,b,c) -> c
> getThird (a,b,c) = c

--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
So, we can generate a list of all actors ever at the NNT (that the history site knows of).
Now, we need some way of finding all actors one particular actor has acted with.
Act doesn't seem like part of a word anymore.
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

Up first, a helper function to remove duplicates, making use of some fun bits from Data.List

> rmdups :: Ord a => [a] -> [a]
> rmdups = map head . group . sort

> flatten :: [[a]] -> [a]
> flatten ass = [a | as <- ass, a <- as]

Now the meat.
allFellow takes an Actor name, and the list of all Actors (`actorss`), filters `actorss` to just the lists with the target in them, and flattens out that list, removing duplicates

> allFellows :: Details -> Actor -> IO [Actor]
> allFellows dt n = do return $ rmdups [a | as <- (filter (elem n) (thirds dt)), a <- as, a /= n]
>                      where thirds = map getThird

--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
Generating trees now: given an actor's name, we generate a list of all of their fellow actors and use that to generate a tree of their connection to the theatre population
This tree is obviously infinite, having no final case, so we need to limit it
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

> limTreeIO :: Int -> Details -> Actor -> IO (Tree Actor)
> limTreeIO 0 dt actorName = do return $ Node actorName []
> limTreeIO n dt actorName = do fellows <- allFellows dt actorName
>                               fellowTrees <- sequence (map (limTreeIO (n-1) dt) fellows)
>                               return $ Node actorName fellowTrees

> treeGenIO :: Details -> Actor -> IO (Tree Actor)
> treeGenIO dt actorName = limTreeIO treeLim dt actorName

> limitTree :: Int -> Tree a -> Tree a
> limitTree 0 (Node x _) = Node x []
> limitTree n (Node x ts) = Node x [limitTree (n-1) t | t <- ts]

treeCheck takes the Detail 

> treeCheck :: Details -> Actor -> Tree Actor -> Tree ([Actor], [ShowName], Int)
> treeCheck detailList target (Node a []) = if a == target then Node ([a], [], 0) [] else Node ([a], [], 1000) []
> treeCheck detailList target (Node a as) = if a == target then Node ([a], [], 0) [] else Node (a:b, link:oldLink, 1+c) []
>                                            where (b, oldLink, c) = minTuple $ map (nodeVal . (treeCheck detailList target)) as
>                                                  link = getTitleFromPath detailList (findShowsWActors a (head b))
>                                                  getTitleFromPath dt fp = getSecond . head $ filter (\t -> getFirst t == fp) dt
>                                                  findShowsWActors a1 a2 = getFirst . head $ filter (\s -> elem a1 (getThird s) && elem a2 (getThird s)) detailList

nodeVal takes the Tree Int generated by treeCheck and takes just the Int from it

> nodeVal :: Tree (a,b,c) -> (a,b,c)
> nodeVal (Node (a,b,c) _) = (a,b,c)

> minTuple :: Ord a => [(a1,a2,a)] -> (a1,a2,a)
> minTuple (x:xs) = minTail x xs
>                   where minTail x [] = x
>                         minTail (p,q,r) ((d,e,f):ms)
>                           | r > f = minTail (d,e,f) ms
>                           | otherwise = minTail (p,q,r) ms

----------------------------------------------------------------------------------------------------------------------------------------------------------------
For use when the program is compiled using GHC; main takes two names entered and returns either the degree or an 'x' if the degree is too great
----------------------------------------------------------------------------------------------------------------------------------------------------------------

> main :: IO()
> main = do a1 <- getLine
>           a2 <- getLine
>           allDetails <- allShowDetails
>           a1Tree <- treeGenIO allDetails a1
>           pp (treeCheck allDetails a2 a1Tree)
>           where flatCommas = flatten . intersperse ", "
>                 pp (Node (as, ss, i) _) = if i > treeLim then putStrLn "These people are not linked"
>                                                          else putStrLn $ head as ++ " and " ++ last as ++
>                                                                          " are linked by these people: " ++ (flatCommas as) ++
>                                                                          ", via these shows: " ++ (flatCommas ss) ++
>                                                                          " with " ++ [intToDigit i] ++ " degrees of separation."

Prettily printing a tree, for no real reason other than it looks cool

> ppTree' (Node a []) depth = a
> ppTree' (Node a as) depth = a 
>                             ++ "\n" 
>                             ++ replicate depth ' ' 
>                             ++ flatten (map (\x -> (ppTree' x (depth+2)) ++ '\n':( replicate depth ' ')) as)

> ppTree (Node a as) = putStrLn $ ppTree' (Node a as) 2

This flattens a tree into a single list; useful for measuring how changing the treeLim value affects the size of the tree

> flatTree (Node a []) = [a]
> flatTree (Node a as) = a :(flatten $ map flatTree as)

--------------------------------------------------------------------------------------------------------------------------------------------------------------
TODO
--------------------------------------------------------------------------------------------------------------------------------------------------------------

Refactor main so the files are only accessed once

--------------------------------------------------------------------------------------------------------------------------------------------------------------
TESTING
----------------------------------------------------------------------------------------------------------------------------------------------------------------


> treeLenComp = do allDetails <- allShowDetails
>                  trees <- sequence $ map (\x -> limTreeIO x allDetails me) [treeLim..treeLim]
>                  return $ map (length . flatTree) trees

> my1Tree = do allDetails <- allShowDetails
>              tree <- limTreeIO 4 allDetails me
>              ppTree tree

This is for debugging, it tells me how many shows we've got. If it doesn't match the value on `history.newtheatre.org.uk`, something's up

> allShowsLength = do allShow <- allShowsDo
>                     return $ length allShow


