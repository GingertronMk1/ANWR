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

--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
A few test variables now:
- treeLim so we can change the depth of a tree here rather than having to muck about in functions way down
- showsPath is where the shows are in my copy of the history-project repo
- escapePath is where the Lakeside performance of Escape For Dummies is, as a test show
- And myself and some people as test cases for the actual degree-finder
- Finally, a test tree for demonstrating printing things
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

> treeLim :: Int
> treeLim = 2
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
> testTree :: Tree Actor
> testTree = limitTree 2 $ treeGen me

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

This is for debugging, it tells me how many shows we've got. If it doesn't match the value on `history.newtheatre.org.uk`, something's up

> allShowsLength = do allShow <- allShowsDo
>                     return $ length allShow

> allShows :: [ShowName]
> allShows = unsafePerformIO allShowsDo

--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
Now that we've got a list of all of the shows, we need to extract from it a list of all actors.
First we're going to extract just the actors from a single show, as such:
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

> actorNames :: FilePath -> IO (ShowName, [Actor])
> actorNames s = do filecontent <- strictReadFile s
>                   let fileLines = lines filecontent
>                   return $ (s, peopleNames fileLines)

> showDetails :: FilePath -> IO (FilePath, ShowName, [Actor])
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

> actorsInShows :: [(ShowName, [Actor])]
> actorsInShows = unsafePerformIO actorsInShowsDo

> actorsInShowsDo :: IO [(FilePath, [Actor])]
> actorsInShowsDo = do showNames <- allShowsDo
>                      showNamesIO <- sequence $ map actorNames showNames
>                      return $ filter (\s -> snd s /= []) showNamesIO

> allShowDetails :: IO [(FilePath, ShowName, [Actor])]
> allShowDetails = do showNames <- allShowsDo
>                     showDetailsIO <- sequence $ map showDetails showNames
>                     return showDetailsIO

> allShowTitles = do s <- allShowDetails
>                    return $ map getShowTitle s


> getTitle :: [String] -> String
> getTitle = drop 2 . dropWhile (/= ':') . head . filter (isInfixOf "title:")

Helpers to extract various bits of info from the show details

> getShowPath :: (FilePath, ShowName, [Actor]) -> FilePath
> getShowPath (f, t, as) = f
> getShowTitle :: (FilePath, ShowName, [Actor]) -> ShowName
> getShowTitle (f, t, as) = t
> getShowActors :: (FilePath, ShowName, [Actor]) -> [Actor]
> getShowActors (f, t, as) = as

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

> allFellow :: Actor -> [Actor]
> allFellow n = rmdups [a | as <- allFellow' n, a <- as, a /= n]
>               where allFellow' n = filter (elem n) (map snd actorsInShows)

> allFellowDo :: Actor -> IO [Actor]
> allFellowDo n = do asd <- allShowDetails
>                    let allActors = map getShowActors asd
>                    return $ rmdups [a | as <- filter (elem n) allActors, a <- as, a /= n]

--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
Generating trees now: given an actor's name, we generate a list of all of their fellow actors and use that to generate a tree of their connection to the theatre population
This tree is obviously infinite, having no final case, so we need to limit it
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

> treeGen :: Actor -> Tree Actor
> treeGen actorName = Node actorName [treeGen a | a <- allFellow actorName]

> limTreeIO :: Int -> Actor -> IO (Tree Actor)
> limTreeIO 0 actorName = do return $ Node actorName []
> limTreeIO n actorName = do fellows <- allFellowDo actorName
>                            fellowTrees <- sequence (map (limTreeIO (n-1)) fellows)
>                            return $ Node actorName fellowTrees

> treeGenIO :: Actor -> IO (Tree Actor)
> treeGenIO actorName = limTreeIO treeLim actorName

> limitTree :: Int -> Tree a -> Tree a
> limitTree 0 (Node x _) = Node x []
> limitTree n (Node x ts) = Node x [limitTree (n-1) t | t <- ts]

> limitedTree :: Actor -> Tree Actor
> limitedTree actorName = limitTree treeLim $ treeGen actorName

treeCheck takes a tree and a target name, and returns the distance from the root node to the first instance of the target name

> treeCheck :: Actor -> Tree Actor -> Tree ([Actor], [ShowName], Int)
> treeCheck target (Node a []) = if a == target then Node ([a], [], 0) [] else Node ([a], [], 1000) []
> treeCheck target (Node a as) = if a == target then Node ([a], [], 0) [] else Node (a:b, link:oldLink, 1+c) []
>                                where (b, oldLink, c) = minTuple2 $ map (nodeVal3 . treeCheck target) as
>                                      link = findShowsWActors a (head b)
>                                      findShowsWActors a1 a2 = fst . head $ filter (\s -> elem a1 (snd s) && elem a2 (snd s)) actorsInShows

> treeCheckIO a2 a1 = do a2Tree <- treeGenIO a2
>                        return $ treeCheck a1 a2Tree

nodeVal takes the Tree Int generated by treeCheck and takes just the Int from it

> nodeVal3 :: Tree (a,b,c) -> (a,b,c)
> nodeVal3 (Node (a,b,c) _) = (a,b,c)

> ppTrail :: Actor -> Actor -> String
> ppTrail a1 a2 = if c > treeLim then "These people are not linked"
>                                else a1 ++ " and " ++ a2 ++ " are linked by these people: " ++ flatten (intersperse ", " a) ++ " and via these shows " ++ flatten (intersperse ", " b)
>                 where Node (a,b,c) as = treeCheck a2 $ limitedTree a1

> minTuple2 :: Ord a => [(a1,a2,a)] -> (a1,a2,a)
> minTuple2 (x:xs) = minTail x xs
>                    where minTail x [] = x
>                          minTail (p,q,r) ((d,e,f):ms)
>                            | r > f = minTail (d,e,f) ms
>                            | otherwise = minTail (p,q,r) ms

----------------------------------------------------------------------------------------------------------------------------------------------------------------
For use when the program is compiled using GHC; main takes two names entered and returns either the degree or an 'x' if the degree is too great
----------------------------------------------------------------------------------------------------------------------------------------------------------------

> main :: IO()
> main = do a1 <- getLine
>           a2 <- getLine
>           putStrLn $ ppTrail a1 a2






> main2 :: IO()
> main2 = do a1 <- getLine
>            a2 <- getLine
>            let a1Tree = limitedTree a1
>--            actorShowList <- map actorNames allShows
>            putStrLn $ a1 ++ a2

Prettily printing a tree, for no real reason other than it looks cool

> ppTree' (Node a []) depth = a
> ppTree' (Node a as) depth = a 
>                             ++ "\n" 
>                             ++ replicate depth ' ' 
>                             ++ flatten (map (\x -> (ppTree' x (depth+2)) ++ '\n':( replicate depth ' ')) as)

> ppTree (Node a as) = putStrLn $ ppTree' (Node a as) 2

This flattens a tree into a single list; useful for measuring how changing the treeLim value affects the size of the tree


--------------------------------------------------------------------------------------------------------------------------------------------------------------
TODO
--------------------------------------------------------------------------------------------------------------------------------------------------------------

Refactor main so the files are only accessed once

--------------------------------------------------------------------------------------------------------------------------------------------------------------
TESTING
----------------------------------------------------------------------------------------------------------------------------------------------------------------

> flatTree (Node a []) = [a]
> flatTree (Node a as) = a :(flatten $ map flatTree as)

> treeSize :: Int -> Actor -> Int
> treeSize d a = length $ flatTree $ limitTree d $ treeGen a

> compareSize :: [Int]
> compareSize = map (\x -> treeSize x me) [1..treeLim]


> testList = [me, ian, jamie, omid]
