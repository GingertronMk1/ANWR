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
> testTree :: Tree Actor
> testTree = limitTree 2 $ treeGen me

--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
First we need to build a list of all of the shows that have records on the history site, step by step.
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

First we take the `showsPath` variable and returns a list of all of the directories inside

> getAllShows :: IO [FilePath]
> getAllShows = getDirectoryContents showsPath

Now we prepend the `showsPath` variable to all of these file names (whilst removing the IO from them)

> allShowDirs :: [FilePath]
> allShowDirs = map (showsPath++) (unsafePerformIO getAllShows)

Up next, getting all of their contents, making the heads of these lists the containing directory so we can prepend that later
More removal of IO as well here

> allShowsHeadDir :: [[FilePath]]
> allShowsHeadDir = map (\s -> s:(unsafePerformIO (getDirectoryContents s))) allShowDirs

Doing the aforementiond prepending of file paths

> allShowsWDir :: [[FilePath]]
> allShowsWDir = map allShowsWDir' allShowsHeadDir
>                where allShowsWDir' ss = map (\s -> (head ss)++"/"++s) $ tail ss

Now we flatten these lists, resulting in one giant list of all shows, but not quite...

> allShowsFlat :: [FilePath]
> allShowsFlat = flatten allShowsWDir

First we have to filter it down to just MarkDown files and also get rid of anything from containing folders
Now we have a comprehensive list of all shows in the history site's archive

> allShows :: [ShowName]
> allShows = filter (\s -> isInfixOf ".md" s && not (isInfixOf ".." s) && not (isInfixOf "freshers_fringe" s)) allShowsFlat

--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
Now that we've got a list of all of the shows, we need to extract from it a list of all actors.
First we're going to extract just the actors from a single show, as such:
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

> actorNames :: ShowName -> IO [Actor]
> actorNames s = do filecontent <- readFile s
>                   return $ peopleNames $ lines filecontent

> actorFilter :: [String] -> [String]
> actorFilter ss = takeWhile (\s -> not (isInfixOf "crew:" s)) ss

> filterPeople :: [String] -> [String]
> filterPeople ss = filter (\s -> isInfixOf " name:" s) $ actorFilter ss

> stripEndSpace :: String -> String
> stripEndSpace (c:' ':[]) = [c]
> stripEndSpace (c:[]) = [c]
> stripEndSpace (c:cs) = c:(stripEndSpace cs)

> peopleNames :: [String] -> [Actor]
> peopleNames ss = map (stripEndSpace . drop 2 . dropWhile (/= ':')) $ filterPeople ss

> actorss :: [[Actor]]
> actorss = filter (/= []) $ map (unsafePerformIO . actorNames) allShows
> flatten :: [[a]] -> [a]
> flatten ass = [a | as <- ass, a <- as]

> allActors :: [String]
> allActors = rmdups $ flatten $ actorss

--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
So, we can generate a list of all actors ever at the NNT (that the history site knows of).
Now, we need some way of finding all actors one particular actor has acted with.
Act doesn't seem like part of a word anymore.
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

Up first, a helper function to remove duplicates, making use of some fun bits from Data.List

> rmdups :: Ord a => [a] -> [a]
> rmdups = map head . group . sort

Now the meat.
allFellow takes an Actor name, and the list of all Actors (`actorss`), filters `actorss` to just the lists with the target in them, and flattens out that list, removing duplicates

> allFellow :: Actor -> [Actor]
> allFellow n = rmdups [a | as <- allFellow' n, a <- as, a /= n]
>               where allFellow' n = filter (elem n) actorss

--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
Generating trees now: given an actor's name, we generate a list of all of their fellow actors and use that to generate a tree of their connection to the theatre population
This tree is obviously infinite, having no final case, so we need to limit it
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

> treeGen :: Actor -> Tree Actor
> treeGen actorName = Node actorName [treeGen a | a <- allFellow actorName]

> limitTree :: Int -> Tree a -> Tree a
> limitTree 0 (Node x _) = Node x []
> limitTree n (Node x ts) = Node x [limitTree (n-1) t | t <- ts]

> limitedTree :: Actor -> Tree Actor
> limitedTree actorName = limitTree treeLim $ treeGen actorName

treeCheck takes a tree and a target name, and returns the distance from the root node to the first instance of the target name

> treeCheck :: Actor -> Tree Actor -> Tree Int
> treeCheck target (Node a []) = if a == target then Node 1 [] else Node 1000 []
> treeCheck target (Node a as) = if a == target then Node 1 [] else Node (1 + (minimum ns)) []
>                                               where ns = map (nodeVal . (treeCheck target)) as

nodeVal takes the Tree Int generated by treeCheck and takes just the Int from it

> nodeVal :: Tree Int -> Int
> nodeVal (Node n _) = n

sixDegs uses treeCheck and limitedTree to take two Actor names and return the degree of separation
If this value is greater than the size of the tree (it'll be 1000 and something), it returns 1000
The thought does occur that I should be using Maybe for this but oh well I've come this far

> sixDegs :: Actor -> Actor -> Int
> sixDegs target base = if calcVal > treeLim then 1000
>                                            else calcVal
>                       where calcVal = nodeVal (treeCheck target (limitedTree base)) - 1

----------------------------------------------------------------------------------------------------------------------------------------------------------------
For use when the program is compiled using GHC; main takes two names entered and returns either the degree or an 'x' if the degree is too great
----------------------------------------------------------------------------------------------------------------------------------------------------------------

> main :: IO()
> main = do a1 <- getLine
>           a2 <- getLine
>           let sixDegsChar = if sixD == 1000 then 'x' else intToDigit sixD
>                             where sixD = sixDegs a1 a2
>           putStrLn $ sixDegsChar:[]

Prettily printing a tree, for no real reason other than it looks cool

> ppTree' (Node a []) depth = a
> ppTree' (Node a as) depth = a 
>                             ++ "\n" 
>                             ++ replicate depth ' ' 
>                             ++ flatten (map (\x -> (ppTree' x (depth+2)) ++ '\n':( replicate depth ' ')) as)

> ppTree (Node a as) = putStrLn $ ppTree' (Node a as) 2

This flattens a tree into a single list; useful for measuring how changing the treeLim value affects the size of the tree



-------------------------------------------------------------------------------
OTHER
-------------------------------------------------------------------------------

This is all stuff that doesn't take into account whether someone acted in a show or was on the backstage team for it.
Including this would make the whole program exponentially more massive and slow to run.

allShowsFilt = filter (\x -> isInfixOf ".md" x) allShowsEver
namess = filter (/= []) $ map unsafePerformIO $ map names $ allShowsFilt
everyone :: [String]
everyone = rmdups $ flatten $ namess
names s = do filecontent <- readFile s
             let peopleNames = map stripEndSpace $ map (drop 2) $ map (dropWhile (/= ':')) $ filterPeople $ lines filecontent
                               where filterPeople ss = filter peopleFilter ss
                                     peopleFilter s = isInfixOf " name:" s
             return $ peopleNames

----------------------------------------------------------------------------------------------------------------------------------------------------------------
TESTING
----------------------------------------------------------------------------------------------------------------------------------------------------------------

> flatTree (Node a []) = [a]
> flatTree (Node a as) = a :(flatten $ map flatTree as)

> treeSize :: Int -> Actor -> Int
> treeSize d a = length $ flatTree $ limitTree d $ treeGen a

> compareSize :: [Int]
> compareSize = map (\x -> treeSize x me) [1..treeLim]
