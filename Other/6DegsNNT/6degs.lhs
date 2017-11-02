----------------------------------------------------------------------------------------------------------------------------------------------------------------
We're first going to import some things:
- Data.List for isInfixOf, sort, and group
- Data.Ord for sorting fun times
- System.Directory so we can muck about with files and directories
- Data.Char for intToDigit
- And finally Data.Text and Data.Text.IO for stricter file reading
----------------------------------------------------------------------------------------------------------------------------------------------------------------

> import Data.List
> import Data.Ord
> import System.Directory
> import Data.Char
> import qualified Data.Text as T
> import qualified Data.Text.IO as TIO

--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
Now defining some data types:
- A Tree for a recursive data type
- And Actor and ShowName for type clarity
- [Detail] for the list we're going to generate that contains all the important bits of a show
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

> data Tree a = Node a [Tree a] deriving Show
> type Actor = String
> type ShowName = String
> type Detail = (ShowName, [Actor])

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
> rj :: Actor
> rj = "RJ"

--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
First we need to build a list of all of the shows that have records on the history site
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

Surprisingly enough, this isn't that many lines. First we get all of the contents of the directory where the shows are kept
Then we drop the first 2 (`.` and `..`), and to that list we map the prepending of the showsPath and the appending of a `/` because filepaths
We also map a little functions that extracts the contents of a directory (in this case the files themselves), and prepends the containing folder
And that is the filepath for all of the shows that have records at the NNT

> allShows :: IO [IO [FilePath]]
> allShows = do baseDir <- getDirectoryContents showsPath
>               return $ map (getDirContentsPrep . (\s -> showsPath ++ s ++ "/")) (drop 2 baseDir)
>               where getDirContentsPrep s = do contents <- getDirectoryContents s
>                                               return $ map (s++) (drop 2 contents)

--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
Now that we've got a list of all of the shows, we need to extract from it a list of all actors.
First we're going to extract just the actors from a single show, as such:
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

OK, so the formatting means that we first take everything before the "crew" section, then drop everything before the "cast" section

> extractCast :: [String] -> [Actor]
> extractCast = dropWhile (\s -> not (isInfixOf "cast:" s)) . takeWhile (\s -> not (isInfixOf "crew:" s)) 

After that, we filter such that only the lines containing names remain

> filterPeople :: [String] -> [String]
> filterPeople = filter (isInfixOf " name:") . extractCast

Next, a helper to remove any thing that isn't someone's name in the line
That is, trailing/leading non-letter characters
Basically my problem is that people's names are incredibly inconsistent on the History Site

> stripShit :: String -> String
> stripShit s = if hs == ' ' || hs == ':' || hs == '\"' || hs == '\'' then stripShit $ tail s
>               else if ls == ' ' || ls == '\"' || ls == '\'' then stripShit $ init s
>               else s
>               where hs = head s
>                     ls = last s

With that, we can extract just the name from the string

> peopleNames :: [String] -> [Actor]
> peopleNames = map (stripShit . dropWhile (/= ':')) . filterPeople

Also we can use them to get the title as well, which is nice

> getTitle :: [String] -> String
> getTitle = stripShit . dropWhile (/= ':') . head . filter (isInfixOf "title:")

Applying these, we can extract the details from a specific file

> showDetails :: FilePath -> IO Detail
> showDetails s = do fileContents <- strictReadFile s
>                    let fileLines = lines fileContents
>                    return (getTitle fileLines, peopleNames fileLines)
>                    where strictReadFile = fmap T.unpack . TIO.readFile

And finally, we can map this across all of the shows (i.e. that list we generated with `allShows`)

> allShowDetails :: IO [Detail]
> allShowDetails = do allDirs' <- allShows
>                     allDirs <- sequence allDirs'
>                     (sequence . map showDetails) (filter (\s -> isInfixOf ".md" s && not (isInfixOf "freshers_fringe" s)) (flatten allDirs))

--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
Helper functions!
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

First, duplicate removal

> rmdups :: Ord a => [a] -> [a]
> rmdups = map head . group . sort

Flattening lists of lists

> flatten :: [[a]] -> [a]
> flatten ass = [a | as <- ass, a <- as]

Extracting the value from a Node of a Tree

> nodeVal :: Tree a -> a
> nodeVal (Node a _) = a

You know how Haskell doesn't like 3-tuples? Sometimes you need to find the smallest tuple from a list of them

> minTuple :: Ord a => [(a1,a2,a)] -> (a1,a2,a)
> minTuple (x:[]) = x
> minTuple (x@(a,b,c):y@(m,n,o):xs) = if c < o then minTuple (x:xs) else minTuple (y:xs)

--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
Generating trees now: given an actor's name, we generate a list of all of their fellow actors and use that to generate a tree of their connection to the theatre population
This tree is obviously infinite, having no final case, so we need to limit it
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

> treeGen :: [Detail] -> Actor -> Tree Actor
> treeGen dt actorName = Node actorName [treeGen dt a | a <- allFellows dt actorName, a /= actorName]
>                        where allFellows dt a = (rmdups . flatten . filter (elem a)) (map snd dt)

> limTree :: Int -> Tree a -> Tree a
> limTree 0 (Node a as) = Node a []
> limTree n (Node a as) = Node a [limTree (n-1) a2 | a2 <- as]

> limitedTree :: [Detail] -> Actor -> Tree Actor
> limitedTree dt a = limTree treeLim (treeGen dt a)

> treeCheck :: [Detail] -> Actor -> Tree Actor -> Tree ([Actor], [ShowName], Int)
> treeCheck detailList target (Node a []) = if a == target then Node ([a], [], 0) [] else Node ([a], [], 1000) []
> treeCheck detailList target (Node a as) = if a == target then Node ([a], [], 0) [] else Node (a:prevA, link:prevLink, 1+c) []
>                                            where (prevA, prevLink, c) = minTuple $ map (nodeVal . treeCheck detailList target) as
>                                                  link = (fst . head . filter (\s -> elem a (snd s) && elem (head prevA) (snd s))) detailList

----------------------------------------------------------------------------------------------------------------------------------------------------------------
So we've got 2 lists: one with the actors that link two actors, and another with the shows by which they're linked.
How to put these together, and make it all look nice...
----------------------------------------------------------------------------------------------------------------------------------------------------------------

> links :: [Actor] -> [ShowName] -> String
> links (a1:a2:as) (s:ss) = if as == [] then str else str ++ links (a2:as) ss
>                           where str = "- " ++ a1 ++ " was in " ++ s ++ " with " ++ a2 ++ "\n"

> ppLinks :: Tree ([Actor], [ShowName], Int) -> String
> ppLinks (Node (as, ss, i) _)
>   | i == 0      = "A person has 0 degrees of separation with themself by definition."
>   | i == 1      = headAndLast ++ " were in " ++ head ss ++ " together, with 1 degree of separation."
>   | i > treeLim = headAndLast ++ " are either not linked, or there are more than " ++ [intToDigit i] ++ " degrees of separation."
>   | otherwise   = headAndLast ++ " are linked as follows:\n" ++ links as ss ++ "\nThey have " ++ [intToDigit i] ++ " degrees of separation."
>   where headAndLast = head as ++ " and " ++ last as

----------------------------------------------------------------------------------------------------------------------------------------------------------------
Finish line!
Using everything above here, we can get two Actors, and return a printed String with the shortest link between them.
----------------------------------------------------------------------------------------------------------------------------------------------------------------

> main' :: Actor -> Actor -> IO ()
> main' a1 a2  = do allDetails <- allShowDetails
>                   putStrLn $ ppLinks $ treeCheck allDetails a2 $ limitedTree allDetails a1

> main :: IO ()
> main = do a1 <- getLine
>           a2 <- getLine
>           main' a1 a2
