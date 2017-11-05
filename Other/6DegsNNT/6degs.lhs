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
> type AdjList = [(Actor, Actor, Int)]

--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
A few test variables now:
- treeLim so we can change the depth of a tree here rather than having to muck about in functions way down
- showsPath is where the shows are in my copy of the history-project repo
- And myself and some people as test cases for the actual degree-finder
- Finally, a test tree for demonstrating printing things
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

> treeLim :: Int
> treeLim = 4
> showsPath :: String
> showsPath = "../../../history-project/_shows/"

> me :: Actor
> me = "Jack Ellis"
> ian :: Actor
> ian = "Ian Sheard"
> omid :: Actor
> omid = "Omid Faramarzi"
> rose :: Actor
> rose = "Rose Edgeworth"
> rj :: Actor
> rj = "RJ"


--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
Only the one general helper function; flattening lists of lists happens a fair bit in here
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

> flatten :: [[a]] -> [a]
> flatten ass = [a | as <- ass, a <- as]

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

> filterPeople :: [String] -> [String]
> filterPeople = filter (isInfixOf " name:") . dropWhile (\s -> not (isInfixOf "cast:" s)) . takeWhile (\s -> not (isInfixOf "crew:" s)) 

Next, a helper to remove anything that isn't someone's name in the line
That is, trailing/leading non-letter characters
Basically my problem is that people's names are formatted incredibly inconsistently on the History Site

> stripShit :: String -> String
> stripShit s
>  | hs == ' ' || hs == '\"' || hs == '\'' || hs == ':' = stripShit (tail s)
>  | ls == ' ' || ls == '\"' || ls == '\''              = stripShit (init s)
>  | otherwise                                          = s
>  where hs = head s
>        ls = last s

> getString :: String -> String
> getString = stripShit . dropWhile (/= ':')

With that, we can extract just the name from the string

> getNames :: [String] -> [Actor]
> getNames = map getString . filterPeople

Also we can use them to get the title as well, which is nice

> getTitle :: [String] -> String
> getTitle = getString . head . filter (isInfixOf "title:")

Applying these, we can extract the details from a specific file

> showDetails :: FilePath -> IO Detail
> showDetails s = do fileContents <- (fmap T.unpack . TIO.readFile) s
>                    let fileLines = lines fileContents
>                    return (getTitle fileLines, getNames fileLines)

And finally, we can map this across all of the shows (i.e. that list we generated with `allShows`)
We discount anything that's not a MarkDown file, is a Freshers' Fringe (otherwise this gets very dull), and any show with fewer than 2 actors

> allShowDetails :: IO [Detail]
> allShowDetails = do allDirs' <- allShows
>                     allDirs <- sequence allDirs'
>                     allDT <- (sequence . map showDetails . filter (\s -> isInfixOf ".md" s && not (isInfixOf "freshers_fringe" s)) . flatten) allDirs
>                     return $ filter (\s -> length (snd s) > 1) allDT

--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
Generating trees now: given an actor's name, we generate a list of all of their fellow actors and use that to generate a tree of their connection to the theatre population
This tree is obviously infinite, having no final case, so we need to limit it
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

Generating a tree based on everyone an actor has worked with

> treeGen :: [Detail] -> Actor -> Tree Actor
> treeGen dt actorName = Node actorName [treeGen dt a | a <- allFellows dt actorName, a /= actorName]

> allFellows :: [Detail] -> Actor -> [Actor]
> allFellows dt a = (map head . group . sort . flatten . filter (elem a) . map snd) dt -- `map head . group . sort` removes duplicates

Limiting this tree, as it's otherwise infinite and that's a ballache

> limTree :: Int -> Tree a -> Tree a
> limTree 0 (Node a as) = Node a []
> limTree n (Node a as) = Node a [limTree (n-1) a2 | a2 <- as]

Putting these together because we're never gonna use treeGen naked after this

> limitedTree :: [Detail] -> Actor -> Tree Actor
> limitedTree dt a = limTree treeLim $ treeGen dt a

> treeCheck :: [Detail] -> Actor -> Tree Actor -> Tree ([Actor], [ShowName], Int)
> treeCheck detailList target (Node a as)
>  | null as && a == target       = Node ([a], [], 0) []
>  | null as && a /= target       = Node ([a], [], 1000) []
>  | not (null as) && a == target = Node ([a], [], 0) []
>  | not (null as) && a /= target = Node (a:prevA, link:prevLink, 1+c) []
>  where (prevA, prevLink, c) = (minTreeple . map (treeCheck detailList target)) as
>        link = (fst . head . filter ((\s -> elem a s && elem (head prevA) s) . snd)) detailList

> minTreeple :: Ord a => [Tree (a1,a2,a)] -> (a1,a2,a)
> minTreeple ((Node x _):[])                        = x
> minTreeple ((Node (a,b,c) _):(Node (d,e,f) _):ns) = if c < f then minTreeple ((Node (a,b,c) []):ns) 
>                                                              else minTreeple ((Node (d,e,f) []):ns)

--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
So we've got 2 lists: one with the actors that link two actors, and another with the shows by which they're linked.
How to put these together, and make it all look nice...
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

> links :: [Actor] -> [ShowName] -> String
> links (a1:a2:as) (s:ss) = if as == [] then str else str ++ links (a2:as) ss
>                           where str = "- " ++ a1 ++ " was in " ++ s ++ " with " ++ a2 ++ "\n"

> ppLinks :: Tree ([Actor], [ShowName], Int) -> String
> ppLinks (Node (as, ss, i) _)
>   | i == 0      = "A person has 0 degrees of separation with themself by definition."
>   | i == 1      = headAndLast ++ " were in " ++ head ss ++ " together\n\nThey have 1 degree of separation."
>   | i > treeLim = "These people are either not linked, or there are more than " ++ [intToDigit treeLim] ++ " degrees of separation."
>   | otherwise   = headAndLast ++ " are linked as follows:\n" ++ links as ss ++ "\nThey have " ++ [intToDigit i] ++ " degrees of separation."
>   where headAndLast = head as ++ " and " ++ last as

--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
Finally, using everything above here, we can get two Actors, and return a printed String with the shortest link between them.
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

> findLink :: Actor -> Actor -> [Detail] -> ShowName
> findLink a1 a2 dt = fst $ (head . filter ((\s -> elem a1 s && elem a2 s) . snd)) dt

> oneAdjList a dt = [([a2, a1], 1) | a1 <- allActors, a2 <- allFellows dt a1, a2 /= a1, a1 == a]
>                 where allActors = (map head . group . sort . flatten . map snd) dt

> fellowAdj2 :: [([Actor], Int)] -> [Detail] -> [([Actor], Int)]
> fellowAdj2 [] dt = []
> fellowAdj2 ((ad, i):as) dt = [((a:ad), i+1) | a <- allFellows dt (head ad), not (elem a ad)] ++ fellowAdj2 as dt

> adjFind2 a1 a2 = do dt <- allShowDetails
>                     let aj = adjFind2' a1 (oneAdjList a2 dt) [] dt
>                     putStrLn $ printLinks aj dt

> adjFind2' :: Actor -> [([Actor], Int)] -> [([Actor], Int)] -> [Detail] -> ([Actor], Int)
> adjFind2' t (a:[]) a2 dt = if (head . fst) a == t then a else adjFind2' t (fellowAdj2 (a:a2) dt) [] dt
> adjFind2' t (a:as) a2 dt = if (head . fst) a == t then a else adjFind2' t as (a:a2) dt

> printLinks :: ([Actor], Int) -> [Detail] -> String
> printLinks (as, i) dt
>   | i == 0      = "A person has 0 degrees of separation with themself by definition."
>   | i == 1      = headAndLast ++ " were in " ++ findLink (head as) (last as) dt ++ " together\n\nThey have 1 degree of separation."
>   | i > 1000    = "These people are not linked."
>   | otherwise   = headAndLast ++ " are linked as follows:\n" ++ links2 as dt ++ "\nThey have " ++ [intToDigit i] ++ " degrees of separation."
>   where headAndLast = head as ++ " and " ++ last as

> links2 :: [Actor] -> [Detail] -> String
> links2 (a1:a2:as) dt = if as == [] then str else str ++ links2 (a2:as) dt
>                           where str = "- " ++ a1 ++ " was in " ++ findLink a1 a2 dt ++ " with " ++ a2 ++ "\n"

