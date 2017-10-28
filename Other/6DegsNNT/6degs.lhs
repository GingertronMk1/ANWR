----------------------------------------------------------------------------------------------------------------------------------------------------------------
We're first going to import some things:
- Data.List for isInfixOf, sort, and group
- Data.Ord for sorting fun times
- System.Directory so we can muck about with files and dirs
- System.IO.Unsafe so we can do things we really oughtn't
- Data.Char for intToDigit
- And finally Data.Text and Data.Text.IO for stricter file reading
----------------------------------------------------------------------------------------------------------------------------------------------------------------

> import Data.List
> import Data.Ord
> import System.Directory
> import System.IO.Unsafe
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
> type Detail = (FilePath, ShowName, [Actor])

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
> testShows = ["Camp Macbeth", "Uz and Them"]
> testPeople = [me, "Angharad Davies", ian]

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
> filterPeople ss = filter (isInfixOf " name:") $ extractCast ss

Next, two helpers: the first strips any trailing white space from the name, and the second removes any quote marks surrounding it

> stripEndSpace :: String -> String
> stripEndSpace (c:' ':[]) = [c]
> stripEndSpace (c:[]) = [c]
> stripEndSpace (c:cs) = c:(stripEndSpace cs)

> stripQuotes :: String -> String
> stripQuotes s = if head s == '\"' && last s == '\"' then (init . tail) s
>                                                     else s

With those, we can extract just the name from the string

> peopleNames :: [String] -> [Actor]
> peopleNames ss = map (stripQuotes . stripEndSpace . drop 2 . dropWhile (/= ':')) $ filterPeople ss

Also we can use them to get the title as well, which is nice

> getTitle :: [String] -> String
> getTitle = stripQuotes . stripEndSpace . drop 2 . dropWhile (/= ':') . head . filter (isInfixOf "title:")

Applying these, we can extract the details from a specific file

> showDetails :: FilePath -> IO Detail
> showDetails s = do fileContents <- strictReadFile s
>                    let fileLines = lines fileContents
>                    return (s, getTitle fileLines, peopleNames fileLines)
>                    where strictReadFile = fmap T.unpack . TIO.readFile

And finally, we can map this across all of the shows (i.e. that list we generated with `allShows`

> allShowDetails :: IO [Detail]
> allShowDetails = do allDirs' <- allShows
>                     allDirs <- sequence allDirs'
>                     sequence $ map showDetails $ filter (\s -> isInfixOf ".md" s && not (isInfixOf "freshers_fringe" s)) (flatten allDirs)

--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
Helper functions!
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

First, duplicate removal

> rmdups :: Ord a => [a] -> [a]
> rmdups = map head . group . sort

Flattening lists of lists

> flatten :: [[a]] -> [a]
> flatten ass = [a | as <- ass, a <- as]

Helpers to extract various bits of info from the show details (Haskell gets shaky around 3-tuples)

> getFirst :: (a,b,c) -> a
> getFirst (a,b,c) = a
> getSecond :: (a,b,c) -> b
> getSecond (a,b,c) = b
> getThird :: (a,b,c) -> c
> getThird (a,b,c) = c

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
>                        where allFellows dt a = (rmdups . flatten . filter (elem a)) (map getThird dt)

> limTree :: Int -> Tree a -> Tree a
> limTree 0 (Node a as) = Node a []
> limTree n (Node a as) = Node a [limTree (n-1) a2 | a2 <- as]

> limitedTree :: [Detail] -> Actor -> Tree Actor
> limitedTree dt a = limTree treeLim $ treeGen dt a

> treeCheck :: [Detail] -> Actor -> Tree Actor -> Tree ([Actor], [ShowName], Int)
> treeCheck detailList target (Node a []) = if a == target then Node ([a], [], 0) [] else Node ([a], [], 1000) []
> treeCheck detailList target (Node a as) = if a == target then Node ([a], [], 0) [] else Node (a:prevA, link:prevLink, 1+c) []
>                                            where (prevA, prevLink, c) = minTuple $ map (nodeVal . treeCheck detailList target) as
>                                                  link = findShowWActors a (head prevA) detailList
>                                                  findShowWActors a1 a2 detailList = getSecond . head $ filter (\s -> elem a1 (getThird s) && elem a2 (getThird s)) detailList

----------------------------------------------------------------------------------------------------------------------------------------------------------------
So, we're generating a tuple containing a list of actors, and the show-based links between them
----------------------------------------------------------------------------------------------------------------------------------------------------------------

First, we take the list of Actors, and put them together in tuples

> actorLink :: [Actor] -> [(Actor, Actor)]
> actorLink (a:[]) = []
> actorLink (a:b:as) = (a,b):(actorLink (b:as))

Then, we take that list, the list of shows, and make a 3-tuple with the two Actors around the ShowName

> actorLinkWShow :: [(Actor, Actor)] -> [ShowName] -> [(Actor, ShowName, Actor)]
> actorLinkWShow (a:[]) (s:[]) = [(fst a, s, snd a)]
> actorLinkWShow (a:as) (s:ss) = (fst a, s, snd a):(actorLinkWShow as ss)

And finally, we take a list of actors and shows, and using the above functions, we can generate a string with each link

> links :: [Actor] -> [ShowName] -> String
> links as ss = ppLinks $ actorLinkWShow (actorLink as) ss
>               where ppLinks ((a1,s,a2):as) = let a1sa2String = a1 ++ " was in " ++ s ++ " with " ++ a2
>                                              in if as == [] then a1sa2String else a1sa2String ++ ", " ++ ppLinks as

----------------------------------------------------------------------------------------------------------------------------------------------------------------
Finish line!
Using everything above here, we can get two Actors, and return a printed String with the shortest link between them.
----------------------------------------------------------------------------------------------------------------------------------------------------------------

> main' :: Actor -> Actor -> IO()
> main' a1 a2  = do allDetails <- allShowDetails
>                   pp (treeCheck allDetails a2 (limitedTree allDetails a1))
>                   where flatCommas = flatten . intersperse ", "
>                         pp (Node (as, ss, i) _) = if i > treeLim then putStrLn $ "These people are either not linked or there are more than " ++ [intToDigit treeLim] ++ " degrees of separation"
>                                                   else if i == 0 then putStrLn $ "You've put the same person twice you idiot"
>                                                                  else putStrLn $ head as ++ " and " ++ last as ++
>                                                                                  " are linked as follows: " ++ (links as ss) ++ ".\n" ++
>                                                                                  "They have " ++ [intToDigit i] ++ " degrees of separation."

> main :: IO()
> main = do a1 <- getLine
>           a2 <- getLine
>           main' a1 a2

