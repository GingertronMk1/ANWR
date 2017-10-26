----------------------------------------------------------------------------------------------------------------------------------------------------------------
We're first going to import some things:
- Data.List for isInfixOf, sort, and group
- System.Directory so we can muck about with files and dirs
- System.IO.Unsafe so we can do things we really oughtn't
- Data.Char for intToDigit
- And finally Data.Text and Data.Text.IO for stricter file reading
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
- Details for the list we're going to generate that contains all the important bits of a show
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
> testShows = ["Camp Macbeth", "Uz and Them"]
> testPeople = [me, "Angharad Davies", ian]

--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
First we need to build a list of all of the shows that have records on the history site, step by step.
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

First within this, build the first level of directories: the years, prepending them with the show path and a forward slash

> baseDirs :: IO [FilePath]
> baseDirs = do baseDir <- getDirectoryContents showsPath
>               return $ map (\s -> showsPath ++ s ++ "/") $ drop 2 baseDir

Then we take that, and extract the contents, prepending the containing directory. This is a messy type but it's OK, the IO'll get pulled out later

> dirBuilder :: IO [IO [FilePath]]
> dirBuilder = do baseDir <- baseDirs
>                 return $ map getDirContentsPrep baseDir
>                 where getDirContentsPrep s = do contents <- getDirectoryContents s
>                                                 return $ map (s++) (drop 2 contents)

Finally, we use `sequence` to pull all the IO out to the front (told you it'd be fine) so we can work on the list as a pure type within other `do` blocks.
We're left with a Monadic list of all show filepaths, excluding the "Freshers Fringe"s, cos otherwise this program becomes very boring (and computationally difficult)

> allShowsDo :: IO [FilePath]
> allShowsDo = do allDirs' <- dirBuilder
>                 allDirs <- sequence allDirs'
>                 return $ filter showFilter $ flatten allDirs
>                 where showFilter s = isInfixOf ".md" s && not (isInfixOf "freshers_fringe" s)

--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
Now that we've got a list of all of the shows, we need to extract from it a list of all actors.
First we're going to extract just the actors from a single show, as such:
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

This is the final product: a function that takes a FilePath and returns a 3-tuple containing the filepath, the show title, and a list of actors in the show.

> showDetails :: FilePath -> IO Detail
> showDetails s = do fileContents <- strictReadFile s
>                    let fileLines = lines fileContents
>                    return (s, getTitle fileLines, peopleNames fileLines)

This basically means we can cock about with files a bit more than we otherwise might be able to

> strictReadFile :: FilePath -> IO String
> strictReadFile = fmap T.unpack . TIO.readFile

In the .md files that inform this program, the "cast" section comes before the "crew" section, so we'll take everything before crew

> actorFilter :: [String] -> [String]
> actorFilter ss = takeWhile (\s -> not (isInfixOf "crew:" s)) ss

And because it turns out naming's weird, we'll drop everything before the word "cast"

> dropUntilCast :: [String] -> [String]
> dropUntilCast ss = dropWhile (\s -> not (isInfixOf "cast:" s)) ss

Now taking just the lines that correspond to people's names

> filterPeople :: [String] -> [String]
> filterPeople ss = filter (isInfixOf " name:") $ dropUntilCast $ actorFilter ss

We can get rid of any trailing whitespace now

> stripEndSpace :: String -> String
> stripEndSpace (c:' ':[]) = [c]
> stripEndSpace (c:[]) = [c]
> stripEndSpace (c:cs) = c:(stripEndSpace cs)

And the final bit of formatting, cleaning up everything that's not just the person's name

> peopleNames :: [String] -> [Actor]
> peopleNames ss = map (stripEndSpace . drop 2 . dropWhile (/= ':')) $ filterPeople ss

A similar thing can be done for the title, but we also have to strip quote marks because naming is inconsistent

> getTitle :: [String] -> String
> getTitle = stripQuotes . stripEndSpace . drop 2 . dropWhile (/= ':') . head . filter (isInfixOf "title:")

This is the function that allows us to do that last part

> stripQuotes s = if head s == '\"' && last s == '\"' then (init . tail) s
>                                                     else s

Finally, we map the showDetails function we've been building for the last few dozen lines and apply it to every show there's a record of.

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

Okidokie then, this has to work a little differently to usual because nested IO gets messy fast.
We'll start with a function that builds a tree of a certain size by default, building a tree in the usual way.

> limTreeIO :: Int -> Details -> Actor -> IO (Tree Actor)
> limTreeIO 0 dt actorName = do return $ Node actorName []
> limTreeIO n dt actorName = do fellows <- allFellows dt actorName
>                               fellowTrees <- sequence (map (limTreeIO (n-1) dt) fellows)
>                               return $ Node actorName fellowTrees

Now we plumb that into a function that automatically applies the right limit (as defined at the top of the file)

> treeGenIO :: Details -> Actor -> IO (Tree Actor)
> treeGenIO dt actorName = limTreeIO treeLim dt actorName

treeCheck does the bulk of the work in this program, taking a prebuilt list of show details, the name of an Actor, the tree of another Actor, and returns a tree whose root is the link between the two actors.
Yeah, it does a lot, I could probably do to break it down a bit more tbh.

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

minTuple, used in conjunction with nodeVal, lets us find the lowest degree of separation between two actors in the tree

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
>                 pp (Node (as, ss, i) _) = if i > treeLim then putStrLn $ "These people are either not linked or there are more than " ++ [intToDigit treeLim] ++ " degrees of separation"
>                                           else if i == 0 then putStrLn $ "You've put the same person twice you idiot"
>                                                          else putStrLn $ head as ++ " and " ++ last as ++
>                                                                          " are linked as follows: " ++ (links as ss) ++ ".\n" ++
>                                                                          "They have " ++ [intToDigit i] ++ " degrees of separation."

Lots of helper functions now to create the string for the above, and make it all look nice

> actorLink :: [Actor] -> [(Actor, Actor)]
> actorLink (a:[]) = []
> actorLink (a:b:as) = (a,b):(actorLink (b:as))

> actorLinkWShow :: [(Actor, Actor)] -> [ShowName] -> [(Actor, ShowName, Actor)]
> actorLinkWShow (a:[]) (s:[]) = [(fst a, s, snd a)]
> actorLinkWShow (a:as) (s:ss) = (fst a, s, snd a):(actorLinkWShow as ss)

> actorLinkString :: [(Actor, Actor)] -> String
> actorLinkString (as:[]) = fst as ++ " -> " ++ snd as
> actorLinkString (as:ass) = fst as ++ " -> " ++ snd as ++ ", " ++ actorLinkString ass

> showLink :: [Actor] -> [ShowName] -> [(Actor, ShowName, Actor)]
> showLink as ss = actorLinkWShow (actorLink as) ss

> ppLinks :: [(Actor, ShowName, Actor)] -> String
> ppLinks ((a1,s,a2):[]) = a1 ++ " was in " ++ s ++ " with " ++ a2
> ppLinks ((a1,s,a2):as) = a1 ++ " was in " ++ s ++ " with " ++ a2 ++ ", " ++ ppLinks as

> links :: [Actor] -> [ShowName] -> String
> links as ss = ppLinks $ showLink as ss


--------------------------------------------------------------------------------------------------------------------------------------------------------------
TODO
--------------------------------------------------------------------------------------------------------------------------------------------------------------

Refactor main so the files are only accessed once:    DONE WOOP WOOP

--------------------------------------------------------------------------------------------------------------------------------------------------------------
TESTING
----------------------------------------------------------------------------------------------------------------------------------------------------------------

Prettily printing a tree, for no real reason other than it looks cool

> ppTree' (Node a []) depth = a
> ppTree' (Node a as) depth = a ++ "\n" ++
>                             replicate depth ' ' ++
>                             flatten (map (\x -> (ppTree' x (depth+2)) ++ '\n':( replicate depth ' ')) as)

> ppTree :: Tree String -> IO ()
> ppTree (Node a as) = putStrLn $ ppTree' (Node a as) 2


This flattens a tree into a single list; useful for measuring how changing the treeLim value affects the size of the tree

> flatTree (Node a []) = [a]
> flatTree (Node a as) = a :(flatten $ map flatTree as)

> treeLenComp = do allDetails <- allShowDetails
>                  trees <- sequence $ map (\x -> limTreeIO x allDetails me) [treeLim..treeLim]
>                  return $ map (length . flatTree) trees

> my1Tree = do allDetails <- allShowDetails
>              tree <- limTreeIO 3 allDetails me
>              ppTree tree

This is for debugging, it tells me how many shows we've got. If it doesn't match the value on `history.newtheatre.org.uk`, something's up

> allShowsLength = do allShow <- allShowsDo
>                     return $ length allShow

> allShowTitles = do allShows <- allShowDetails
>                    let allTitles = map getSecond allShows
>                    return (allTitles, length allTitles)
