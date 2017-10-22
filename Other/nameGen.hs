import Data.List

granddads :: [String]
granddads = ["Ronald", "Terry"]

streets :: [String]
streets = ["Wilbert", "Victoria", "Mace", "Poplars"]

headteachers :: [String]
headteachers = ["Hyland", "Goodwin", "Hodson"]

--nameGen :: [Char]
nameGen = [x ++ " " ++ y ++ "-" ++ z | x <- granddads, y <- streets, z <- headteachers]
