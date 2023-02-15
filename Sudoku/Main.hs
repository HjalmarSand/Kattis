--Skriven av Hjalmar Sandblom och Erik Biörklund
-- How to run: 1. Enter the reposoitory including this haskell file and run "ghc --make Main"
--2. Type ./Main and enter the txt file containing the desired sudokufile, make sure this txt file is in the same repository
-- Note: Make sure the dimensions of sudoku match the values if size and root in the top of the code

module Main where
import Data.Bool
import Data.Char
import Data.List
import Data.Maybe hiding (fromMaybe)


main :: IO ()
main = do
  fileName <- getLine
  file <- readFile fileName
  let size = length (head(lines file))
  putStrLn ("Size of board: " ++ show size)
  let boards = concatInnerRows $ filterFile (lines file) size
  putStrLn $ show (length boards) ++ " boards registered \n"
  putStrLn $ show (zip [1..] [if verifySudoku x then "Solvable" else "Not Solvable" | x <- boards])


-- Removes equal String
filterFile :: [String] -> Int -> [[String]]
filterFile [] _ = []
filterFile fileLines size = take size fileLines : filterFile (drop (size + 1) fileLines) size

-- Concatenates the values in the inner lists together
concatInnerRows :: [[String]] -> [String]
concatInnerRows xs = [concat x | x <- xs]

--print the given integerstring in sudoku form
printSudoku :: String -> Int -> IO ()
printSudoku board size =
    if board == "" then
        return ()
    else
        do
        putStrLn (take size board)
        printSudoku (drop size board) size

--Prints the board but the units with impossible values are replaced with a "-"
prettyPrint :: String -> Int -> IO ()
prettyPrint board size = do
  let zipped = zip board (map snd ((validBoardNumbers (parseStringToBoard board size)) size ))
  let units =  [(x, validUnit x (validBoardNumbers (parseStringToBoard board size) size ) size) | x <- unitList size]
  if validUnits (parseStringToBoard board size) size then
    printSudoku (map (\x -> if null (snd x) then 'X' else fst x) zipped) size
  else
    printList (map (\x -> if not (snd x) then " Fel i unit " ++ concatStringList (fst x) else "") units)

cross :: [a] -> [a] -> [[a]]
cross s1 s2 = [[r, c] | r <- s1, c <- s2]
ints = [1..9]
dynamicLetter = ['A'..'Z']
dynamicNumber = "123456789"

rowBoxes, colBoxes :: [String]
rowBoxes = ["ABC", "DEF", "GHI"]
colBoxes = ["123", "456", "789"]

smallRowBoxes,smallColBoxes :: [String]
smallRowBoxes = ["AB", "CD"]
smallColBoxes = ["12", "34"]

rows, cols :: String
rows = concat rowBoxes
cols = concat colBoxes

smallRows, smallCols :: String
smallRows = concat smallRowBoxes
smallCols = concat smallColBoxes

squares,smallSquares :: [String]
squares = cross rows cols
smallSquares = cross smallRows smallCols

unitlist :: Int -> [[String]]
unitlist size  =
  if size == 9 then
    [cross rows [c] | c <- cols]
    ++ [cross [r] cols | r <- rows]
    ++ [cross rs cs | rs <- rowBoxes, cs <- colBoxes]
  else
    [cross smallRows [c] | c <- smallCols]
    ++ [cross [r] smallCols | r <- smallRows]
    ++ [cross rs cs | rs <- smallRowBoxes, cs <- smallColBoxes]

units :: Int -> [(String, [[String]])]
units size =
  if size == 9 then
    [(s, filter (elem s) (unitlist size)) | s <- squares]
  else
    [(s, filter (elem s) (unitlist size)) | s <- smallSquares]

peers :: Int ->  [(String, [String])]
peers size = map (\(s, u) -> (s, delete s (foldl union [] u))) (units size)

type Board = [(String, [Int])]


concatStringList :: [String] -> String
concatStringList [] = ""
concatStringList xs = concat xs
--concatStringList (x:xs) = x + concatStringList xs


printList :: [String] -> IO ()
printList [] = putStrLn ""
printList xs = putStrLn (concatStringList xs)

fromMaybe :: a -> Maybe a -> a
fromMaybe d x = case x of
  {Nothing -> d ; Just v  -> v}

justifyList :: [Maybe a] -> [a]
justifyList xs = [x | Just x <- xs]

lookups :: Eq a => [a] -> [(a, b)] -> [b]
lookups xs ys = justifyList [lookup x ys | x <- xs]

--reduceList :: Eq a => [a] -> [a] -> [a]
reduceList :: Eq a => [a] -> [a] -> [a]
reduceList [] _ = []
reduceList xs [] = xs
reduceList xs (y:ys)
  | null ys = filter (/= y) xs
  | otherwise  = reduceList (filter (/= y) xs ) ys


dynamicCross :: Int -> [String]
dynamicCross size = cross (take size dynamicLetter) (take size dynamicNumber)
-- Replaces all points in a list of chars with 0:s by mapping through the list and, if the char currently mapped through
-- is a . then 0 else returns the element

replacePointsWithZeros :: String -> String
replacePointsWithZeros = map (\c -> if c=='.' then '0'; else c)

--Takes a binary sequence and maps every square to its corresponding binary value 
parseStringToBoard :: String -> Int -> [(String, Int)]
parseStringToBoard a size = zip (dynamicCross size) (map digitToInt (replacePointsWithZeros a))

--Divides a String into equal elements of root 
boxCreator :: [a] -> Int -> [[a]]
boxCreator [] _ = []
boxCreator xs size =
  if size == 9 then
    take 3 xs : boxCreator (drop 3 xs) size
  else
    take 2 xs : boxCreator (drop 2 xs) size

--Creates every unit in a 4x4 sudoku by using cross product on a row with every col, col with every row 
--and cross the letterboxes with the numberboxes to create a list of lists
--if desiring a 9x9 or bigger sudoku, this function either needs to take input or manually change rows, cols and numberboxes 
unitList :: Int -> [[String]]
unitList size = [cross (take size dynamicLetter) [col] | col <- take size dynamicNumber] ++
            [cross [row] (take size dynamicNumber) | row <- take size dynamicLetter] ++
            [cross xs ys  | xs  <- boxCreator (take size dynamicLetter) size, ys <- boxCreator (take size dynamicNumber) size]

--Finds all units in unitlist containging the inputstring
filterUnitList :: String -> Int -> [[String]]
filterUnitList x size = [u | u <- (unitList size), x  `elem` u]


-- sorts the list, then returns the list with each element grouped with itself then takes head of each group i.e removes duplicates
removeDuplicates :: (Ord a) => [a] -> [a]
removeDuplicates [] = []
removeDuplicates (x:xs)
  |  x `elem` xs = removeDuplicates xs
  | otherwise = x:removeDuplicates xs

--Returns all other peers for input square
getPeers :: String -> Int -> [String]
getPeers x size = fromMaybe ["0"] (lookup x (peers size))

validSquare :: (String, Int) -> [(String, Int)] -> Int -> Bool
validSquare (_, 0) _ _ = True
validSquare sq b size = sqrValue `notElem` peerValues
  where
    peerValues = lookups ((getPeers . fst ) sq size) b
    sqrValue = snd sq

validSquareNumbers :: (String,Int) -> [(String,Int)] -> Int -> (String,[Int])
validSquareNumbers (sq,v) b size
  | v == 0               = (sq, possibleValues)
  | validSquare (sq,v) b size = (sq,[v])
  | otherwise            = (sq,[])
  where
    possibleValues = foldl reduceList [1..size] peerValues
    peerValues = [lookups unit b | unit <- filterUnitList sq size]

--Returns all possible values for all squares with input board
validBoardNumbers :: [(String, Int)] -> Int -> [(String, [Int])]
validBoardNumbers xs size =  [validSquareNumbers x xs size | x <- xs]

  --if length snd validSquareNumbers  == 1 then [if x == snd validSquareNumbers | x <- getPeers snd validSquareNumbers parseBoard]

-- Checks if all squares in a unit can contain a unique integer
validUnit :: [String] -> [(String, [Int])] -> Int -> Bool
validUnit xs ys size =
  if (and [ int `elem` concat (lookups xs ys) | int <- [1..size]]) then
  not ( filter (\x -> length x == 1) (lookups xs ys) > removeDuplicates (filter (\x -> length x == 1) (lookups xs ys)) )
  else False

-- calculates validity for input board
validUnits :: [(String, Int)] -> Int -> Bool
validUnits y size = and [validUnit x (validBoardNumbers y size) size | x <- unitList size]

--Given input String (i.e 12....34.......) returns if it is possible to solve sudoku
verifySudoku :: String -> Bool
verifySudoku x  =

  if length(x) == 81 then
    validUnits (parseStringToBoard x 9) 9
  else
    validUnits (parseStringToBoard x 4) 4


