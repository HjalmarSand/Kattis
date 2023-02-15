--Skriven av Hjalmar Sandblom och Erik Biörklund
-- How to run: 1. Enter the reposoitory including this haskell file and run "ghc --make Test"
--2. Type ./Test 
module Main where
import Data.Bool
import Data.Char
import Data.List
import Data.Maybe hiding (fromMaybe)


cross :: [a] -> [a] -> [[a]]
cross s1 s2 = [[r, c] | r <- s1, c <- s2]

size :: Int
size = 9
root :: Int
root = 3
ints = [1..size]
dynamicLetter = ['A'..'Z']
dynamicNumber = "123456789"

rowBoxes, colBoxes :: [String]
rowBoxes = ["ABC", "DEF", "GHI"]
colBoxes = ["123", "456", "789"]

rows, cols :: String
rows = concat rowBoxes
cols = concat colBoxes

squares :: [String]
squares = cross rows cols
unitlist :: [[String]]
unitlist = [cross rows [c] | c <- cols]
        ++ [cross [r] cols | r <- rows]
        ++ [cross rs cs | rs <- rowBoxes, cs <- colBoxes]

units :: [(String, [[String]])]
units = [(s, filter (elem s) unitlist) | s <- squares]

peers :: [(String, [String])]
peers = map (\(s, u) -> (s, delete s (foldl union [] u))) units

type Board = [(String, [Int])]

allDigits :: [Int]
allDigits = [1, 2, 3, 4, 5, 6, 7, 8, 9]
infAllDigits = repeat allDigits
emptyBoard = zip squares infAllDigits

parseSquare :: (String, Char) -> Board -> Maybe Board
parseSquare (s, x) values
  | x == '.' || x == '0' = return values
  | isDigit x = assign (digitToInt x) s values
  | otherwise = fail "not a valid grid"

parseBoard :: String -> Maybe Board
parseBoard = foldr ((=<<) . parseSquare) (Just emptyBoard) . zip squares

main :: IO ()
main = do
  putStrLn "Please enter filename.txt"
  fileName <- getLine
  file <- readFile fileName
  let boards = concatInnerRows $ filterFile (lines file)
  playSudoku boards

--Function for continiously running the program
playSudoku :: [String]  -> IO ()
playSudoku [] = putStrLn "File is exhausted"
playSudoku (b:boards) = do
  putStrLn "What do you want to do for the current Sudoku \n [1] solve all remaining Sudokus \n [2] Solve this Sudoku automatically \n [3] Solve this sudoku manually "
  operation <- getLine

  case operation of
    "1" -> do
      mapM_ (\x -> putStrLn (solveSudoku x)) (b:boards)
      playSudoku boards
    "2" -> do
      printSudoku (solveSudoku b)
      playSudoku boards
    "3" -> do
      printSudoku b
      putStrLn("What square do you want to change? (ex. A1)")
      tempString <- getLine
      putStrLn("What do you want to change it to (1-9)?")
      tempInt <- getLine
      let tempBoard = parseBoardToString (setValueOfOneElement (read tempInt) tempString (parseStringToBoard b))
      if verifySudoku tempBoard then do
        printSudoku tempBoard
        playSudoku (tempBoard:boards)
      else do
        putStrLn "The given input number makes the sudoku unsolvable, pls try with a new number!"
        playSudoku (b:boards)


    _ -> do
      putStrLn "Invalid input"
      playSudoku (b:boards)



--Converts the function from a board [(String, Int)] to String
parseBoardToString :: [(String, Int)] -> String
parseBoardToString board = concatMap show [snd x | x <- board]

-- Removes equal String 
filterFile :: [String] -> [[String]]
filterFile [] = []
filterFile fileLines = take size fileLines : filterFile (drop (size + 1) fileLines)

-- Concatenates the values in the inner lists together
concatInnerRows :: [[String]] -> [String]
concatInnerRows xs = [concat x | x <- xs]

--print the given integerstring in sudoku form
printSudoku :: String -> IO ()
printSudoku board  =
    if board == "" then
        return ()
    else
        do
        putStrLn (take size board)
        printSudoku (drop size board)

--Prints the board but the units with impossible values are replaced with a "-"
prettyPrint :: String -> IO ()
prettyPrint board = do
  let zipped = zip board (map snd ((validBoardNumbers . parseStringToBoard) board))
  let units =  [(x, validUnit x (validBoardNumbers (parseStringToBoard board))) | x <- unitList]
  if validUnits (parseStringToBoard board) then
    printSudoku (map (\x -> if null (snd x) then 'X' else fst x) zipped)
  else
    printList (map (\x -> if not (snd x) then " Fel i unit " ++ concatStringList (fst x) else "") units)
    --map (\x -> if not validUnit x then "Fel i unit" + fst x else "Inte fel i unit" + x) parseStringToBoard board

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

dynamicCross :: [String]
dynamicCross = cross (take size dynamicLetter) (take size dynamicNumber)
-- Replaces all points in a list of chars with 0:s by mapping through the list and, if the char currently mapped through
-- is a . then 0 else returns the element

replacePointsWithZeros :: String -> String
replacePointsWithZeros = map (\c -> if c=='.' then '0'; else c)

--Takes a binary sequence and maps every square to its corresponding binary value 
parseStringToBoard :: String -> [(String, Int)]
parseStringToBoard a = zip dynamicCross (map digitToInt (replacePointsWithZeros a))

--Divides a String into equal elements of root 
boxCreator :: [a] -> [[a]]
boxCreator [] = []
boxCreator xs = take root xs : boxCreator (drop root xs)

--Creates every unit in a 4x4 sudoku by using cross product on a row with every col, col with every row 
--and cross the letterboxes with the numberboxes to create a list of lists
--if desiring a 9x9 or bigger sudoku, this function either needs to take input or manually change rows, cols and numberboxes 
unitList :: [[String]]
unitList = [cross (take size dynamicLetter) [col] | col <- take size dynamicNumber] ++
            [cross [row] (take size dynamicNumber) | row <- take size dynamicLetter] ++
            [cross xs ys  | xs  <- boxCreator (take size dynamicLetter), ys <-boxCreator (take size dynamicNumber)]

--Finds all units in unitlist containging the inputstring
filterUnitList :: String -> [[String]]
filterUnitList x = [u | u <- unitList, x  `elem` u]


-- sorts the list, then returns the list with each element grouped with itself then takes head of each group i.e removes duplicates
removeDuplicates :: (Ord a) => [a] -> [a]
removeDuplicates [] = []
removeDuplicates (x:xs)
  |  x `elem` xs = removeDuplicates xs
  | otherwise = x:removeDuplicates xs

--Returns all other peers for input square
getPeers :: String -> [String]
getPeers x = fromMaybe ["0"] (lookup x peers)

validSquare :: (String, Int) -> [(String, Int)] -> Bool
validSquare (_, 0) _ = True
validSquare sq b  = sqrValue `notElem` peerValues
  where
    peerValues = lookups ((getPeers . fst ) sq) b
    sqrValue = snd sq

--Returns all possible values of a square for with input square and board
validSquareNumbers1 :: (String, Int) -> [(String, Int)] -> (String, [Int])
validSquareNumbers1 x ys = (fst x, reduceList ints (lookups (getPeers (fst x)) ys))

validSquareNumbers :: (String,Int) -> [(String,Int)] -> (String,[Int])
validSquareNumbers (sq,v) b
  | v == 0               = (sq, possibleValues)
  | validSquare (sq,v) b = (sq,[v])
  | otherwise            = (sq,[])
  where
    possibleValues = foldl reduceList [1..size] peerValues
    peerValues = [lookups unit b | unit <- filterUnitList sq]

--Returns all possible values for all squares with input board
validBoardNumbers :: [(String, Int)] -> [(String, [Int])]
validBoardNumbers xs =  [validSquareNumbers x xs | x <- xs]

  --if length snd validSquareNumbers  == 1 then [if x == snd validSquareNumbers | x <- getPeers snd validSquareNumbers parseBoard]

-- Checks if all squares in a unit can contain a unique integer
validUnit :: [String] -> [(String, [Int])] -> Bool
validUnit xs ys = if (and [ int `elem` concat (lookups xs ys) | int <- ints]) then
  not ( filter (\x -> length x == 1) (lookups xs ys) > removeDuplicates (filter (\x -> length x == 1) (lookups xs ys)) )
  else False

-- calculates validity for input board
validUnits :: [(String, Int)] -> Bool
validUnits y = and [validUnit x (validBoardNumbers y) | x <- unitList]

--Given input String (i.e 12....34.......) returns if it is possible to solve sudoku
verifySudoku :: String -> Bool
verifySudoku x = validUnits (parseStringToBoard x)

--For a input 2-tuple, applies function 1 to first value and function2 too second value
--map2 :: (a -> c) -> (b -> d) -> (a, b) -> (c, d)
map2 :: (a1 -> a2) -> (t -> b) -> (a1, t) -> (a2, b)
map2 a b cd = (a (fst cd), b (snd cd))

--applies first function if sencond function returns true
mapIf :: (a -> a) -> (a -> Bool) -> [a] -> [a]
mapIf func boolfunc xs = [ if boolfunc x then func x else x | x <- xs ]

--returns a if a /= Nothing or if both a and b /= Nothing else returns b 
maybeOr :: Maybe a -> Maybe a -> Maybe a
maybeOr a b = case a of
  Nothing -> case b of
    Nothing -> Nothing
    Just b' -> b
  Just a' -> a

--Takes first non-Nothing value from list
firstJust :: Eq a => [Maybe a] -> Maybe a
firstJust [] = Nothing
firstJust [x] = x
firstJust (x:xs) = if isJust x then x else firstJust xs

--Returns the list of the first occurence of the second value of a two-tuple if the first value is equal too input
lookupList :: Eq a => a -> [(a, [b])] -> [b]
lookupList _ [] = []
lookupList a (x:xs)
  |a == fst x = snd x
  |otherwise = lookupList a xs

--Applies function to input if input /= Nothing
maybeBind :: Maybe a -> (a -> Maybe b) -> Maybe b
maybeBind a func = case a of
  Nothing -> Nothing
  Just a -> func a

--Replaces first occurence of first parameter with second parameter  
tryReplace :: Eq a => a -> a -> [a] -> Maybe [a]
tryReplace _ _ [] = Nothing
tryReplace y z (x:xs)
  | x == y = Just (z:xs)
  | otherwise = fmap (x:) $ tryReplace y z xs

--tryReplace but for multiple list of first and second parameter
recursiveReplacement :: Eq a => [a] -> [a] -> [a] -> Maybe [a]
recursiveReplacement _ _ [] = Nothing
recursiveReplacement [] _ zs = Just zs
recursiveReplacement _ [] zs =  Just zs
recursiveReplacement (x:xs) (y:ys) zs = recursiveReplacement xs ys zs `maybeBind` tryReplace x y

--Sets a value of a square
setValue :: Int -> String -> Board -> Board
setValue int str board = mapIf (\x -> (fst x, [int])) (\x -> fst x == str) board

--Takes a board in datastructure [(String, Int)] and returns new value of same board but new Int
setValueOfOneElement :: Int -> String -> [(String, Int)] -> [(String, Int)]
setValueOfOneElement int str = mapIf (\x -> (fst x, int)) (\x -> fst x == str)

--Eliminates a value of a square
eliminateValue :: Int -> String -> Board -> Board
eliminateValue int str board = mapIf (\x -> (fst x, delete int (snd x))) (\x -> fst x == str) board

--Checks for edge-cases for elimination
eliminate :: Int -> String -> Board -> Maybe Board
eliminate int str board
  | lookupList str board == [int] = Nothing
  | int `notElem` lookupList str board = Just board
  | otherwise = Just(eliminateValue int str board)

--Help function too remove edge cases in eliminate on multiple squares
assign' :: Int -> [String] -> Maybe Board -> Maybe Board
assign' _ [] board = board
assign' _ _ Nothing = Nothing
assign' int (x:xs) board = assign' int xs (eliminate int x (fromJust board))

--Sets value for a square and reomves this value from peers
assign ::  Int -> String -> Board -> Maybe Board
assign int str board
  | isNothing new_board = Nothing
  | otherwise = Just (setValue int str (fromJust new_board))
  where new_board = assign' int (lookupList str peers) (Just board)

--helpfunction to solve sudoku recursively by calling assign on every possible value on every square in order
solveSudoku' :: [String] -> Board -> Maybe Board
solveSudoku' [] board = Just board
solveSudoku' (s:strs) board =  firstJust (map(\x -> maybeBind (assign x s board) (solveSudoku' strs)) (lookupList s board))

--Parses input and Solves sudoku
solveSudoku :: String -> String
solveSudoku str = 
  if length (concatMap show [head (snd x)| x <-board]) == 0 then
    "Unsolvable" 
  else 
    concatMap show [head (snd x)| x <-board] -- ++ "\n"
  where board = removeMaybe (parseBoard str `maybeBind` solveSudoku' squares)

--Removes maybe from a board if the board is Just
removeMaybe :: Maybe Board -> Board
removeMaybe Nothing = []
removeMaybe (Just board) = board

