data Item = Value Int | Empty deriving Show
type Row = [Item]
type Col = [Item]
type Box = [Item]
type Sudoku = [Row]

charToItem :: Char -> Item
charToItem c = if c `elem` ['1'..'9'] then Value (read [c] :: Int) else Empty

stringToRow :: String -> Row
stringToRow = map charToItem

stringsToSudoku :: [String] -> Sudoku
stringsToSudoku = map stringToRow

validValues :: [Item] -> Bool
validValues [] = True
validValues (Empty:is) = validValues is
validValues ((Value v):is) = not (v <= 0 || v > 9) && validValues is

isValidItemArr :: [Item] -> Bool
isValidItemArr r = length r == 9 && maxRowOccurances r 0 0 0 0 0 0 0 0 0 <= 1 && validValues r

maxRowOccurances :: Row -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int
maxRowOccurances [] n1 n2 n3 n4 n5 n6 n7 n8 n9 = maximum [n1, n2, n3, n4, n5, n6, n7, n8, n9]
maxRowOccurances ((Value v):is) n1 n2 n3 n4 n5 n6 n7 n8 n9
    | v == 1 = maxRowOccurances is (n1 + 1) n2 n3 n4 n5 n6 n7 n8 n9
    | v == 2 = maxRowOccurances is n1 (n2 + 1) n3 n4 n5 n6 n7 n8 n9
    | v == 3 = maxRowOccurances is n1 n2 (n3 + 1) n4 n5 n6 n7 n8 n9
    | v == 4 = maxRowOccurances is n1 n2 n3 (n4 + 1) n5 n6 n7 n8 n9
    | v == 5 = maxRowOccurances is n1 n2 n3 n4 (n5 + 1) n6 n7 n8 n9
    | v == 6 = maxRowOccurances is n1 n2 n3 n4 n5 (n6 + 1) n7 n8 n9
    | v == 7 = maxRowOccurances is n1 n2 n3 n4 n5 n6 (n7 + 1) n8 n9
    | v == 8 = maxRowOccurances is n1 n2 n3 n4 n5 n6 n7 (n8 + 1) n9
    | otherwise  = maxRowOccurances is n1 n2 n3 n4 n5 n6 n7 n8 (n9 + 1)
maxRowOccurances (Empty:is) n1 n2 n3 n4 n5 n6 n7 n8 n9 = maxRowOccurances is n1 n2 n3 n4 n5 n6 n7 n8 n9

isValidBoard :: Sudoku -> Bool
isValidBoard sudoku = length sudoku == 9 && all isValidItemArr sudoku && all isValidItemArr (columns sudoku []) && all isValidItemArr (boxes sudoku [])

columns :: Sudoku -> [[Item]] -> [[Item]]
columns sudoku cols = if length (head sudoku) <= 1 then cols ++ [column] else columns nextIterBoard (cols ++ [column])
    where
        nextIterBoard = map (\(r:rs) -> rs) sudoku
        column = map (\(r:rs) -> r) sudoku

rowsToBoxes :: Row -> Row -> Row -> [[Item]]
rowsToBoxes [i11, i12, i13, i14, i15, i16, i17, i18, i19] [i21, i22, i23, i24, i25, i26, i27, i28, i29] [i31, i32, i33, i34, i35, i36, i37, i38, i39] =
    [[i11, i12, i13, i21, i22, i23, i31, i32, i33], [i14, i15, i16, i24, i25, i26, i34, i35, i36], [i17, i18, i19, i27, i28, i29, i37, i38, i39]]
rowsToBoxes r1 r2 r3 = []

boxes :: Sudoku -> [[Item]] -> [[Item]]
boxes [] bxs = bxs
boxes (r1:r2:r3:rs) bxs = boxes rs (bxs ++ rowsToBoxes r1 r2 r3)
boxes s bxs = []

printRow :: Row -> IO ()
printRow [] = do
    putStrLn ""
printRow (Empty:is) = do
    putChar '.'
    printRow is
printRow ((Value v):is) = do
    putStr (show v)
    printRow is

printSudoku :: Sudoku -> IO ()
printSudoku [] = return ()
printSudoku (r:rs) = do
    printRow r
    printSudoku rs

solveAndPrint :: Sudoku -> IO ()
solveAndPrint sudoku = do
    print sudoku
    putStrLn ""
    putStrLn "Board is valid! Solving..."
    putStrLn ""
    if null solution then  putStrLn "No solutions exist for this board!" else putStrLn "Solved successfully!"
    putStrLn ""
    printSudoku solution
    where solution = solve sudoku

isValueItem :: Item -> Bool
isValueItem Empty = False
isValueItem (Value _) = True

isSolvedRow :: Row -> Bool
isSolvedRow = all isValueItem

isSolved :: Sudoku -> Bool
isSolved = all isSolvedRow

hasEmptyCells :: Row -> Bool
hasEmptyCells = foldr ((||) . not . isValueItem) False

itemArrValues :: [Item] -> [Int]
itemArrValues [] = []
itemArrValues (Empty:is) = itemArrValues is
itemArrValues ((Value v):is) = v:itemArrValues is

numsLegalInItemArr :: Row -> Col -> Box -> [Int]
numsLegalInItemArr row col box = filter (\num -> num `notElem` rowVals && num `notElem` colVals && num `notElem` boxVals) [1..9]
    where 
        rowVals = itemArrValues row
        colVals = itemArrValues col
        boxVals = itemArrValues box

replaceFirstEmptyInRowLegalCombinations :: Row -> [Item] -> Int -> [Col] -> [Box] -> Int -> Row -> [Row]
replaceFirstEmptyInRowLegalCombinations [] prevItems colInd cols bxs rowInd r = []
replaceFirstEmptyInRowLegalCombinations (Empty:is) prevItems colInd cols bxs rowInd r = map (\val -> prevItems ++ Value val : is) legalValues
    where
        col = last(take colInd cols)
        box = last(take (((rowInd - 1) `div` 3) * 3 + ((colInd - 1) `div` 3) + 1) bxs)
        legalValues = numsLegalInItemArr r col box
replaceFirstEmptyInRowLegalCombinations (i:is) prevItems colInd cols bxs rowInd r = replaceFirstEmptyInRowLegalCombinations is (prevItems ++ [i]) (colInd + 1) cols bxs rowInd r

replaceFirstEmptyLegalCombinations :: Sudoku -> [Row] -> Int -> [Col] -> [Box] -> [Sudoku]
replaceFirstEmptyLegalCombinations [] prevRows rowInd cols bxs = []
replaceFirstEmptyLegalCombinations (r:rs) prevRows rowInd cols bxs = if hasEmptyCells r then map (\r -> prevRows ++ r : rs) (replaceFirstEmptyInRowLegalCombinations r [] 1 cols bxs rowInd r) else replaceFirstEmptyLegalCombinations rs (prevRows ++ [r]) (rowInd + 1) cols bxs

tryNewLegalCombination :: [Sudoku] -> Sudoku
tryNewLegalCombination [] = []
tryNewLegalCombination (c:cs) = if null sol then tryNewLegalCombination cs else sol
    where
        sol = solve c

solve :: Sudoku -> Sudoku
solve sudoku | isSolved sudoku = sudoku
             | otherwise = tryNewLegalCombination allNewCombinations
             where
                allNewCombinations = replaceFirstEmptyLegalCombinations sudoku [] 1 cols bxs
                cols = columns sudoku []
                bxs = boxes sudoku []

loadAndSolveSudoku :: Int -> Sudoku -> IO ()
loadAndSolveSudoku 0 sudoku = do
    putStrLn ""
    putStrLn "Board loaded successfully!"
    if isValidBoard sudoku then solveAndPrint sudoku else putStrLn "Invalid board"
loadAndSolveSudoku 9 sudoku = do
    putStrLn "Please enter the board below:"
    row <- getLine
    if length row == 9 then loadAndSolveSudoku 8 (sudoku ++ [stringToRow row]) else putStrLn "Invalid row length!"
loadAndSolveSudoku n sudoku = do
    row <- getLine
    if length row == 9 then loadAndSolveSudoku (n - 1) (sudoku ++ [stringToRow row]) else putStrLn "Invalid row length!"

main :: IO ()
main = loadAndSolveSudoku 9 []
