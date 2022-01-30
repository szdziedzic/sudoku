data Item = Value Int | Empty deriving Show
type Row = [Item]
type Sudoku = [Row]

charToItem :: Char -> Item
charToItem c = if c `elem` ['1'..'9'] then Value (read [c] :: Int) else Empty

stringToRow :: String -> Row
stringToRow = map charToItem

stringsToSudoku :: [String] -> Sudoku
stringsToSudoku = map stringToRow

maximumOccurances :: [(Int, Int)] -> Int
maximumOccurances [] = 0
maximumOccurances (o:os) = if snd o > m then snd o else m
    where m =  maximumOccurances os

validValues :: [Item] -> Bool
validValues [] = True
validValues (Empty:is) = validValues is
validValues ((Value v):is) = not (v <= 0 || v > 9) && validValues is

isValidItemArr :: [Item] -> Bool
isValidItemArr r = length r == 9 && maximumOccurances rowOccurances <= 1 && validValues r
    where rowOccurances = numbersOccurences r

isValidBoard :: Sudoku -> Bool
isValidBoard sudoku = length sudoku == 9 && all isValidItemArr sudoku && all isValidItemArr (columns sudoku []) && all isValidItemArr (boxes sudoku [])

emptyNumberOccurences :: [(Int, Int)]
emptyNumberOccurences = [(1, 0), (2, 0), (3, 0), (4, 0), (5, 0), (6, 0), (7, 0), (8, 0), (9, 0)]

itemValue :: Item -> Int
itemValue Empty = 0
itemValue (Value v) = v

updateOccurances :: [(Int, Int)] -> Item -> [(Int, Int)]
updateOccurances occ item = if null filteredArr then occ else (fst(head filteredArr), snd(head filteredArr) + 1) : filter (\(n, q) -> n /= itemValue item) occ
    where
        filteredArr = filter (\(n, q) -> n == itemValue item) occ

numbersOccurences :: [Item] -> [(Int, Int)]
numbersOccurences = foldl updateOccurances emptyNumberOccurences

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
    putStrLn ""
    putStrLn "Board is valid! Solving..."
    putStrLn ""
    printSudoku sudoku

isValueItem :: Item -> Bool
isValueItem Empty = False
isValueItem (Value _) = True

isSolvedRow :: Row -> Bool
isSolvedRow = all isValueItem

isSolved :: Sudoku -> Bool 
isSolved = all isSolvedRow

loadAndSolveSudoku :: Int -> Sudoku -> IO ()
loadAndSolveSudoku 0 sudoku = do
    putStrLn ""
    putStrLn "Board loaded successfully!"
    if isValidBoard sudoku then solveAndPrint sudoku else putStrLn "Invalid board"
loadAndSolveSudoku 9 sudoku = do
    putStrLn "Please enter the board below:"
    row <- getLine
    loadAndSolveSudoku 8 (sudoku ++ [stringToRow row])
loadAndSolveSudoku n sudoku = do
    row <- getLine
    loadAndSolveSudoku (n - 1) (sudoku ++ [stringToRow row])

main :: IO ()
main = loadAndSolveSudoku 9 []
