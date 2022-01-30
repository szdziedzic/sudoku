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
isValidBoard sudoku = length sudoku == 9 && all isValidItemArr sudoku && all isValidItemArr (columns sudoku [])

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

columns :: Sudoku -> [Row] -> [Row]
columns sudoku cols = if length (head sudoku) <= 1 then column:cols else columns nextIterBoard (column:cols)
    where
        nextIterBoard = map (\(r:rs) -> rs) sudoku
        column = map (\(r:rs) -> r) sudoku

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


loadSudoku :: Int -> Sudoku -> IO ()
loadSudoku 0 sudoku = do
    putStrLn ""
    putStrLn "Board loaded successfully!"
    if isValidBoard sudoku then solveAndPrint sudoku else putStrLn "Invalid board"
loadSudoku 9 sudoku = do
    putStrLn "Please enter the board below:"
    row <- getLine
    loadSudoku 8 (sudoku ++ [stringToRow row])
loadSudoku n sudoku = do
    row <- getLine
    loadSudoku (n - 1) (sudoku ++ [stringToRow row])

main :: IO ()
main = loadSudoku 9 []
