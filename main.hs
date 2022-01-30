-- Informacje ogolne:
-- Program wczytuje sudoku rzad po rzedzie
-- Przykladowy format wejscia
-- ...26.7.1
-- 68..7..9.
-- 19...45..
-- 82.1...4.
-- ..46.29..
-- .5...3.28
-- ..93...74
-- .4..5..36
-- 7.3.18...
-- puste miejsca oznaczane sa .
-- Podczas i po wczytaniu plansza wejsciowa jest walidowana
-- to znaczy sprawdzamy czy rzedy, kolumny, boxy sa dlugosci 9
-- czy wartosci sa z zakresu od 1 do 9
-- czy zadna wartosc nie wystepuje wielokrotnie w zakresie wiersza, kolumny, boxa
-- Jesli plansza jest prawidlowa przystepujemy do rozwiazania
-- Jesli jest nie prawidlowa wyswietlany jest komunikat o bledzie
-- Przykladowe dane wejsciowe podane sa w folderze exampleInput (zrodlo danych https://sandiway.arizona.edu/sudoku/examples.html)
-- Zastosowany algorytm to backtracking (https://www.geeksforgeeks.org/sudoku-backtracking-7/, )




-- pojedyncza komorka planszy
data Item = Value Int | Empty deriving Show
-- rzad
type Row = [Item]
-- kolumna
type Col = [Item]
-- kwadrat 3x3
type Box = [Item]
-- cala plansza sudoku
type Sudoku = [Row]

-- konweruje pojedynczy znak na Item
-- znaki od '1' do '9' sa konwertowane na Value od 1 do 9
-- a pozostale znaki sa konwertowane na Empty
-- w szczegolnosci '.' jest konwertowana na Empty
charToItem :: Char -> Item
charToItem c = if c `elem` ['1'..'9'] then Value (read [c] :: Int) else Empty

-- konwertuje caly wczytany jako string wiersz na Row
stringToRow :: String -> Row
stringToRow = map charToItem

-- sprawdza czy tablica item (np wiersz, kolumna, box) zawiera
-- tylko wartosci calkowite z przedzialu od [1, 9]
validValues :: [Item] -> Bool
validValues [] = True
validValues (Empty:is) = validValues is
validValues ((Value v):is) = not (v <= 0 || v > 9) && validValues is

-- sprawdza czy podana tablica item, w tym przypadku argumentem zawsze bedzie rzad, kolumna lub box
-- jest prawidłowa w konekście sudoku to znaczy:
-- jest dlugosci 9 (rzad ma miec dlugosc 9, tak samo kolumna, box przechowujemy jako tablice dl 9 wiec rowniez musi spelniac ten warunek)
-- zawiera tylko wartosci calkowite z przedzialu od 1 do 9
-- kazda z liczb od 1 do 9 wystepuje co najwyzej raz w tablicy
isValidItemArr :: [Item] -> Bool
isValidItemArr r = length r == 9 && maxRowOccurances r 0 0 0 0 0 0 0 0 0 <= 1 && validValues r

-- wylicza maksymalna liczbe wystapien liczb od 1 do 9 w tablicy item
maxRowOccurances :: [Item] -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int
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

-- sprawdza czy plansza jest prawidlowa plansza do sudoku
-- w szczegolnosci czy sudoku ma 9 rzedow 
-- oraz czy kazdy rzad, kolumna i box jest prawidlowy
isValidBoard :: Sudoku -> Bool
isValidBoard sudoku = length sudoku == 9 && all isValidItemArr sudoku && all isValidItemArr (columns sudoku []) && all isValidItemArr (boxes sudoku [])

-- wyodrebnia kolumny z planszy Sudoku zdefiniowanej jako lista wierszy
columns :: Sudoku -> [Col] -> [Col]
columns sudoku cols = if length (head sudoku) <= 1 then cols ++ [column] else columns nextIterBoard (cols ++ [column])
    where
        nextIterBoard = map (\(r:rs) -> rs) sudoku
        column = map (\(r:rs) -> r) sudoku

-- sklada 3 kolejne 9 elementowe wiersze w tablice 3 boxow
rowsToBoxes :: Row -> Row -> Row -> [Box]
rowsToBoxes [i11, i12, i13, i14, i15, i16, i17, i18, i19] [i21, i22, i23, i24, i25, i26, i27, i28, i29] [i31, i32, i33, i34, i35, i36, i37, i38, i39] =
    [[i11, i12, i13, i21, i22, i23, i31, i32, i33], [i14, i15, i16, i24, i25, i26, i34, i35, i36], [i17, i18, i19, i27, i28, i29, i37, i38, i39]]
rowsToBoxes r1 r2 r3 = []

-- wyodrebnia boxy z planszy Sudoku
boxes :: Sudoku -> [[Item]] -> [[Item]]
boxes [] bxs = bxs
boxes (r1:r2:r3:rs) bxs = boxes rs (bxs ++ rowsToBoxes r1 r2 r3)
boxes s bxs = []

-- metoda do wypisywania wiersza sudoku
-- jesli wartosc to Empty wypisujemy '.' jesli Value v to wypisujemy v
-- po kazdym rzedzie wypisujemy znak nowej lini
printRow :: Row -> IO ()
printRow [] = do
    putStrLn ""
printRow (Empty:is) = do
    putChar '.'
    printRow is
printRow ((Value v):is) = do
    putStr (show v)
    printRow is

-- metoda do wypisywania sudoku wiersz po wierszu
printSudoku :: Sudoku -> IO ()
printSudoku [] = return ()
printSudoku (r:rs) = do
    printRow r
    printSudoku rs

-- metoda przyjmuje prawidlowa plansze sudoku i probuje ja rozwiazac
-- rozwiazanie zostaje wypisane na ekran
solveAndPrint :: Sudoku -> IO ()
solveAndPrint sudoku = do
    putStrLn ""
    putStrLn "Board is valid! Solving..."
    putStrLn ""
    if null solution then  putStrLn "No solutions exist for this board!" else putStrLn "Solved successfully!"
    putStrLn ""
    printSudoku solution
    where solution = solve sudoku

-- czy dany Item jest Wartoscia (wtedy True) czy moze jest Empty (wtedy False)
isValueItem :: Item -> Bool
isValueItem Empty = False
isValueItem (Value _) = True

-- czy dany wiersz jest rozwiazana, w tym kontekscie czy nie ma w nim pustych miejsc
isSolvedRow :: Row -> Bool
isSolvedRow = all isValueItem

-- czy sudoku jest rozwiazane
-- tj czy kazdy wiersz w nim jest rozwiazany
-- ta metoda nie zajmuje sie weryfikacja poprawnosci planszy
isSolved :: Sudoku -> Bool
isSolved = all isSolvedRow

-- czy wiersz ma puste komorki
hasEmptyCells :: Row -> Bool
hasEmptyCells = not.isSolvedRow

-- tablica wartosci z tablicy Item (rzad, kolumna, box)
-- bierze wszystkie wartosci z tablicy i wklada je do listy int
-- przykladowo z [Value 1, Value 2, Empty, Value 5, Empty] otrzymamy [1, 2, 5]
itemArrValues :: [Item] -> [Int]
itemArrValues [] = []
itemArrValues (Empty:is) = itemArrValues is
itemArrValues ((Value v):is) = v:itemArrValues is

-- jako input bierze rzad, kolumne i boxa do ktorych nalezy konkretny Item
-- sprawdza jakie liczby bylyby legalne do wstawienia w pusty Item
-- biorac pod uwage ogarniczenia obowiazujace box, kolumne i rzad
-- tj jakie nie wystepuja ani w rzedzie ani w boxie ani w kolumnie
numsLegalInItemArr :: Row -> Col -> Box -> [Int]
numsLegalInItemArr row col box = filter (\num -> num `notElem` rowVals && num `notElem` colVals && num `notElem` boxVals) [1..9]
    where 
        rowVals = itemArrValues row
        colVals = itemArrValues col
        boxVals = itemArrValues box

-- funkcja przechodzi przez rzad do znalezenia w nim pustego miejsca
-- jesli puste miejsce jest znaleznione
-- funkcja na podstawie odpowiedniego indeksu rzedu i kolumny
-- wyodrebnia box i kolumne do jakiego nalezy dana wartosc z rzedu
-- a nastepnie wyznacza legalne wartosci ktore moga sie znalezc w tym konkretnym pustym miejscu
-- na podstawie istniejacych wartosci z rzedu, kolumny i boxa
-- funkcja zwraca tablice wersji danego rzedu, po jednej dla kazdej mozliwej wartosci, z zastapionym pustym miejscem przez dana wartosc
-- przykladowo (bardzo uproszczony i nieprawidlowy nawet wymiarowo przyklad dla pokazania idei):
-- zalozmy ze wiersz to [Empty, Value 1, Value 7]
-- kolumna to [Empty, Value 5, Value 4]
-- box to [Empty, Value 1, Value 7, Value 5, Value 2, Value 6]
-- wtedy mozliwe legalne wartosci to [3, 8, 9]
-- funkcja zwroci wtedy 3 wersje wiersza w liscie [[Value 3, Value 1, Value 7], [Value 8, Value 1, Value 7], [Value 9, Value 1, Value 7]]
-- dla kazdej mozliwej wartosci wsadzonej w miejsce pierwszego wolnego Empty
replaceFirstEmptyInRowLegalCombinations :: Row -> [Item] -> Int -> [Col] -> [Box] -> Int -> Row -> [Row]
replaceFirstEmptyInRowLegalCombinations [] prevItems colInd cols bxs rowInd r = []
replaceFirstEmptyInRowLegalCombinations (Empty:is) prevItems colInd cols bxs rowInd r = map (\val -> prevItems ++ Value val : is) legalValues
    where
        col = last(take colInd cols)
        box = last(take (((rowInd - 1) `div` 3) * 3 + ((colInd - 1) `div` 3) + 1) bxs)
        legalValues = numsLegalInItemArr r col box
replaceFirstEmptyInRowLegalCombinations (i:is) prevItems colInd cols bxs rowInd r = replaceFirstEmptyInRowLegalCombinations is (prevItems ++ [i]) (colInd + 1) cols bxs rowInd r

-- podobnie jak powyzsza funkcja dla wiersza, tak ta funckja zwraca 
-- liste wszystkich mozliwych legalnych Sudoku z zapelnionym pierwszym pustym miejscem idac z gory na dol od lewej do prawej
-- metoda w istotny sposob wykorzystuje powyzsza funckje zweracajaca wszytskie legalne wersje danego wiersza
-- przykladowo (bardzo uproszczony i nieprawidlowy nawet wymiarowo przyklad dla pokazania idei):
-- niech Sudoku [[Value 1, Value 3, Empty], [Value 2, Value4, Value 7]]
-- zalozmy ze powyzsza funkcja zwrocila 2 wersje rzedu [[Value 1, Value 3, Value 5], [Value 1, Value 3, Value 9]]
-- wtedy funckja zwroci 2 warianty Sudoku po jednym dla kazdego wariantu wiersza
-- [[[Value 1, Value 3, Value 5], [Value 2, Value4, Value 7]], [[Value 1, Value 3, Value 9], [Value 2, Value4, Value 7]]]
-- zwrocne plansze zawsze beda legalne dzieki sprawdzeniom dokonwanym w funkcji dotyczacej rzedu
replaceFirstEmptyLegalCombinations :: Sudoku -> [Row] -> Int -> [Col] -> [Box] -> [Sudoku]
replaceFirstEmptyLegalCombinations [] prevRows rowInd cols bxs = []
replaceFirstEmptyLegalCombinations (r:rs) prevRows rowInd cols bxs = if hasEmptyCells r then map (\r -> prevRows ++ r : rs) (replaceFirstEmptyInRowLegalCombinations r [] 1 cols bxs rowInd r) else replaceFirstEmptyLegalCombinations rs (prevRows ++ [r]) (rowInd + 1) cols bxs

-- funckja ta jako input bierze liste mozliwych legalnych rozwiazan sudoku
-- po kolei probuje rekurencyjnie rozwiazac kolejne legalne propozycje
-- jesli ktoras opcja jest rozwiazaniem jest ona zwracana
-- jesli nie zwracana jest pusta tablica
tryNewLegalCombination :: [Sudoku] -> Sudoku
tryNewLegalCombination [] = []
tryNewLegalCombination (c:cs) = if null sol then tryNewLegalCombination cs else sol
    where
        sol = solve c

-- funkcja rozwiazujaca sudoku
-- jako input przyjmuje legalna plansze do sudoku
-- jesli plansza jest rozwiazana to jest ma wyplenione wszytskie wartosci to jest ona zwracana
-- w przeciwnym wypadku generujemy wszystkie mozliwe legalne plansze z zaplenionym kolejnym pustym miejscem
-- i wykonujemy na nich funkcje tryNewLegalCombination
solve :: Sudoku -> Sudoku
solve sudoku | isSolved sudoku = sudoku
             | otherwise = tryNewLegalCombination allNewCombinations
             where
                allNewCombinations = replaceFirstEmptyLegalCombinations sudoku [] 1 cols bxs
                cols = columns sudoku []
                bxs = boxes sudoku []

-- wczytujemy sudoku rzad po rzedzie sprawdzajac czy rzedy maja odpowiednia dlugosc
-- po wczytaniu calej planszy sprawdzamy czy jest ona prawidlowa
-- jesli tak to przystepujemy do rozwiazania
-- jesli nie to wysiwetlamy o tym informacje 
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
