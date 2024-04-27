--8 Damen Problem
--Finde mittels einer haskell funktion eine lösung für das problem
--Überlege dir daür eine passen Darstellung der position der 8 Damen
--Generiert eine Matrix
generate :: Int -> Int -> [[Int]] 
generate x y = replicate x (replicate y 0)

updateGrid :: [[Int]] -> [[Int]]
updateGrid matrix = updateGridHelper matrix 0 0

--Geht durch die matrix und überprüft ob eine dame platziert wird
updateGridHelper :: [[Int]] -> Int -> Int -> [[Int]]
updateGridHelper matrix currentRow currentCol
    | currentRow >= 7 = updateGridHelper (updateMatrix matrix currentRow currentCol 1 (checkIsSafe matrix currentRow currentCol)) 0 (currentCol + 1)
    | currentCol >= 8 = matrix
    | otherwise = updateGridHelper (updateMatrix matrix currentRow currentCol 1 (checkIsSafe matrix currentRow currentCol)) (currentRow + 1) currentCol

--Ändert an einer position der matrix ein neuen int ein
updateMatrix :: [[Int]] -> Int -> Int -> Int -> Bool -> [[Int]]
updateMatrix matrix _ _ _ False = matrix
updateMatrix matrix col row newValue _ =
    let (before, currentRow:after) = splitAt row matrix
        newCurrentRow = take col currentRow ++ [newValue] ++ drop (col + 1) currentRow
    in before ++ [newCurrentRow] ++ after

--Code zum überprüfen ob eine dame möglich ist
checkIsSafe :: [[Int]] -> Int -> Int -> Bool
checkIsSafe matrix col row = checkIfQueenVectorSafe (getAllQueens matrix) (col, row)

--Zieht eine liste von tuples die die positionen der damen enthalten
getAllQueens :: [[Int]]  -> [(Int, Int)]
getAllQueens matrix = [(i, j) | (rowIndex, row) <- zip [0..] matrix, (colIndex, val) <- zip [0..] row, val == 1, let i = colIndex, let j = rowIndex]

--Der tatsächliche code zum überprüfen ob die position möglich ist
checkIfQueenVectorSafe :: [(Int, Int)] -> (Int, Int) -> Bool
checkIfQueenVectorSafe [] _ = True
checkIfQueenVectorSafe queenPositions position
    | abs (fst (vector (head queenPositions) position)) /= abs (snd (vector (head queenPositions) position)) &&
    abs (fst (vector (head queenPositions) position)) /= 0 && abs (snd (vector (head queenPositions) position)) /= 0 = checkIfQueenVectorSafe (tail queenPositions) position
    | otherwise = False

vector :: (Int, Int) -> (Int, Int) -> (Int, Int)
vector (x1, y1) (x2, y2) = (x2 - x1, y2 - y1)

--In der console "updateGrid (generate 8 8)" eingeben um zu testen
--Eine 1 in der matrix representiert eine dame und die nullen representieren leere felder
