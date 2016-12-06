module Sudoku where

import Test.QuickCheck
import Data.Char
import System.Random
import Data.List
import Debug.Trace
import Data.Maybe


example =
  Sudoku
    [ [Just 3, Just 6, Nothing,Nothing,Just 7, Just 1, Just 2, Nothing,Nothing]
    , [Nothing,Just 5, Nothing,Nothing,Nothing,Nothing,Just 1, Just 8, Nothing]
    , [Nothing,Nothing,Just 9, Just 2, Nothing,Just 4, Just 7, Nothing,Nothing]
    , [Nothing,Nothing,Nothing,Nothing,Just 1, Just 3, Nothing,Just 2, Just 8]
    , [Just 4, Nothing,Nothing,Just 5, Nothing,Just 2, Nothing,Nothing,Just 9]
    , [Just 2, Just 7, Nothing,Just 4, Just 6, Nothing,Nothing,Nothing,Nothing]
    , [Nothing,Nothing,Just 5, Just 3, Nothing,Just 8, Just 9, Nothing,Nothing]
    , [Nothing,Just 8, Just 3, Nothing,Nothing,Nothing,Nothing,Just 6, Nothing]
    , [Nothing,Nothing,Just 7, Just 6, Just 9, Nothing,Nothing,Just 4, Just 3]
    ]
-------------------------------------------------------------------------

data Sudoku = Sudoku { rows :: [[Maybe Int]] }
 deriving ( Show, Eq )

-- allBlankSudoku is a sudoku with just blanks
allBlankSudoku :: Sudoku
allBlankSudoku = Sudoku [ [ Nothing | j <- [1..9] ] | i <- [1..9] ]

-- isSudoku sud checks if sud is really a valid representation of a sudoku
-- puzzle
isSudoku :: Sudoku -> Bool
isSudoku (Sudoku rows) = map length rows == [ 9 | i <- [1..9]]

-- isSolved sud checks if sud is already solved, i.e. there are no blanks
---isSolved :: Sudoku -> Bool
isSolved (Sudoku rows) = map (\row -> filter (\cell -> cell == Nothing) row) rows == [[] | i <- [1..9]];

-------------------------------------------------------------------------

-- printSudoku sud prints a representation of the sudoku sud on the screen
printSudoku :: Sudoku -> IO ()
printSudoku (Sudoku rows) = putStr (unlines (map (map $ maybe '.' intToDigit) rows))


-- readSudoku file reads from the file, and either delivers it, or stops
-- if the file did not contain a sudoku
readSudoku :: FilePath -> IO Sudoku
readSudoku fp = do
                file <- readFile fp
                return (Sudoku [[ if el == '.' then Nothing else Just ((ord el) - (ord '0'))
                  | el <- line] | line <- (lines file)])

-------------------------------------------------------------------------

-- cell generates an arbitrary cell in a Sudoku
cell :: Gen (Maybe Int)
cell = elements ([ Just x | x <- [1..9] ] ++ [ Nothing | i <- [1..100] ])



-- an instance for generating Arbitrary Sudokus
instance Arbitrary Sudoku where
  arbitrary =
    do rows <- sequence [ sequence [ cell | j <- [1..9] ] | i <- [1..9] ]
       return (Sudoku rows)

-------------------------------------------------------------------------

prop_Sudoku :: Sudoku -> Bool
prop_Sudoku sudoku = isSudoku sudoku

--- Assignment D ---
type Block = [Maybe Int]

-- D1
isOkayBlock :: Block -> Bool
isOkayBlock block = let list = filter (\a -> a /= Nothing) block in
    (length list) == (length (nub list))

-- D2

-- get the squares
get3x3 [] = []
get3x3 m  = get3x3' (take 3 m) ++ get3x3 (drop 3 m)
-- get the horizontal squares
get3x3' [[],[],[]] = []
get3x3' l          = concatMap (take 3) l ++ get3x3 (map (drop 3) l)


takeDrop ::[Block] -> [Block] -> [Block]
takeDrop [] list2 = list2
takeDrop (a:b:c:ds) list2 = takeDrop (ds) (list2 ++ [a++b++c])

getThreeByThree :: [Block] -> [Block]
getThreeByThree list = (takeDrop (get3x3 list []) [])
  where
    get3x3 :: [Block] -> [Block] -> [Block]
    get3x3 [[],[],[],[],[],[],[],[],[]] list2 = list2
    get3x3 list1 list2 = get3x3 (map (drop 3) list1) (list2 ++ (map (take 3) list1))


blocks :: Sudoku -> [Block]
blocks (Sudoku blocks) = blocks ++ (transpose blocks) ++ (getThreeByThree blocks)

-- D4
isOkay :: Sudoku -> Bool
isOkay sudoku = filter (\block -> not (isOkayBlock block)) (blocks sudoku) == []



--- Assignment E ---
--          row, col
type Pos = (Int,Int)

-- E1
blanks :: Sudoku -> [Pos]
blanks (Sudoku rows) = let a = map (\a -> elemIndices Nothing a) rows in
                        let b = zip [0..8] a in
                        concat (map (\(x,y) -> zip (replicate (length y) x) y) b)

-- E2
(!!=) :: [a] -> (Int,a) -> [a]
(x:xs) !!= (0, element) = [element] ++ xs
(x:xs) !!= (index, element) = [x] ++ (xs !!= (index-1, element))

-- E3
update :: Sudoku -> Pos -> Maybe Int -> Sudoku
update (Sudoku rows) (row, col) value = Sudoku (rows !!= (row, ((rows !! row)
  !!= (col, value))))

-- E4
candidates :: Sudoku -> Pos -> [Int]
candidates sudoku pos = filter (\a -> a /= 0) [ if isOkay (update sudoku pos (Just val)) then
   val else 0 | val <- [1..9] ]


--- Assignment F ---

sortGT (a1, b1) (a2, b2)
  | (length b1) <  (length b2) = LT
  | (length b1) >  (length b2) = GT
  | (length b1) == (length b2) = compare a1 a2


-- F1

solve' :: [Pos] -> Sudoku -> [Maybe Sudoku]
solve' [] old = [Just old]
solve' blank old = let cand = map (\pos -> (pos, (candidates old pos)) ) blank in
                    let (pos, list) = head ( sortBy sortGT cand ) in
                    if (length list) == 0 then [Nothing] else
                      concat (map (\value -> solve' (blanks (update old pos (Just value))) (update old pos (Just value)) ) list)

solve :: Sudoku -> Maybe Sudoku
solve sudoku = head (filter (\sud -> sud /= Nothing) (solve' (blanks sudoku) sudoku))

-- F2
readAndSolve :: FilePath -> IO ()
readAndSolve filename = do
                        sud <- readSudoku filename
                        let solved = solve sud in
                          printSudoku (fromJust solved)

-- F3
-- Gives all numeric values and position (skips blanks)
getValues (Sudoku rows) = let a = map (\(i, r) -> (i,(zip [0..8] r)) ) (zip [0..8] rows) in
                          filter (\((r,c),v) -> v /= Nothing )
                          (concat (map (\(r, l) -> map (\(c, v) -> ((r,c), v) )l ) a))

isSolutionOf :: Sudoku -> Sudoku -> Bool
isSolutionOf solution sudoku = let sudValues = getValues sudoku in
                                length (intersect sudValues (getValues solution))
                                 == length sudValues

-- F4
prop_SolveSound :: Sudoku -> Property
prop_SolveSound sud = isJust (solve sud) ==> (fromJust (solve sud)) `isSolutionOf` sud


-- Extra, for automation
solve_all_easy = solve_iter (["easy0"++ (show a) | a <- [1..9]]++["easy" ++ (show a) | a <- [10..50]])
solve_all_hard = solve_iter (["hard0"++ (show a) | a <- [1..9]]++["hard" ++ (show a) | a <- [10..50]])

solve_iter::[FilePath] -> IO()
solve_iter [] = putStrLn "Done"
solve_iter (x:xs) = do
                    sud <- readSudoku x
                    let sol = solve sud in
                      printSudoku (fromJust sol)
                    print (x)
                    solve_iter xs
