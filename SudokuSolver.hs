-- \/\/\/ DO NOT MODIFY THE FOLLOWING LINES \/\/\/
module SudokuSolver(Board, Solutions(..), author, nickname, numSolutions) where
import Sudoku(Board, Solutions(..))
import Data.Maybe (fromMaybe)

-- /\/\/\ DO NOT MODIFY THE PRECEDING LINES /\/\/\

{-
SOURCES:
For my solution, I have decided to implement a solver based on the one presented by Graham Hutton on his Youtube channel of the same
name under his course Advanced Functional Programming. He in turn has based his solver on Professor Richard Bird´s solution, which
was originally implemented in Pearl.

To name a few of the differences between our versions - firstly, Graham´s board representation is as a list of strings, unlike mine 
which is a list of list of integers. Moreover, once he has created a list of choices, he is reducing values iteratively until a fix
point is reached. My reduce function does this automatically.



Both sources are linked below:

Graham Hutton´s Youtube channel:
https://www.youtube.com/@haskellhutt

Richard Bird´s paper:
https://www.cs.tufts.edu/~nr/cs257/archive/richard-bird/sudoku.pdf

EXPLANATION OF SOLUTION:

-}

author :: String
author = "Jonas Richter"
nickname :: String
nickname = "jcric"


{-
numSolutions

Takes a Board and returns the number of ways that it can be completed into a solved Sudoku: 0, 1, or several.

RETURNS:
PARAMETERS:
SIDE EFFECTS: None. Also, no input or output operations from e.g. the console, file system or network are performed.
EXAMPLES:


-}

--numSolutions :: Board -> Solutions
--numSolutions = undefined -- remove `= undefined' and define your function here


{- transpose xs
   Transposes a 2D list by swapping rows and columns.
   RETURNS: A new list with rows and columns swapped.
   EXAMPLES: transpose [[1,2,3], [4,5,6], [7,8,9]] = [[1,4,7], [2,5,8], [3,6,9]]
-}
transpose :: [[a]] -> [[a]]
transpose ([]:_) = []
transpose x = map head x : transpose (map tail x)

{- rows b
   Retrieves rows from the board.
   RETURNS: The board itself, representing rows as lists.
-}
rows :: Board -> Board
rows = id

{- cols b
   Retrieves columns from the board by transposing rows into columns.
   RETURNS: Transposed board representing columns.
   EXAMPLES: cols [[1,2], [3,4]] = [[1,3], [2,4]]
-}
cols :: Board -> Board
cols = transpose

{- group n xs
   Splits a list into sublists of specified size.
   RETURNS: A list of lists, each containing at most `n` elements.
   EXAMPLES: group 2 [1,2,3,4,5] = [[1,2], [3,4], [5]]
-}
group :: Int -> [a] -> [[a]]
group _ [] = []
group n xs = take n xs : group n (drop n xs)

{- ungroup xs
   Flattens a list of lists into a single list.
   RETURNS: A single list with all elements from the nested lists.
   EXAMPLES: ungroup [[1,2], [3,4], [5]] = [1,2,3,4,5]
-}
ungroup :: [[a]] -> [a]
ungroup = concat

{- boxs board
   Divides the board into square sub-blocks (3x3 by default).
   RETURNS: A board rearranged into 3x3 sub-blocks.
   EXAMPLES: boxs [[1,2,3,4,5,6,7,8,9], ...] = [[1,2,3,...], ...]
-}
boxs :: Board -> Board
boxs board =
    let n = length board
        blockSize = floor . sqrt $ fromIntegral n
    in map concat . concatMap transpose . map (map (group blockSize)) . group blockSize $ board

{- noDups xs
   Checks for duplicates in a list, ignoring zeros.
   RETURNS: True if there are no duplicates among non-zero elements.
   EXAMPLES: noDups [1,2,0,2] = False
-}
noDups :: (Eq a, Num a) => [a] -> Bool
noDups xs = let filtered = filter (/= 0) xs
            in length filtered == length (unique filtered)

{- unique xs
   Removes duplicate elements from a list, preserving only unique entries.
   PARAMETERS: xs - A list of elements.
   RETURNS: A list with only unique elements from the original list.
   EXAMPLES:
     unique [1,2,2,3,4] = [1,2,3,4]
     unique [5,5,5] = [5]
-}
unique :: Eq a => [a] -> [a]
unique []     = []
unique (x:xs) = x : unique (filter (/= x) xs)

{- noDupsInAll xss
   Checks each list within a list of lists for duplicates.
   RETURNS: True if no duplicates exist in any list.
   EXAMPLES: noDupsInAll [[1,0,3], [4,0,4]] = False
-}
noDupsInAll :: (Eq a, Num a) => [[a]] -> Bool
noDupsInAll = all noDups

{- valid b
   Determines if the board satisfies Sudoku constraints.
   RETURNS: True if there are no duplicates in rows, columns, or sub-blocks.
-}
valid :: Board -> Bool
valid b = (noDupsInAll(rows b)) &&  (noDupsInAll(cols b)) && (noDupsInAll(boxs b))


{- MatrixChoices
   A 2D grid where each cell can hold multiple choices represented by a list of integers.
   Each sublist corresponds to a row in the grid and each integer in the sublist represents a choice for that cell.
   INVARIANT: Each row in the grid must be a non-empty list of Choices, and each Choices list must contain at least one integer.
-}
type MatrixChoices = [[Choices]]

{- Matrix a
   A 2D grid where each cell holds a single value of type a.
   This type allows for operations on matrices with different types of elements.
   INVARIANT: Each row must be a non-empty list and all rows must have the same length (forming a proper rectangle).
-}
type Matrix a = [[a]]

{- Choices
   A list of integers representing possible choices for a cell in a MatrixChoices.
   Each integer corresponds to a distinct option available for selection.
   INVARIANT: The list of integers must not be empty and must contain distinct values.
-}
type Choices = [Int]

{- blank e
   Checks if a cell is blank.
   RETURNS: True if `e` equals 0.
-}
blank :: Int -> Bool
blank e = e == 0

{- cellvals board
   Provides possible values for a blank cell.
   RETURNS: A list of integers from 1 to the board's dimension.
-}
cellvals :: Board -> Choices
cellvals board = [1 .. n]
  where n = length board

{- choose board e
   Replaces blank cells with possible choices.
   RETURNS: A list of choices for each cell, where blanks are replaced by all possible values.
-}
choose :: Board -> Int -> Choices
choose board e = if blank e then cellvals board else [e]

{- choices board
   Generates a matrix of choices for each cell in the board.
   PARAMETERS: board - A Sudoku board where blank cells are represented by 0.
   RETURNS: A matrix where each cell contains a list of possible values.
   EXAMPLES:
     let board = [
         [0, 2],
         [3, 0]
     ]
     choices board = [
         [[1, 2], [2]],   -- Top row, with choices for blank cell and fixed value 2
         [[3],    [1, 2]] -- Bottom row, with fixed value 3 and choices for blank cell
     ]
-}
choices :: Board -> Matrix Choices
choices board = map (map (choose board)) board


-- Cartesian product for a list of lists
cp :: [[a]] -> [[a]]
cp [] = [[]]
cp (xs:xss) = [y:ys | y <- xs, ys <- cp xss]

{- explode m
   Expands a matrix of choices into a list of all possible Sudoku boards.
   PARAMETERS: m - A matrix of choices for each cell.
   RETURNS: A list of boards, where each board is a unique combination of choices.
   EXAMPLES:
     let m = [[ [1, 2], [3] ],
              [ [2],    [1, 3] ]]
     explode m = [
         [[1, 3], [2, 1]],
         [[1, 3], [2, 3]],
         [[2, 3], [2, 1]],
         [[2, 3], [2, 3]]
     ]
-}
explode :: MatrixChoices -> [Board]
explode m = cp (map cp m)

{- reduce xs
   Simplifies a matrix of choices by removing redundant values based on fixed (single-element) values within each row.
   PARAMETERS: xs - A list of lists, where each inner list represents a row of choices for cells in a Sudoku board.
   RETURNS: A matrix where each cell is reduced by removing choices that conflict with fixed values in the same row.
   EXAMPLES:
     let choicesMatrix = [
         [[1, 2], [2]],
         [[3],    [1, 2]]
     ]
     reduce choicesMatrix = [
         [[1],    [2]],
         [[3],    [1]]
     ]
-}
reduce :: Eq a => [[a]] -> [[a]]
reduce xs =
  let singleElements = [head l | l <- xs, length l == 1]
      uniqueSingleElements = unique singleElements
  in map (\l -> if length l == 1 then l else filter (`notElem` uniqueSingleElements) l) xs

{- cols3D boards
   Transposes a 3D board, effectively changing rows to columns.
   RETURNS: A list of boards transposed from the input.
   SIDE EFFECTS: None.
   EXAMPLES: cols3D [[[1, 2], [3, 4]], [[5, 6], [7, 8]]] = [[[1, 3], [5, 7]], [[2, 4], [6, 8]]]
-}
cols3D :: [Board] -> [Board]
cols3D = transpose

{- rows3D boards
   Returns the input 3D board as is, effectively serving as an identity function.
   RETURNS: The original list of boards.
   SIDE EFFECTS: None.
   EXAMPLES: rows3D [[[1, 2], [3, 4]], [[5, 6], [7, 8]]] = [[[1, 2], [3, 4]], [[5, 6], [7, 8]]]
-}
rows3D :: [Board] -> [Board]
rows3D = id

{- boxs3D boards
   Extracts and concatenates boxes (subsections) from a 3D board.
   RETURNS: A list of boxes from the input boards.
   SIDE EFFECTS: None.
   EXAMPLES: boxs3D [[[1, 2], [3, 4]], [[5, 6], [7, 8]]] = [[[1, 2, 5, 6], [3, 4, 7, 8]]]
-}
boxs3D :: [Board] -> [Board]
boxs3D = map concat . concatMap transpose . map (map (group 2)) . group 2

{- applyReduceToRows boards
   Applies the reduce function to each row of the given boards.
   RETURNS: A list of boards where each row has been reduced.
   SIDE EFFECTS: None
-}
applyReduceToRows :: [Board] -> [Board]
applyReduceToRows = map reduce


{- applyReduceToCols boards
   Applies the reduce function to each column of the given boards after transposing them.
   RETURNS: A list of boards where each column has been reduced.
   SIDE EFFECTS: None
-}
applyReduceToCols :: [Board] -> [Board]
applyReduceToCols b = cols3D(map reduce(cols3D(b)))

{- applyReduceToBoxs boards
   Applies the reduce function to each box of the given boards after extracting boxes from them.
   RETURNS: A list of boards where each box has been reduced.
   SIDE EFFECTS: None
-}
applyReduceToBoxs :: [Board] -> [Board]
applyReduceToBoxs b = boxs3D(map reduce(boxs3D(b)))

{- prune b
   Repeatedly applies reduction across rows, columns, and boxes.
   PARAMETERS: b - A 2D matrix where each cell contains a list of possible values.
   RETURNS: A 2D matrix of choices with unnecessary values pruned.
   EXAMPLES:
      let choicesMatrix = [
          [[1],     [1, 2], [1, 2, 3]],
          [[2],     [3],    [1, 2, 3]],
          [[1, 2],  [1, 2], [3]]
      ]
      prune choicesMatrix = [
          [[1],     [2],    [3]],
          [[2],     [3],    [1]],
          [[1],     [2],    [3]]
      ]
-}
prune :: [Board] -> [Board]
prune b = applyReduceToBoxs(applyReduceToCols(applyReduceToRows(b)))

{- solve b
   Generates all possible solutions for the Sudoku puzzle.
   RETURNS: A list of valid boards solving the puzzle.
-}
solve :: Board -> [Board]
solve b = filter valid(explode(prune(choices b)))
