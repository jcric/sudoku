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



transpose :: [[a]] -> [[a]]
transpose ([]:_) = []
transpose x = map head x : transpose (map tail x)

numSolutions :: Board -> IO()
numSolutions b = print b

rows :: Board -> Board
rows = id

cols :: Board -> Board
cols = transpose

-- Groups a list into sublists of given size
group :: Int -> [a] -> [[a]]
group _ [] = []
group n xs = take n xs : group n (drop n xs)

-- Ungroups a list of lists into a single list
ungroup :: [[a]] -> [a]
ungroup = concat

-- Rearranges board into square sub-blocks of a specified size
--boxs :: Int -> Board -> Board
--boxs n = map concat . concatMap transpose . map (map (group n)) . group n
boxs :: Board -> Board
boxs board =
    let n = length board           -- Get the number of rows (assuming a square board)
        blockSize = floor . sqrt $ fromIntegral n  -- Calculate the block size
    in map concat . concatMap transpose . map (map (group blockSize)) . group blockSize $ board--boxs = map concat . concatMap transpose . map (map (group 3)) . group 3

-- Define a function to check for duplicates in a single list, ignoring zeros
noDups :: (Eq a, Num a) => [a] -> Bool
noDups xs = let filtered = filter (/= 0) xs  -- Remove zeros from the list
            in length filtered == length (unique filtered)

-- Helper function to get unique elements in a list
unique :: Eq a => [a] -> [a]
unique []     = []
unique (x:xs) = x : unique (filter (/= x) xs)

-- Check each list in a list of lists
noDupsInAll :: (Eq a, Num a) => [[a]] -> Bool
noDupsInAll = all noDups

-- Valid if no duplicates in any row, columns or any box
valid :: Board -> Bool  --add n-box later
valid b = (noDupsInAll(rows b)) &&  (noDupsInAll(cols b)) && (noDupsInAll(boxs b))


-- Solves for all solutions
-- A composition of three functions
solve :: Board -> [Board]
solve b = filter valid(explode(prune(choices b)))
--solve b = explode(prune(choices b))
-- Takes each blank cell in sudoku board (0 elements) and replaces it by all possible choices
type Matrix a = [[a]]         -- Define Matrix as a list of lists of a generic type
type Choices = [Int]         -- Define Choices as a synonym for [Char]


-- Example function to determine if a cell is blank
blank :: Int -> Bool
blank e = e == 0  -- Assuming blank is represented by 0

-- Example function to return possible values for a blank cell
cellvals :: Board -> Choices
cellvals board = [1 .. n]
  where n = length board

-- Choose function to replace blank entries with possible choices
choose :: Board -> Int -> Choices
choose board e = if blank e then cellvals board else [e]

-- Choices function that replaces blank entries with all possible choices
choices :: Board -> Matrix Choices
choices board = map (map (choose board)) board
type MatrixChoices = [[Choices]] -- A 2D grid where each cell can have multiple values

-- Cartesian product for a list of lists
cp :: [[a]] -> [[a]]
cp [] = [[]]
cp (xs:xss) = [y:ys | y <- xs, ys <- cp xss]

-- Explode a MatrixChoices into a list of all possible Boards
explode :: MatrixChoices -> [Board]
explode m = cp (map cp m)

reduce :: Eq a => [[a]] -> [[a]]
reduce xs =
  let singleElements = [head l | l <- xs, length l == 1]  -- Get elements from single-element lists
      uniqueSingleElements = unique singleElements        -- Remove duplicates
  in map (\l -> if length l == 1 then l else filter (`notElem` uniqueSingleElements) l) xs

cols3D :: [Board] -> [Board]
cols3D = transpose

rows3D :: [Board] -> [Board]
rows3D = id

boxs3D :: [Board] -> [Board]
boxs3D = map concat . concatMap transpose . map (map (group 2)) . group 2

applyReduceToRows :: [Board] -> [Board]
applyReduceToRows = map reduce

applyReduceToCols :: [Board] -> [Board]
applyReduceToCols b = cols3D(map reduce(cols3D(b)))

applyReduceToBoxs :: [Board] -> [Board]
applyReduceToBoxs b = boxs3D(map reduce(boxs3D(b)))
--applyReduceToBoxs b = (map (map reduce) (boxs3D b))(boxs3D)
prune :: [Board] -> [Board]
prune b = applyReduceToBoxs(applyReduceToCols(applyReduceToRows(b)))
