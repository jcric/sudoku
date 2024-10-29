-- \/\/\/ DO NOT MODIFY THE FOLLOWING LINES \/\/\/
module SudokuSolver(Board, Solutions(..), author, nickname, numSolutions) where
import Sudoku(Board, Solutions(..))
-- /\/\/\ DO NOT MODIFY THE PRECEDING LINES /\/\/\

{-
SOURCES:
For my solution, I have decided to implement a solver based on the one presented by Graham Hutton on his Youtube channel of the same
name under his course Advanced Functional Programming. He in turn has based his solver on Professor Richard Bird´s solution, which
was originally implemented in Pearl. Both sources are linked below:

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


-- Define transpose if it's not imported
transpose :: [[a]] -> [[a]]
transpose ([]:_) = []
transpose x = map head x : transpose (map tail x)

numSolutions :: Board -> IO()
numSolutions b = print b

rows :: Board -> Board
rows = id

cols :: Board -> Board
cols = transpose

-- Group a list into sublists of given size
group :: Int -> [a] -> [[a]]
group _ [] = []
group n xs = take n xs : group n (drop n xs)

-- Ungroup a list of lists into a single list
ungroup :: [[a]] -> [a]
ungroup = concat

-- Rearrange board into square sub-blocks of a specified size
boxs :: Int -> Board -> Board
boxs n = map concat . concatMap transpose . map (map (group n)) . group n
