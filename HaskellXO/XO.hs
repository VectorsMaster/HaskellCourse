{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
import CodeWorld
import Data.List
import Data.Maybe

-- | A mark for tic-tac-toe.
data Mark = X | O
  deriving (Eq, Show)
  
-- | A board is a 2D grid of cells.  
data Cell = Cell {
  mrk :: Maybe Mark,
  coords :: (Int, Int)
}

-- | A board is a 2D grid of cells.
type Board = [[Cell]]

-- | Initialise an empty NxM board.
initBoard :: (Int, Int) -> Board
initBoard (n, m) = iterRows 0 
  where 
    iterRows r = if r == n then [] else iterColumns 0 : iterRows (r + 1)
      where 
        iterColumns c = 
          if c == m then [] else Cell Nothing (r, c) : iterColumns (c + 1)
   
-- | Draw a single mark (X or O).
drawMark :: Mark -> Picture
drawMark X = scaled 0.4 0.4 (rotated (pi/4)
  (solidRectangle 0.5 2 <> solidRectangle 2 0.5))
drawMark O = thickCircle 0.2 0.3

-- | Draw one board cell at given coordinates.
drawCellAt :: Int -> Int -> Cell -> Picture
drawCellAt i j cell = translated x y
  (cellPicture <> rectangle 1 1)
  where
    x = fromIntegral i
    y = fromIntegral j
    cellPicture =
      case (mrk cell) of
        Nothing -> blank
        Just m -> drawMark m
        
-- | Draw a rectangular board.
-- | Using foldl it iterate over all rows and all elements in this row and draw
-- | then using drawCellAt
drawBoard :: Board -> Picture
drawBoard board = fst (foldl 
  (\(pic, i) curRow -> (drawRowAt i curRow <> pic, i + 1))
  (blank, 0) 
  board)
  where 
   -- | function to to draw a specific Row 
    drawRowAt i curRow = fst (foldl 
      (\(pic, c) curCell -> 
        ((if contains (i, c) then colored red (drawCellAt c i curCell) 
          else drawCellAt c i curCell) <> pic , c + 1))
      (blank, 0) 
      curRow)
    
    -- | lst is the coordinates of winning streak (if it exists)
    (_, lst) = winner board
    
    -- | A function to check whether a specific coordinates is in the winning streak
    contains (x, y) = 
      foldl (\ret (i, j) -> ret || (x == i && y == j)) False lst

-- | Question: 
-- | why not Int -> a -> [a] -> [a] or Int -> (a -> b) -> [a] -> [b]?
-- | Answer: 
-- | Because other position will not be changed (They will keep being of type a)

-- | Try place a mark a given position on a board.
-- The type of mark is determined automatically.
-- When the game is over (a winner exists) no marks are placed.
putMarkAt :: (Int, Int) -> Board -> Board
putMarkAt (i, j) board = ret
  where 
    (curWinner, _) = winner board
    
    -- | if there is a winner just return the same board 
    -- | otherwise try to put mark in the cell
    ret = case curWinner of 
     Just _ -> board
     Nothing -> updateAt i (\row -> updateAt j modifyElement row) board
     
    -- | compare two marks
    cmp :: Maybe Mark -> Maybe Mark -> Int
    cmp (Just O) (Just O) = 1
    cmp (Just X) (Just X) = 1
    cmp Nothing Nothing = 1
    cmp _ _ = 0
    
    -- | Count Xs and Os to determine what is the type of mark in this turn
    countX = count (Just X)
    countO = count (Just O)
    markToPlay = if countX == countO then Just X else Just O
    
    -- | A function to Count for a sepcific mark how many times it occures 
    count :: Maybe Mark -> Int
    count mark = foldl 
      (\ans row -> foldl (\curAns x -> curAns + cmp (mrk x) mark) ans row)
      0
      board
  
    modifyElement curCell =
      case (mrk curCell) of
        Nothing -> Cell markToPlay (coords curCell)
        Just _ -> curCell
    
    
    

-- | Try update an element at a given position in a list.
updateAt :: Int -> (a -> a) -> [a] -> [a]
updateAt pos f curLst = iter curLst 0
  where 
    iter [] _ = []
    iter (x:xs) idx = if idx == pos then f x : xs else x : iter xs (idx + 1)
    
    
-- | Handle mouse clicks to put marks.
handleGame :: Event -> Board -> Board
handleGame (PointerPress mouse) = putMarkAt (pointToCoords mouse)
handleGame _ = id

-- | Convert mouse position into board coordinates.
pointToCoords :: Point -> (Int, Int)
pointToCoords (x, y) = (round y, round x)

-- | Determine a winner in a game of tic-tac-toe (if exists).
winner :: Board -> (Maybe Mark, [(Int, Int)])
winner board = getWinner (filter isLongStreak (concatMap streaks allLines))
  where
    allLines = rows ++ columns ++ diagonals
    rows = board
    columns = transpose rows
    diagonals = leftDiagonals ++ rightDiagonals
    leftDiagonals = leftDiagonalsOf rows
    rightDiagonals = leftDiagonalsOf (reverse rows)
    leftDiagonalsOf b = leftTopDiagonalsOf b ++ leftBottomDiagonalsOf b
    leftTopDiagonalsOf = transpose . zipWith drop [0..]
    leftBottomDiagonalsOf = leftTopDiagonalsOf . transpose

-- | Get all consequent streaks ignoring 'Nothing'.
streaks :: [Cell] -> [(Int, Mark, [(Int, Int)])]
streaks [] = []
streaks (Cell Nothing _ : xs) = streaks xs
streaks (Cell (Just x) crds : xs) 
  = (1 + length ys, x, crds : getAllCoords ys) : streaks zs
  where
    check cell = mrk cell == Just x
    (ys, zs) = span check xs
    getAllCoords [] = []
    getAllCoords (cell:cells) = coords cell : getAllCoords cells
    
-- | Determine is a streak is long enough to be a winning streak.
-- | any value k will work just write it instead of 3
isLongStreak :: (Int, a, [(Int, Int)]) -> Bool
isLongStreak (i, _, _) = i >= 3

-- | Get a winning mark (if exists).
getWinner :: [(Int, a, [(Int, Int)])] -> (Maybe a, [(Int, Int)])
getWinner [] = (Nothing, [])
getWinner (x : _) = (Just mark, lst)
  where 
    (_, mark, lst) = x

main :: IO ()
main = activityOf (initBoard (4, 4)) handleGame drawBoard

