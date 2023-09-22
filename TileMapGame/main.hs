{-# LANGUAGE OverloadedStrings #-}

import CodeWorld

type Coords = (Int, Int)
data Tile = Wall | Floor | Door DoorColor | Exit | Button DoorColor
data DoorColor = Red | Blue | Green 
data Dir = Up | Down | Lft | Rght

-- | All available colors of doors
allDoorColors :: [DoorColor]
allDoorColors = [Red, Blue, Green]

-- | Here exist all elements I should keep track of in the state of solution4
-- | Character coordinates
-- | List of colors of which all coresponding doors are open
data State = State Coords [DoorColor]

main :: IO ()
main = solution4

-- | Simple Map
solution1 :: IO ()
solution1 = activityOf initialWorld (handleWorld simpleLevelMap)
 (renderWorld (drawRows (-5, 5) (-5, 5) simpleLevelMap))

-- | A map contains doors, buttons, walls and one exit point
-- | It's not winnable because buttons are not pressable
solution2 :: IO ()
solution2 = activityOf initialWorld (handleWorld (levelMap []))
 (renderWorld (drawLevelMap (levelMap [])))

-- | Same as solution2 but all doors are open
solution3 :: IO ()
solution3 = activityOf initialWorld (handleWorld 
 (openDoors allDoorColors (levelMap []))) 
  (renderWorld (drawLevelMap (levelMap allDoorColors)))
  
-- | A full interative map
-- | When the character presses a button (stands on it), the state of the doors
-- | with the color of the button will be reversed
-- | (Opened doors will be closed and closed doors will be opened)
-- | You need to open two (red and blue) doors to reach the exit
-- | Success message will be written if you are standing in the exit cell
solution4 :: IO ()
solution4 = activityOf initialState handleState renderState

-- | A function to handle movements of the character 
-- | (it does not work for solution4 because I indroduced new state for it)
-- | First argument  : function that is responsilbe for the map (decides type of each cell)
-- | Second argument : event (keyboard movement keys)
-- | Third argument  : current coordinates
handleWorld :: (Coords -> Tile) -> Event -> Coords ->  Coords
handleWorld f (KeyPress "Up") (x, y) = tryMove Up (x, y) f
handleWorld f (KeyPress "Down") (x, y) = tryMove Down (x, y) f
handleWorld f (KeyPress "Left") (x, y) = tryMove Lft (x, y) f
handleWorld f (KeyPress "Right") (x, y) = tryMove Rght (x, y) f
handleWorld f _anyEvent coords = coords

-- | A function to draw the map after each event
-- | (it does not work for solution4 because I indroduced new state for it)
-- | The only change it can happen in solution1, solution2 and solution3 
-- |   is character coordinates
-- | First argument   : the map we are using
-- | Secpond argument :  character current coordinates
renderWorld :: Picture -> Coords -> Picture
renderWorld t (x, y) = pEmoji x y <> t

-- | A function to draw emoji that represents the character in specific position
-- | First argument  : column coordinate
-- | Second argument : raw coordinate
pEmoji :: Int -> Int -> Picture
pEmoji x y = translated (fromIntegral x) (fromIntegral y) (lettering "\x1F6B6")

-- | Starting coordinates for the solution1
initialWorld :: Coords
initialWorld = (0, 0)

-- | A function which try to change coordinates according direction of movement
-- | It blocks illegal movements according to the type of tiles in that direction
-- | First argument  : direction of the movement
-- | Second argument : current coordinates
-- | Third argument  : function that convert coordinates to tile (to check the 
-- |   type of the tile in the direction of the movement)
tryMove :: Dir -> Coords -> (Coords -> Tile) -> Coords
tryMove dir coords f
 | canMove (f (goToDirection dir coords)) = goToDirection dir coords
 | otherwise = coords

-- | A function which decide whether it is legal to move to a tile from its type
-- | First argument : tile
-- | It returns true for wall, floor and exit tiles
-- | It returns false for the others
canMove :: Tile -> Bool
canMove Wall = False
canMove (Door _) = False
canMove (Button _) = True
canMove Floor = True
canMove Exit = True

-- | A function to change the coordinates in specific direction
-- | First argument : direction of movement
-- | Second argument : currenct coordinates
goToDirection :: Dir -> Coords -> Coords
goToDirection Up (x, y)   = (x, y + 1)
goToDirection Down (x, y) = (x, y - 1)
goToDirection Rght (x, y) = (x + 1, y)
goToDirection Lft (x, y)  = (x - 1, y)

-- | Simple Map for Solution1
-- | First argument : coordinates
-- | We can modify it as we want the map
simpleLevelMap :: Coords -> Tile 
simpleLevelMap (i, j) 
 | (i, j) == (5, 0) = Exit
 | abs i > 4 || abs j > 4 = Wall
 | (i, j) == (-1, 2)      = Button Red
 | (i, j) == (1, 3)       = Door Red
 | i == 1                 = Wall
 | i < 1 && j == -2       = Wall
 | otherwise              = Floor

-- | Function which draw a map with row and column ranges
-- | First argument : range of the rows
-- | Secnd argument : range of the columns
-- | Third argument  : a function which take coordinates and convert them to tile
-- |   to determine the type of each tile (cell) 
drawRows :: (Int, Int) -> (Int, Int) -> (Coords -> Tile) -> Picture
drawRows (fromY, toY) (fromX, toX) f
 | fromY > toY = blank
 | otherwise = drawRow fromY (fromX, toX) f 
  <> drawRows (fromY + 1, toY) (fromX, toX) f
  
-- | Function to draw row of tiles with specific length
-- | First argument  : index of the row
-- | Second argument : range of the columns
-- | Third argument  : a function which take coordinates and convert them to tile
-- |   to determine the type of each tile (cell) 
drawRow :: Int -> (Int, Int) -> (Coords -> Tile) -> Picture
drawRow j (from, to) f
  | from > to = blank
  | otherwise = 
     coordinatedTile drawTile (f (from, j)) (from, j) 
     <> drawRow j (from + 1, to) f
  
-- | A function which locate a tile in specific location
-- | First argument  : function that take a tile and draw it
-- | Second argument : type of the tile
-- | Third argument  : location of the tile
coordinatedTile :: (Tile -> Picture) -> Tile -> Coords -> Picture
coordinatedTile f t (x, y) = translated (fromIntegral x) (fromIntegral y) (f t)

-- | A function takes the type of the tile and draw it
-- | First argument : the type of the tile
drawTile :: Tile -> Picture
drawTile Wall = tile black
drawTile Floor = tile yellow
drawTile Exit = tile orange
drawTile (Button Red) = buttonTile red
drawTile (Button Blue) = buttonTile blue
drawTile (Button Green)= buttonTile green
drawTile (Door Red) = doorTile red
drawTile (Door Blue) = doorTile blue
drawTile (Door Green) = doorTile green

-- | A function draws a door tile with specific color
doorTile :: Color -> Picture
doorTile c = colored c (solidCircle 0.4) <> tile black

-- | A function draws a button tile with specific color
buttonTile :: Color -> Picture
buttonTile c = colored c (solidCircle 0.4) <> tile yellow

-- | A function draws a tile (cell) with specific color
tile :: Color -> Picture
tile c = colored c (solidRectangle 0.98 0.98)
  
 
-- | Down from here all are necessary implementations for solution2, solution3 
-- |   and solution4

-- | A function to handle events of the characters
-- | It handles openning doors if the character presses the button with 
-- |   the corresponding color
-- | It handles movements of the characters
handleState :: Event -> State -> State
handleState (KeyPress "Up") (State  c dc)  = moveTo Up (State c dc) (levelMap dc) 
handleState (KeyPress "Down") (State c dc) = moveTo Down (State c dc) (levelMap dc)
handleState (KeyPress "Left") (State c dc) = moveTo Lft (State c dc) (levelMap dc)
handleState (KeyPress "Right") (State c dc) = moveTo Rght (State c dc) (levelMap dc)
handleState _anyEvent state = state

-- | A function which draw the map after events and changes
renderState :: State -> Picture
renderState (State (x, y) dc) 
 | exists (x, y) exitCells = scaled 2 2 ((lettering  "Congrats! you won"))
 | otherwise = pEmoji x y <> (drawLevelMap (levelMap dc))

-- | A function which changes the state (coordinates of the character and list 
-- |  open doors) according to the events
-- | First argument  : the direction of the movement
-- | Second argument  : the current state
-- | Third argument : the function that decide the type of the tile according 
-- |  to its coordinates
moveTo :: Dir -> State -> (Coords -> Tile) -> State
moveTo dir (State coords dc) f
 | not (canMove (f (goToDirection dir coords))) = (State coords dc)
 | otherwise = (addColor f (State (goToDirection dir coords) dc))
 
-- | A function to check whether the character is on button cell and change the 
-- |  state of doors according to that
-- | When the character enters button tile with a specific colors, there will be
-- | two situations.
-- |  1. That color exists in the list of colors of opened doors: In this case 
-- |    the color will be removed from that list
-- |  2. That color does not exist in the list: In this case the color will be 
-- |    added to that list.
-- | In other words, the state of the doors with that color will be reversed.
-- | First argument : a function which to design the map
-- | Second argument : current state
addColor :: (Coords -> Tile) -> State -> State
addColor f (State (x, y) dc) 
 | isButton (x, y) f && oneOf clr dc = (State (x, y) (eraseColor clr dc))
 | isButton (x, y) f = (State (x, y) (clr : dc))
 | otherwise = (State (x, y) dc)
  where
   clr = colorOfButton (f (x, y))
  
 
-- | A function to remove a color element from a list of colors
eraseColor :: DoorColor -> [DoorColor] -> [DoorColor]
eraseColor _ [] = []
eraseColor dc (x:xs) 
 | eqDoorColor dc x = eraseColor dc xs
 | otherwise = x : eraseColor dc xs


-- | A function which open all doors with colors in the given list and returns 
-- |  new function to create the new map (which has that doors open)
-- | First argument  : the list of colors
-- | Second argument : original function which creates the map
-- | I used it only for solution3 becuse I introduced different state for solution4
openDoors :: [DoorColor] -> (Coords -> Tile) -> (Coords -> Tile)
openDoors doorColors _ = levelMap doorColors


-- | A function to draw the map according to ranges of columns, rows and 
-- |   the function the determine the type of each cell
-- | First argument : function to determine the type of each cell
-- |  It's not in the implementation of the function but it's crucial
drawLevelMap :: (Coords -> Tile) -> Picture
drawLevelMap = drawRows (-10, 10) (-10, 10)

-- | A function which convert coordinates into tiles 
-- | it keeps track of the colors of which corresponding doors are open 
-- |   to convert them to floor tiles
-- | First argument  : list of colors of which corresponding doors are open
-- | Second argument : coordinates which will converted to tile
levelMap :: [DoorColor] -> Coords -> Tile 
levelMap dc (i, j) 
 | exists (i, j) exitCells                               = Exit
 | exists (i, j) (doorCells Red) && not (oneOf Red dc)   = Door Red
 | exists (i, j) (doorCells Red) && oneOf Red dc         = Floor 
 | exists (i, j) (doorCells Blue) && not (oneOf Blue dc) = Door Blue
 | exists (i, j) (doorCells Blue) && oneOf Blue dc       = Floor
 | exists (i, j) (buttonCells Blue)                      = Button Blue
 | exists (i, j) (buttonCells Red)                       = Button Red
 | wallCells (i, j)                                      = Wall
 | otherwise                                             = Floor
 
 
-- | A recursion function to check wether specific coordinates are in a list or not
-- | First argument  : coordinates we want to test whether it exists in the list
-- | Second argument : list of coordinates
exists :: Coords -> [Coords] -> Bool
exists _ [] = False
exists coords (x : xs)  
 | eqCoords coords x = True
 | otherwise         = False

-- | A function to check the equality of two coordinates
eqCoords :: Coords -> Coords -> Bool
eqCoords (x1, y1) (x2, y2) 
 | x1==x2 && y1 == y2 = True
 | otherwise          = False

-- | A recursion funciton which check whether a doorcolor exists in the list or not
-- | First argument  : the color
-- | Second argument : the list
oneOf :: DoorColor -> [DoorColor] -> Bool
oneOf _ [] = False
oneOf dc (x : xs) 
 | eqDoorColor dc x = True
 | otherwise = oneOf dc xs

-- | A function which takes two colors and compare them
-- | returns true in case of equality and otherwise false
eqDoorColor :: DoorColor -> DoorColor -> Bool
eqDoorColor Red Red   = True
eqDoorColor Blue Blue = True
eqDoorColor Green Green = True
eqDoorColor _ _ = False
 

-- | A function which takes two tiles and returns true if the tiles are buttons
-- |  with the same color, and false otherwise
eqButton :: Tile -> Tile -> Bool
eqButton (Button _) (Button _) = True
eqButton _ _                   = False

-- | A function which decide wether a tile in the given coordinates is button
-- | First argument  : coordinates
-- | Second argument : the function that designs the map 
isButton :: Coords -> (Coords -> Tile) -> Bool
isButton (x, y) f 
  | eqButton (f (x, y)) (Button Red) = True
  | eqButton (f (x, y)) (Button Blue) = True
  | eqButton (f (x, y)) (Button Green) = True
  | otherwise = False

-- | A function which takes a button tile and returns its color
-- |  I assumed that only it receives only button tiles as input
-- |  I cheched the type of the tile every time I used it in the implementation
colorOfButton :: Tile -> DoorColor
colorOfButton (Button color) = color

-- | Exit Cells
exitCells :: [Coords]
exitCells = [(10, 0)]

-- | A function which return color of door tiles according to 
-- |   how you want the map to look like 
doorCells :: DoorColor -> [Coords]
doorCells Red  = [(-7, -2)]
doorCells Blue = [(1, 3)] 


-- | A function which return color of button tiles according to 
-- |   how you want the map to look like
buttonCells :: DoorColor -> [Coords]
buttonCells Red  = [(-9, 9)]
buttonCells Blue = [(-5, -5)] 

-- | A function where you can place wall cells in your map
-- | return True of you want this coordinates to be a wall cells 
-- | otherwise return false
wallCells :: Coords -> Bool
wallCells (i, j) 
 | abs i >= 10 || abs j >= 10 = True
 | j == 6 && i < -5           = True
 | i == -6 && j > 6 && j < 9  = True
 | j == 4 && i < -2 && i > -9 = True
 | i == -2 && j >= 4          = True
 | i == 1                     = True
 | i < 1 && j == -2           = True
 | otherwise                  = False

-- | Initial State
initialState :: State
initialState = (State (0, 0) [])

 
