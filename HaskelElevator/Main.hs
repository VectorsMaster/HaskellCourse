{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -fno-warn-type-defaults #-}
{-# OPTIONS_GHC -fdefer-typed-holes -fshow-hole-constraints -funclutter-valid-hole-fits #-}

module Main where

import CodeWorld

-- | Elevator Button
data Button = Up | Down | Stop

-- | Elevator Mode of Operation
data Mode = MovingUp | MovingDown | Idle

main :: IO ()
main = solution4

solution1 :: IO ()
solution1 = drawingOf (asSpaced 1 drawButton [Up, Stop, Down, Up, Down])

solution2 :: IO ()
solution2 = activityOf Idle handleWorld renderWorld


solution3 :: IO ()
solution3 = interactiveFSM Idle cmpButtons elevator convEvent drawMode drawButton

solution4 :: IO ()
solution4 = interactiveSystem Idle cmpButtons elevator convEvent drawMode drawButton 0.0 (moveElevator speed) drawElevator

highestFloor :: Double
highestFloor = 5.5

lowestFloor :: Double
lowestFloor = (-5.5)

-- | A function to get the speed of the elevator according to its mode
speed :: Mode -> Double
speed Idle = 0
speed MovingUp = 0.9
speed MovingDown = (-0.9)

interactiveSystem
  :: s -- ˆ Initial state of a FSM.
  -> (a -> a -> Bool) -- ˆ FSM action equality test.
  -> (s -> [(a, s)]) -- ˆ FSM State transitions.
  -> (Event -> Maybe a) -- ˆ How to convert events into actions.
  -> (s -> Picture) -- ˆ How to draw states.
  -> (a -> Picture) -- ˆ How to draw actions.
  -> system -- ˆ System state, whose modes -- are modelled with FSM.
  -> (Double -> s -> system -> system) -- How system evolves with time.
  -> (system -> Picture) -- ˆ How to render system.
  -> IO ()
interactiveSystem md cmp f conv dS dA sys g dSys = activityOf (md, sys) handleSystem renderSystem
 where
  handleSystem (TimePassing dt) (curMd, curSys) = (curMd, (g dt curMd curSys))
  handleSystem event (curMd, curSys) = ((applyAction (conv event) cmp f curMd), curSys)
  
  renderSystem (curMd, curSys)
   = translated 0 1.5 (dS curMd)
   <> translated 0 (-1.5) (asSpaced 1 dA (getActions (f curMd)))
   <> dSys curSys
   
  getActions [] = []
  getActions ((ac, _):xs) = (ac : getActions xs)


-- | A function to draw elevator according to its position
-- | It doesn't allow the elevator higher than the higher floor nor lower
-- | the the lowest floor
-- | It has only one double parameter the current position
drawElevator :: Double -> Picture
drawElevator p = translated 5 0 
 (rectangle 1.0 (highestFloor - lowestFloor + 2.0) <> myEmoji)
   where 
    alertUp = translated (-4.0) 8
     (colored red (lettering "You can't move up the highest floor"))

    alertDown = translated (-4.0) 8
     (colored red (lettering "You can't move down the lowest floor"))

    myEmoji = translated 0 p hEmoji <> additionalAlert
    additionalAlert 
     | p > lowestFloor && p< highestFloor = blank
     | p == highestFloor                  = alertUp
     | otherwise                          = alertDown
 
  
-- | A function to change the elevator position according to period of time 
-- |  with a fixed mode in that period
moveElevator :: (a -> Double) -> Double -> a -> Double -> Double
moveElevator f dt md curSys = position
 where 
  expectedPosition = curSys + (f md) * dt
  position
   | expectedPosition > highestFloor = highestFloor
   | expectedPosition < lowestFloor = lowestFloor
   | otherwise = expectedPosition



-- | Interactive finite state machine simulation.
interactiveFSM
  :: s -- ˆ Initial state.
  -> (a -> a -> Bool) -- ˆ Action equality test.
  -> (s -> [(a, s)]) -- ˆ State transitions.
  -> (Event -> Maybe a) -- ˆ How to convert events into actions.
  -> (s -> Picture) -- ˆ How to draw states.
  -> (a -> Picture) -- ˆ How to draw actions.
  -> IO ()
interactiveFSM initialS cmp f conv dS dA = activityOf initialS handleState renderState 
 where
  handleState event curState = applyAction (conv event) cmp f curState
  renderState curState
   = translated 0 1.5 (dS curState)
   <> translated 0 (-1.5) (asSpaced 1 dA (getActions (f curState)))
   
  getActions [] = []
  getActions ((ac, _):xs) = (ac : getActions xs)
   
  
-- | A function to convert an event to maybe button
-- | pressing Up and Down and Stop will lead to just Button
-- | other events will lead to nothing
convEvent :: Event -> Maybe Button
convEvent (KeyPress "Up") = Just Up
convEvent (KeyPress "Down") = Just Down
convEvent (KeyPress " ") = Just Stop
convEvent _ = Nothing


-- | A function to apply keyboard events into elevator modes
handleWorld :: Event -> Mode -> Mode
handleWorld (KeyPress "Up") md = applyAction (Just Up) cmpButtons elevator md
handleWorld (KeyPress "Down") md = applyAction (Just Down) cmpButtons elevator md
handleWorld (KeyPress " ") md = applyAction (Just Stop) cmpButtons elevator md 
handleWorld _anyEvent md = applyAction Nothing cmpButtons elevator md

-- | A function to render the elevator State
renderWorld :: Mode -> Picture
renderWorld md = drawMode md

-- | Function to compate buttons
cmpButtons :: Button -> Button -> Bool 
cmpButtons Up Up = True
cmpButtons Down Down = True
cmpButtons Stop Stop = True
cmpButtons _ _ = False

-- | Apply an action (if any) to the current state
-- of a finite state machine.
applyAction
  :: Maybe a -- ˆ An action to apply (if any).
  -> (a -> a -> Bool) -- ˆ Action equality test.
  -> (s -> [(a, s)]) -- ˆ State transitions.
  -> s -- ˆ Current state.
  -> s -- ˆ New state (if possible).
applyAction Nothing _ _ state = state
applyAction (Just tran) cmp t state = getNewState tran candidateStates
 where 
  candidateStates = t state 
  getNewState tr ((x, y):xs) 
   | cmp tr x =  y
   | otherwise = getNewState tr xs
  getNewState _ _ = state

-- | FSM corresponding to a simple elevator.
-- | When the elevator is Idle up and down buttons can be pressed and will lead 
-- | to new mode
-- | When the elevator is moving up or down only stop button can be pressed.
elevator :: Mode -> [(Button, Mode)]
elevator Idle = [(Up, MovingUp), (Down, MovingDown)]
elevator MovingUp = [(Stop, Idle)]
elevator MovingDown = [(Stop, Idle)]

-- | A function to draw the mode of the function
drawMode :: Mode -> Picture
drawMode md = rectangle 1.4 3.0 <> picU <> picD <> background
 where 
  background = colored (light grey) (solidRectangle 1.4 3.0)
  upColor = checkModes md MovingUp
  downColor = checkModes md MovingDown
  picU = translated 0.08 0.8 (colored upColor upEmoji)
  picD = translated (-0.08) (-0.8)  (colored downColor downEmoji)


-- | Function to compare wether two modes are equal (used to to color arrows
-- | corresponding to current state)
checkModes :: Mode -> Mode -> Color
checkModes MovingUp MovingUp = red
checkModes MovingDown MovingDown = red
checkModes _ _ = grey

-- | Draw several objects some distance apart from each other.
-- | First parameter:  How far apart to draw objects.
-- | Second parameter: Function of How to draw a single object.
-- | Third parameter: The list of objects to draw
asSpaced :: Double -> (a -> Picture)  -> [a] -> Picture
asSpaced dis f lst = ret
 where 
  totalDis = (fromIntegral ((length lst) - 1)) * (dis + 1.4) 
  ret =  translated (totalDis/2.0)  0 (trans (map f lst))
  
  trans :: [Picture] -> Picture
  trans [] = blank
  trans (x:xs) = translated (- len * (dis + 1.4)) 0 x <> trans xs
   where 
    len = fromIntegral (length xs)
  
  


-- | Render elevator Button
drawButton :: Button -> Picture
drawButton Up = inCircle white (translated 0.1 0 upEmoji)
drawButton Down = inCircle white (translated (-0.1) 0 downEmoji)
drawButton Stop = inCircle red stopEmoji

-- | Function to center a picture in a circle
inCircle :: Color -> Picture -> Picture
inCircle c p = colored c p <> circle 0.7 <> colored (light grey) (solidCircle 0.7)

-- | Up arrow Emoji
upEmoji :: Picture
upEmoji = (rotated (pi/2.0) arrowEmoji)

-- | Down arrow Emoji
downEmoji :: Picture
downEmoji = (rotated (3*pi/2.0) arrowEmoji)

-- | Emoji of an arrow (used in buttons)
arrowEmoji :: Picture
arrowEmoji = lettering "\x27A4"

-- | Emoji of stop
stopEmoji :: Picture 
stopEmoji = scaled 0.5 0.5 (lettering "STOP")


-- | Emoji of human
hEmoji :: Picture
hEmoji = lettering "\x1F6B6"
