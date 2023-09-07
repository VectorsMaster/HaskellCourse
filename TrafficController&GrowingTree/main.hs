{-# LANGUAGE OverloadedStrings #-}
import CodeWorld

-- | Draw a circle for traffic lights given color and Y-offset.
lightCircle :: Color -> Double -> Double -> Picture
lightCircle c x y = translated x y (colored c (solidCircle 1))

-- | Green traffic light.
greenLight :: Picture
greenLight = lightCircle green 0 (-3)

-- | Red traffic light.
redLight :: Picture
redLight = lightCircle red 0 3

-- | Yellow traffic light.
yellowLight :: Picture
yellowLight = lightCircle yellow 0 0

-- | Grey traffic light.
greyLight :: Double -> Picture
greyLight y = lightCircle grey 0 y

-- | Pedestrains Emoji
pEmoji :: Double -> Double -> Picture
pEmoji x y = translated x y(lettering "\x1F6B6")

-- | Cyclists Emoji
cEmoji :: Double -> Double -> Picture
cEmoji x y = colored white (translated x y (lettering "\x1F6B2"))

-- | Green state emojies
greenEmoji :: Picture
greenEmoji = pEmoji 3.5 (-1.2) <> cEmoji 6.5 (-1.2)

-- | Red state emojies
redEmoji :: Picture
redEmoji = pEmoji 3.5 1.2 <> cEmoji 6.5 1.2

-- | green light for Pedestrains and Cyclists
-- | PC means Pedestrains and Cyclists
greenLightPC :: Double -> Picture
greenLightPC x = lightCircle green x (-1.2)

-- | red light for PC
-- | PC means Pedestrains and Cyclists
redLightPC :: Double -> Picture
redLightPC x = lightCircle red x 1.2

-- | Frame for traffic lights for cars.
frameForCars :: Picture
frameForCars = rectangle 2.5 8.5 <> greyLight 0 <> greyLight (-3)
 <> greyLight 3

-- | Frame for traffic lights for Pedestrains
frameForPedestrains :: Picture
frameForPedestrains = translated 3.5 0 (rectangle 2.5 5 <> greyLight 1.2
 <> greyLight (-1.2))
 
-- | Frame for traffic lights for Cyclists
frameForCyclists :: Picture
frameForCyclists = translated 6.5 0 (rectangle 2.5 5 <> greyLight 1.2
 <> greyLight (-1.2))
 
-- | frame for Pedestrains and Cyclists
framePC :: Picture
framePC = frameForPedestrains <> frameForCyclists

-- | traffic lights for cars with four states.
-- | 1 green 
-- | 2 yellow
-- | 3 red
-- | 4 red and yellow
trafficLightsForCars :: Int -> Picture
trafficLightsForCars state 
 | state == 1 = greenLight <> frameForCars
 | state == 2 = yellowLight <> frameForCars 
 | state == 3 = redLight <> frameForCars
 | otherwise  = redLight <> yellowLight <> frameForCars

-- | traffic lights for PC with two states (green and red)
trafficLightsPC :: Int -> Picture
trafficLightsPC state
 | state == 1 = greenEmoji <> greenLightPC 3.5 <> greenLightPC 6.5 <> framePC
 | otherwise  = redEmoji <> redLightPC 3.5 <> redLightPC 6.5 <> framePC

blinkingPC :: Double -> Picture
blinkingPC t
 | floorT `mod` 2 == 0 = trafficLightsPC 1
 | otherwise           = framePC
 where 
  floorT :: Integer
  floorT = floor t
    
-- | traffic Controller for Pedestrains and Cyclists
trafficControllerPC :: Double -> Picture
trafficControllerPC t
 | floorT `mod` 8 >= 0 && floorT `mod` 8 < 4 = trafficLightsPC 2
 | floorT `mod` 8 >=4  && floorT `mod` 8 < 7 = trafficLightsPC 1
 | otherwise = blinkingPC (10*(t - (fromIntegral floorT)))
 where
  floorT :: Integer
  floorT = floor t

-- | Traffic lights controller switching according to the states 
-- | specfied in the homework document.
trafficControllerForCar :: Double -> Picture
trafficControllerForCar t
 | floorT `mod` 8 >= 0 && floorT `mod` 8 < 3 = trafficLightsForCars 1
 | floorT `mod` 8 == 3                       = trafficLightsForCars 2
 | floorT `mod` 8 >= 4 && floorT `mod` 8 < 7 = trafficLightsForCars 3
 | otherwise                                 = trafficLightsForCars 4
 where 
  floorT :: Integer
  floorT = floor t

-- | combined controller
trafficController :: Double -> Picture
trafficController t = 
 (translated (-2) 0 (trafficControllerForCar t)) <> 
  translated (-2.45) 0 (trafficControllerPC t)

-- | leaf with specific radius 
leaf :: Double -> Picture
leaf radius = colored green (solidCircle radius)

-- | A fractal tree of a given rank.
-- | Parameters description
-- | 1. n (integer): the number of remaining ranks
-- | 2. d (double) : the length of the trunk in the currrent rank
-- | 3. t (double) : the thickness of the trunks in the current rank
-- | 4. scale : the repesents the ratio of the current length of the upper trunk
-- | because the upper trunk will keep growing until it reaches it's maximum
-- | 5. leaves (boolean) : does this tree have leaves?
-- | 6. radius : the radius of the leaves (taller trees has bigger leaves)
-- | d and t decrease as the tree grows
tree :: Int -> Double -> Double -> Double -> Bool -> Double ->  Picture
tree 0 d t scale leaves radius 
 | leaves == True = leaf radius
 | otherwise   = blank
   
tree n d t scale leaves radius = 
 (translated (-0.1*t) yAxisTranslation leftBranch) 
  <> (translated (0.1*t) yAxisTranslation rightBranch)
  <> segment
 where
  points = [((-t)/2, 0), ((-t)/2, d), (t/2, d), (t/2, 0)]
  scaledPoints = [((-t)/2, 0), ((-t)/2, d*scale), (t/2, d*scale), (t/2, 0)]
    
  yAxisTranslation 
   | n == 1 = 0.9 * d * scale
   | otherwise = 0.9 * d
       
  segment
   | n == 1 = solidPolygon scaledPoints
   | otherwise = solidPolygon points
       
  nxtT = 0.75 * t
  nxtD = 0.8 * d
  leftBranch = rotated (pi/10) (tree (n - 1) nxtD nxtT scale leaves radius)
  rightBranch = rotated (-pi/10) (tree (n - 1) nxtD nxtT scale leaves radius)

-- | Picture of the maximum height of the tree
finalTree :: Picture
finalTree = tree 11 2.5 0.5 1.0 True 0.14

-- | Function to control the growment of the tree through time
-- | Each three seconds the number of ranks increases
-- | As the ranks increases the radius of leaves incrases, thickness and lengths
-- | of trunks decreases
growingTree :: Double -> Picture
growingTree t 
 | t >= 33.0 = finalTree
 | otherwise = tree n 2.5 0.5 scale leaves radius
 where
       
  n = floor ((3.0+t)/3)
  scale = (3.0+t)/3 - (fromIntegral n)
  leaves 
   | n >= 5 = True
   | otherwise = False
  radius = ((fromIntegral n) - 5)/50 + scale*0.02

-- | Function to move the growing tree down a little bit.
growingTreeTranslated :: Double -> Picture
growingTreeTranslated t = translated 0 (-2) (growingTree t)

-- | solution for the first task
solution1 :: IO ()
solution1 = animationOf trafficController

-- | solution for the second task
solution2 :: IO ()
solution2 = animationOf growingTreeTranslated

main :: IO ()
main = solution2
