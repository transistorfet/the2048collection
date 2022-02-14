module Lib (mainFunc) where

import Debug.Trace

import System.Random
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact


squares = 4

windowSizeX = 800
windowSizeY = 800

squareSize = (fromIntegral windowSizeX) / (fromIntegral squares) :: Float
squareSizeInner = squareSize - (squareSize / 10) :: Float


data World = World {
  seed :: StdGen,
  rows :: [[Int]]
}

data Direction =
    Forward
  | Reverse


-- Main Entry Point
mainFunc :: IO ()
mainFunc = do
  gen <- getStdGen
  interactIO
    (InWindow "2048" (windowSizeX, windowSizeY) (10, 10))
    (greyN 0.5)     -- background color
    (generate gen)  -- initial world
    render          -- function to convert world to a Picture
    input           -- function to handle input events
    modify          -- function to step the world one iteration


-- Generate an empty world
generate :: StdGen -> World
generate gen =
  World {
    seed = gen,
    rows = map (\_ -> map (\_ -> 0) [0..(squares - 1)]) [0..(squares - 1)]
  }


-- Respond to an Up arrow press by shifting all the numbers upward, combining any like numbers as we go
shiftWorldUp :: World -> World
shiftWorldUp world =
  let (rs, newSeed) = randomSequence (seed world) squares [] in
  let newRows = toColumns (collapseAndFillRows rs Forward (toColumns (rows world))) in
  World { seed = newSeed, rows = newRows }

-- Same for Down arrow
shiftWorldDown :: World -> World
shiftWorldDown world =
  let (rs, newSeed) = randomSequence (seed world) squares [] in
  let newRows = toColumns (collapseAndFillRows rs Reverse (toColumns (rows world))) in
  World { seed = newSeed, rows = newRows }

-- Same for Left arrow
shiftWorldLeft :: World -> World
shiftWorldLeft world =
  let (rs, newSeed) = randomSequence (seed world) squares [] in
  let newRows = collapseAndFillRows rs Forward (rows world) in
  World { seed = newSeed, rows = newRows }

-- Same for Right arrow
shiftWorldRight :: World -> World
shiftWorldRight world =
  let (rs, newSeed) = randomSequence (seed world) squares [] in
  let newRows = collapseAndFillRows rs Reverse (rows world) in
  World { seed = newSeed, rows = newRows }


-- Collapse the rows left or right depending on the direction, filling in with zeros and a random
-- chance that a new number will appear on the opposite side of the direction
collapseAndFillRows :: [Double] -> Direction -> [[Int]] -> [[Int]]
collapseAndFillRows rs dir rows =
  map forEachRow (zip [0..] rows)
  where
    forEachRow (i, row) =
      let newRow = maybeReverse dir $ collapseNumbers $ maybeReverse dir row in
      let zeros = fillZeros (rs !! i) (squares - (length newRow)) in
      case dir of
        Forward -> newRow ++ zeros
        Reverse -> (reverse zeros) ++ newRow

-- Return either a forward or reversed list based on the direction value
maybeReverse :: Direction -> [Int] -> [Int]
maybeReverse Forward list = list
maybeReverse Reverse list = reverse list

-- Remove all the zeros from a list, and add any adjacent numbers that have the same value
collapseNumbers :: [Int] -> [Int]
collapseNumbers []            = []
collapseNumbers (0 : remain)  = collapseNumbers remain
collapseNumbers (num : [])    = [ num ]
collapseNumbers (first : second : remain) =
  if second == 0 then
    collapseNumbers (first : remain)
  else if first == second then
    (first + second) : collapseNumbers remain
  else
    first : collapseNumbers (second : remain)

-- Return a list of zeros of the given size, but the last zero has a random chance of being a 2 instead
fillZeros :: Double -> Int -> [Int]
fillZeros r 0   = []
fillZeros r 1   = [ if r < 0.2 then 2 else 0 ]
fillZeros r len = 0 : fillZeros r (len - 1)

-- Return a list of random numbers of the given length, along with an updated random seed
randomSequence :: StdGen -> Int -> [Double] -> ([Double], StdGen)
randomSequence g 0 list =
  (list, g)
randomSequence g n list =
  let (r, nextg) = randomR (0.0, 1.0) g
  in randomSequence nextg (n - 1) (r : list)

-- Transpose the rows with the columns in a matrix
toColumns :: [[Int]] -> [[Int]]
toColumns rows =
  map (\i ->
    map (\row -> (row !! i)) rows)
    [0..(squares - 1)]


-- Draw the world
render :: World -> IO Picture
render world =
  return $ pictures $ renderAllRows (rows world)

-- Draw all the rows
renderAllRows :: [[Int]] -> [Picture]
renderAllRows rows =
  map forEachRow (zip [0..] rows)
  where
    forEachRow (i, entry) =
      let y = offsetOf i in
      pictures $ renderRow (-1 * y) entry

-- Draw a single row
renderRow :: Float -> [Int] -> [Picture]
renderRow y row =
  map forEachTile (zip [0..] row)
  where
    forEachTile (i, entry) =
      let x = offsetOf i in
      Translate x y $ renderTile entry

-- Get the offset from the centre of the screen for a square on a given row or column
offsetOf :: Float -> Float
offsetOf i =
  ((i - (fromIntegral squares / 2)) * squareSize + (squareSize / 2))

-- Draw a single square with its number
renderTile :: Int -> Picture
renderTile number =
  let len = fromIntegral $ length (show number) in
  pictures [
    color (greyN 0.1) $
      rectangleSolid squareSizeInner squareSizeInner

  , color white $
      Translate (-squareSizeInner / 8) (-squareSizeInner / 8) $ Scale (0.5 / len) (0.5 / len) $
        text (show number)
  ]

-- Handle input events and call the appropriate shift function
input :: Event -> World -> IO World
input event world =
  case event of
    EventKey (SpecialKey key) Down _ _ ->
      return $ case key of
        KeyUp -> shiftWorldUp world
        KeyDown -> shiftWorldDown world
        KeyLeft -> shiftWorldLeft world
        KeyRight -> shiftWorldRight world
        _ -> world
    _ -> return world

-- Not used, called to update after input?
modify :: Controller -> IO ()
modify controller =
  return ()

