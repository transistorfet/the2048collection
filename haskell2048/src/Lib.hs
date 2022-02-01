module Lib (mainFunc) where

import Debug.Trace

import System.Random
import System.IO.Unsafe
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact

data World = World {
  seed :: StdGen,
  rows :: [[Int]]
}

mainFunc :: IO ()
mainFunc =
  interactIO
    (InWindow "Test" (800, 600) (10, 10))
    (greyN 0.5)     -- background color
    (generate ())   -- initial world
    render          -- function to convert world to a Picture
    input           -- function to handle input events
    modify          -- function to step the world one iteration


generate :: () -> World
generate () =
  World {
    seed = mkStdGen 123,
    rows = [ [ 0, 0, 4, 0]
           , [ 0, 0, 2, 2]
           , [ 0, 0, 2, 4]
           , [ 0, 8, 2, 0] ]
  }


shiftWorldUp :: World -> World
shiftWorldUp world =
  let (rs, newSeed) = randomSequence (seed world) 4 [] in
  let newRows = toColumns (collapseAndFillRows rs (\(rows, zeros) -> rows ++ zeros) (toColumns (rows world))) in
  World { seed = newSeed, rows = newRows }


shiftWorldDown :: World -> World
shiftWorldDown world =
  let (rs, newSeed) = randomSequence (seed world) 4 [] in
  let newRows = toColumns (collapseAndFillRows rs (\(rows, zeros) -> reverse zeros ++ rows) (toColumns (rows world))) in
  World { seed = newSeed, rows = newRows }


shiftWorldLeft :: World -> World
shiftWorldLeft world =
  let (rs, newSeed) = randomSequence (seed world) 4 [] in
  let newRows = collapseAndFillRows rs (\(rows, zeros) -> rows ++ zeros) (rows world) in
  World { seed = newSeed, rows = newRows }


shiftWorldRight :: World -> World
shiftWorldRight world =
  let (rs, newSeed) = randomSequence (seed world) 4 [] in
  let newRows = collapseAndFillRows rs (\(rows, zeros) -> reverse zeros ++ rows) (rows world) in
  World { seed = newSeed, rows = newRows }


collapseAndFillRows :: [Double] -> (([Int], [Int]) -> [Int]) -> [[Int]] -> [[Int]]
collapseAndFillRows rs f rows =
  map (\(i, row) ->
    let newRow = collapseNumbers row
    in f (newRow, (fillZeros (rs !! i) (4 - (length newRow)))))
  (zip [0..] rows)


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


fillZeros :: Double -> Int -> [Int]
fillZeros r 0   = []
fillZeros r 1   = [ if r < 0.1 then 2 else 0 ]
fillZeros r len = 0 : fillZeros r (len - 1)


randomSequence :: StdGen -> Int -> [Double] -> ([Double], StdGen)
randomSequence g 0 list =
  (list, g)
randomSequence g n list =
  let (r, nextg) = randomR (0.0, 1.0) g
  in randomSequence nextg (n - 1) (r : list)


toColumns :: [[Int]] -> [[Int]]
toColumns rows =
  map (\i ->
    map (\row -> (row !! i)) rows)
    [0..3]


render :: World -> IO Picture
render world =
  return $ pictures $ renderAllRows (rows world)


renderAllRows :: [[Int]] -> [Picture]
renderAllRows rows =
  map forEachRow (zip [0..] rows)
  where
    forEachRow (i, entry) =
      pictures $ renderRow (200 + (i * (-200))) entry


renderRow :: Float -> [Int] -> [Picture]
renderRow y row =
  map forEachTile (zip [0..] row)
  where
    forEachTile (i, entry) =
      Translate (-200 + (i * (200))) y $ renderTile entry


renderTile :: Int -> Picture
renderTile number =
  pictures [ color (greyN 0.1) $ rectangleSolid 180 180, color white $ Translate (-180 / 4) (-180 / 4) $ text (show number) ]


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


modify :: Controller -> IO ()
modify controller =
  putStrLn "thing"

