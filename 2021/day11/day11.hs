import Control.Monad.ST (runST)
import Control.Monad.State
import Data.Bifunctor (Bifunctor (bimap))
import Data.Char (digitToInt)

main = do
  input <-
    getContents
  let stuff = map (map digitToInt) $ lines input
  let states = iterate (>>= doRound) $ return stuff
  let zeroesGrid = allZeros stuff
  print $ part1 states
  -- takeWhile ((/= allZeros) . (`evalState` 0)) states
  -- print $ (map ((/= zeroesGrid) . (`evalState` 0)) states) !! 194

  print $ part2 states $ allZeros stuff

part1 :: [State Int a] -> Int
part1 states =
  execState (states !! 100) 0

part2 :: [State Int [[Int]]] -> [[Int]] -> Int
part2 states allZeros =
  length $ takeWhile ((/= allZeros) . (`evalState` 0)) states

allZeros = map $ map $ const 0

doRound :: [[Int]] -> State Int [[Int]]
doRound grid = do
  let newGrid = step grid
  let toFlash = filter ((== 10) . valueAt newGrid) $ gridCoords newGrid
  countFlashes $ length toFlash
  fmap resetFlashed (flash (newGrid, toFlash))

countFlashes f = do
  count <- get
  put (count + f)

resetFlashed :: [[Int]] -> [[Int]]
resetFlashed = map $ map (\val -> if val > 9 then 0 else val)

step :: [[Int]] -> [[Int]]
step = map (map (+ 1))

flash :: ([[Int]], [(Int, Int)]) -> State Int [[Int]]
flash (grid, []) = return grid
flash (grid, coord : restToFlash) =
  do
    countFlashes $ length newToFlash
    flash (newGrid, newToFlash ++ restToFlash)
  where
    toUpdate = neighbors grid coord
    newToFlash = filter ((== 9) . valueAt grid) toUpdate
    newGrid = mapGridWithCoords updateCoord grid
    updateCoord coord val = if coord `elem` toUpdate then val + 1 else val

newValue grid coord val = val + length (map ((== 9) . valueAt grid) (neighbors grid coord))

gridCoords :: [[a]] -> [(Int, Int)]
gridCoords grid =
  [(x, y) | y <- [0 .. length (head grid) - 1], x <- [0 .. length grid - 1]]

mapGridWithCoords :: ((Int, Int) -> t -> a) -> [[t]] -> [[a]]
mapGridWithCoords func grid =
  [[func (x, y) (grid !! x !! y) | y <- [0 .. length (head grid) - 1]] | x <- [0 .. length grid - 1]]

neighborCoords = [(x, y) | x <- [-1 .. 1], y <- [-1 .. 1]]

neighbors :: [[a]] -> (Int, Int) -> [(Int, Int)]
neighbors hmap (x, y) = filter (inBounds hmap) adjacent
  where
    -- adjacent = map (bimap (x +) (y +)) [(0, 1), (0, -1), (1, 0), (-1, 0)]
    adjacent = map (bimap (x +) (y +)) neighborCoords

inBounds :: [[a]] -> (Int, Int) -> Bool
inBounds hmap (x, y) = x >= 0 && x < mx && y >= 0 && y < my
  where
    mx = length hmap; my = length (head hmap)

valueAt :: [[a]] -> (Int, Int) -> a
valueAt hmap (x, y) = hmap !! x !! y

setValue :: (Int, Int) -> a -> [[a]] -> [[a]]
setValue c1 v1 = mapGridWithCoords (\c2 v2 -> if c1 == c2 then v1 else v2)
