import Control.Monad.Writer
import Data.Bifunctor (Bifunctor (bimap))
import Data.Char (digitToInt)

main = do
  input <-
    getContents
  let stuff = map (map digitToInt) $ lines input
  print $ part1 stuff
  print $ part2 stuff

part1 :: [[Int]] -> Int
part1 initState =
  getSum $ execWriter (states !! 100)
  where
    states = iterate (>>= doRound) $ return initState

part2 :: [[Int]] -> Int
part2 initState =
  statesUntilAllZero initState (allZeros initState) 0

evalWriter = fst . runWriter

statesUntilAllZero state allZeros count =
  if state == allZeros
    then count
    else statesUntilAllZero (evalWriter (doRound state)) allZeros count + 1

allZeros = map $ map $ const 0

doRound :: [[Int]] -> Writer (Sum Int) [[Int]]
doRound grid = do
  let newGrid = step grid
  let toFlash = filter ((== 10) . valueAt newGrid) $ gridCoords newGrid
  countFlashes $ length toFlash
  fmap resetFlashed (flash (newGrid, toFlash))

countFlashes :: Int -> Writer (Sum Int) ()
countFlashes f = tell $ Sum f

resetFlashed :: [[Int]] -> [[Int]]
resetFlashed = map $ map (\val -> if val > 9 then 0 else val)

step :: [[Int]] -> [[Int]]
step = map (map (+ 1))

flash :: ([[Int]], [(Int, Int)]) -> Writer (Sum Int) [[Int]]
flash (grid, []) = return grid
flash (grid, coord : restToFlash) = countFlashes (length newToFlash) >> flash (newGrid, newToFlash ++ restToFlash)
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
