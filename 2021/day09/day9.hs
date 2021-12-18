import Data.Bifunctor (Bifunctor (bimap))
import Data.Char
import Data.Maybe (isJust)

main = do
  input <-
    getContents
  let hmap = map (map digitToInt) (lines input)

  print $ part1 hmap

flatMap func list = list >>= func

part1 :: [[Int]] -> Int
part1 hmap = sum $ map sum $ mapGridWithCoords isBasin hmap
  where
    neighborValues = map (valueAt hmap) . neighbors hmap
    isBasin coords v = if all (> v) $ neighborValues coords then v + 1 else 0

-- Grid utils
mapGridWithCoords :: ((Int, Int) -> t -> a) -> [[t]] -> [[a]]
mapGridWithCoords func grid =
  [[func (x, y) (grid !! x !! y) | y <- [0 .. length (head grid) - 1]] | x <- [0 .. length grid - 1]]

neighbors :: [[a]] -> (Int, Int) -> [(Int, Int)]
neighbors hmap (x, y) = filter (inBounds hmap) adjacent
  where
    adjacent = map (bimap (x +) (y +)) [(0, 1), (0, -1), (1, 0), (-1, 0)]

inBounds :: [[a]] -> (Int, Int) -> Bool
inBounds hmap (x, y) = x >= 0 && x < mx && y >= 0 && y < my
  where
    mx = length hmap; my = length (head hmap)

valueAt :: [[a]] -> (Int, Int) -> a
valueAt hmap (x, y) = hmap !! x !! y
