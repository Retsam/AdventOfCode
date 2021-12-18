import Data.Bifunctor (Bifunctor (bimap))
import Data.Char (digitToInt)
import Data.List
import Data.Maybe (isJust)
import qualified Data.Set as S

main = do
  input <-
    getContents
  let hmap = map (map digitToInt) (lines input)

  print $ part2 hmap

isBasin :: [[Int]] -> (Int, Int) -> Bool
isBasin hmap coords = all (> valueAt hmap coords) $ neighborValues coords
  where
    neighborValues = map (valueAt hmap) . neighbors hmap

part1 :: [[Int]] -> Int
part1 hmap = sum $ map sum $ mapGridWithCoords riskValue hmap
  where
    riskValue coords v = if isBasin hmap coords then v + 1 else 0

part2 :: [[Int]] -> Int
part2 hmap = part2Score $ map (basinSize hmap) $ filter (isBasin hmap) $ gridCoords hmap

part2Score :: [Int] -> Int
part2Score = product . take 3 . reverse . sort

basinSize :: [[Int]] -> (Int, Int) -> Int
basinSize hmap coord = fst $ _basinSize (S.singleton coord) hmap coord

_basinSize :: S.Set (Int, Int) -> [[Int]] -> (Int, Int) -> (Int, S.Set (Int, Int))
_basinSize visited hmap coord = foldl (floodAcc hmap) (1, newVisited) toVisit
  where
    newVisited = S.union visited (S.fromList toVisit)
    shouldVisit coord = not (S.member coord visited || valueAt hmap coord == 9)
    toVisit = filter shouldVisit $ neighbors hmap coord

floodAcc hmap (count, set) coord = (count + newCount, newSet)
  where
    (newCount, newSet) = _basinSize set hmap coord

-- Grid utils
gridCoords :: [[a]] -> [(Int, Int)]
gridCoords grid =
  [(x, y) | y <- [0 .. length (head grid) - 1], x <- [0 .. length grid - 1]]

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
