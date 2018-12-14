import Control.Exception.Base
import Control.Monad
import Control.Monad.State
import Debug.Trace
import Data.Ord  (comparing)
import Data.List
import Data.Maybe (fromJust)
import qualified Data.Map as Map

main = putStrLn (show $ calcMax part2Areas)

type Coord = (Int, Int)
type Area = (Int, Int, Int)
type SummedAreaTable = Map.Map Coord Int

serialNum :: Int
-- serialNum = 42 -- Example
serialNum = 4151 -- Input

squares = [
        (x, y) |
        x <- [1..300],
        y <- [1..300]
    ]

part1Areas = [
        (x, y, z) |
        z <- [3],
        x <- [1..301-z],
        y <- [1..301-z]
    ]
part2Areas = [
        (x, y, z) |
        z <- [1..300],
        x <- [1..301-z],
        y <- [1..301-z]
    ]

-- summed area table logic

summedAreaTable :: SummedAreaTable
summedAreaTable = foldl summedAreaTable' Map.empty squares

summedAreaTable' :: SummedAreaTable -> Coord -> SummedAreaTable
summedAreaTable' map c = Map.insert c (calcAreaSum map c) map

calcAreaSum :: SummedAreaTable -> Coord -> Int
calcAreaSum map (x, y)
    = getAreaSum map (x-1, y)
    + getAreaSum map (x, y-1)
    + getPower (x, y)
    - getAreaSum map (x-1, y-1)

getAreaSum :: SummedAreaTable -> Coord -> Int
getAreaSum t (0, _) = 0
getAreaSum t (_, 0) = 0
getAreaSum t c = t Map.! c

-- Get power logic

getPower :: (Int, Int) -> Int
getPower = fromJust . (flip Map.lookup $ powers)

powers = Map.fromList [
        ((x,y), calcPower x y) |
        x <- [1..300],
        y <- [1..300]
    ]

calcPower :: Int -> Int -> Int
calcPower x y =
    let
        validInput = (x > 0 && x <= 300 && y > 0 && y <= 300)
        rackId = x + 10
        p1 = rackId * y
        p2 = p1 + serialNum
        p3 = p2 * rackId
        p4 = hundredsDigit p3 - 5
    in assert validInput p4

hundredsDigit :: Int -> Int
hundredsDigit x = x `div` 100 `mod` 10

-- Get Power Sum

getPowerSum :: Area -> Int
getPowerSum (x, y, z) =
    getAreaSum summedAreaTable (xx, yy)
    + getAreaSum summedAreaTable (x-1, y-1)
    - getAreaSum summedAreaTable (xx, y-1)
    - getAreaSum summedAreaTable (x-1, yy)
    where
        xx = x + z - 1
        yy = y + z - 1

-- Max calculation
calcMax :: [Area] -> Area
calcMax coords = execState (mapM_ maxByPowerSum coords) (1,1,1)

maxByPowerSum :: Area -> State Area ()
maxByPowerSum = trackMaxBy (comparing getPowerSum)

trackMaxBy :: (a -> a -> Ordering) -> a -> State a ()
trackMaxBy comp value = modify (\x -> (maximumBy comp [value, x]))
