import Data.List;
import Data.Maybe;
import Control.Applicative
import qualified Data.Set as Set


type MemoryState = [Int];

main = interact $ show . findRepeatedState Set.empty 0 . parseInput

parseInput :: String -> [Int]
parseInput = (map read) . words . head . lines

--- Utils

minElemIndex :: [Int] -> Int
minElemIndex xs = fromJust (elemIndex (minimum xs) xs)

chunk :: Int -> [a] -> [[a]]
chunk _ [] = []
chunk n l = (take n l) : (chunk n (drop n l))

rpad :: a -> Int -> [a] -> [a]
rpad padding n xs = take n $ xs ++ repeat padding

sumArrays :: (Num a) => [[a]] -> [a]
sumArrays xs = getZipList $ (foldl1 (liftA2 (+)) (map ZipList xs))

---

findRepeatedState :: Set.Set MemoryState -> Int -> MemoryState -> (Int, MemoryState)
findRepeatedState prevStates i state  =
    if Set.member state prevStates then
        (i, state)
    else
        findRepeatedState (Set.insert state prevStates) (i + 1) (nextState state)

-- balanceCycle :: MemoryState -> Int
-- balanceCycle mem =
--     let
--         seenPositionsSet = Set.singleton mem

allStates :: MemoryState -> [MemoryState]
allStates mem = mem : (allStates . nextState $ mem)


nextState :: MemoryState -> [Int]
nextState mem =
    let
        maxValue = maximum mem
        maxIndex = fromJust $ elemIndex maxValue mem
        emptied = emptyMemoryChunk maxIndex mem
        spread = rotate (maxIndex + 1) $ spreadMemoryChunk (length mem) maxValue
    in sumArrays [emptied, spread]

emptyMemoryChunk :: Int -> MemoryState -> MemoryState
emptyMemoryChunk i mem = case splitAt i mem of
                (front, element:back) -> front ++ 0:back


type NumBlocks = Int;
spreadMemoryChunk :: NumBlocks -> Int -> [Int]
spreadMemoryChunk blocks n = sumArrays . chunk blocks . rpad 0 (n * blocks) . take n . repeat $ 1

rotate :: Int -> [a] -> [a]
rotate n xs = take len . drop (len - n) . cycle $ xs
    where len = (length xs)
