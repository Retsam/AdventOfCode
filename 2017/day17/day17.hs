import Data.List
import Data.Maybe
import Debug.Trace

input = 369

initState = ([0], 0, 0)

-- main =
--     (iterate (spinLock input) initState)
--     .> find (\(_, _, n) -> (n == 2017))
--     .> fromJust
--     .> (\(b, _, _) -> b)
--     .> elementAfter 2017
--     .> show .> putStrLn

main =
    (iterate (spinLockLite input) (0, 0, 0))
    .> drop 50000000 .> head
    .> show .> putStrLn

f |> g = g . f
a .> b = b a

tapTrace :: Show x => x -> x
tapTrace x = trace (show x) x


type Steps = Int
type Position = Int
type Buffer = [Int]
type Last = Int
type State = (Buffer, Position, Last)

type StateLite = (Int, Position, Last)

spinLock :: Steps -> State -> State
spinLock steps (buffer, pos, last)
    | trace (show last) False = (buffer, pos, last)
    | otherwise =
    let
        toInsert = last + 1
        newPos = (steps + pos) `mod` (length buffer) + 1
        (before, after) = splitAt newPos buffer
    in (before ++ toInsert:after, newPos, toInsert)

spinLockLite :: Steps -> StateLite -> StateLite
spinLockLite steps (afterZero, pos, last) =
    let
        toInsert = last + 1
        newPos = (steps + pos) `mod` toInsert
        newAfterZero = if newPos == 0 then toInsert else afterZero
    in (newAfterZero, newPos + 1, toInsert)

elementAfter :: Int -> Buffer -> Int
elementAfter i buffer =
    buffer !! (1 + (fromJust (elemIndex i buffer)))


