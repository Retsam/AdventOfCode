import Text.Printf
import Debug.Trace
import Data.Bits
import Control.Monad
import Control.Monad.Writer

main = (iterate' nextPair
    |> zip [1 ..]
    |> take 5000000
    -- |> map (\(a, b) -> trace (show a) b)
    |> map snd
    |> map testPair
    |> mconcat
    |> getSum
    |> show
    |> putStrLn) initPair

initPair = (516, 190)
-- initPair = (65, 8921)


--UTILS
x |> y = y . x

iterate' x = drop 1 . iterate x


toBinary :: Int -> String
toBinary = printf "%016b"

tapTrace :: Show x => x -> x
tapTrace x = trace (show x) x

---

y `divisibleBy` x = (y `rem` x) == 0

nextPair :: (Int, Int) -> (Int, Int)
nextPair (a, b) = (genANext a, genBNext b)

genANext :: Int -> Int
genANext = head . filter (`divisibleBy` 4) . (iterate' genANext')

genANext' :: Int -> Int
genANext' =
    (* 16807)
    |> (`rem` 2147483647)

genBNext :: Int -> Int
genBNext = head . filter (`divisibleBy` 8) . (iterate' genBNext')

genBNext' :: Int -> Int
genBNext' =
    (* 48271)
    |> (`rem` 2147483647)

lastSixteenBits :: Int -> String
lastSixteenBits =
    (.&. 65535)
    |>  printf "%016b"

testPair :: (Int, Int) -> (Sum Int)
testPair (a, b)
    | (lastSixteenBits a) == (lastSixteenBits b) = (Sum 1)
    | otherwise = (Sum 0)
