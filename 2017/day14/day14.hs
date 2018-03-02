import Numeric
import Data.Char
import Text.Printf
import Data.Bits
import Debug.Trace
import Control.Monad
import Control.Monad.Writer

x |> y = y . x
x ||> y = map y . x

main :: IO ()
main = getContents
    >>= filter isLetter
    |> buildGrid
    |> countRegions
    |> show |> putStrLn


gridSize = 127

showLn :: Show a => a -> String
showLn x = show x ++ "\n"

tapTrace :: Show x => x -> x
tapTrace x = trace (show x) x

toBinary :: Int -> String
toBinary = printf "%04b"

hexToBinary :: String -> String
hexToBinary str = str >>= hexToBinary'

hexToBinary' :: Char -> String
hexToBinary' = (:[]) |> readHex |> head |> fst |> toBinary

---
rowKeys :: String -> [String]
rowKeys key = fmap (((key ++ "-") ++) . show) [0..gridSize]

type SquareState = Maybe ()
type GridRow = [SquareState]
type Grid = [GridRow]
type Coords = (Int, Int)

buildGrid :: String -> Grid
buildGrid =
    rowKeys
    ||> (decodeHash |> hexToGridRow)

countRegions :: Grid -> (Grid, Sum Int)
countRegions grid =
    let
        allCoords = (,) <$> [0..gridSize] <*> [0..gridSize]
    in
        runWriter
        . foldM (\grid c -> floodFillGroup c $ grid) grid $ allCoords


hexToGridRow :: String -> GridRow
hexToGridRow =
    hexToBinary
    |> map (\x -> if x == '0' then Nothing else Just ())

inRange :: Int -> Maybe ()
inRange x
    | x >= 0 && x <= gridSize = Just ()
    | otherwise = Nothing

getSquare :: Grid -> Coords -> SquareState
getSquare grid (row, col) = do
    inRange row
    inRange col
    getSquare' grid (row, col)

getSquare' :: Grid -> Coords -> SquareState
getSquare' grid (r, c) = grid !! r !! c

getNeighbors :: Coords -> [Coords]
getNeighbors (r, c) = [(r + 1, c), (r - 1, c), (r, c + 1), (r, c - 1)]

updateList :: Int -> [a] -> (a -> a)  -> [a]
updateList n list f =
    let
        (xs, y:ys) = splitAt n list
    in
        xs ++ (f y):ys

clearCell :: Coords -> Grid -> Grid
clearCell (r, c) grid =
    updateList r grid
        (\row -> updateList c row (return Nothing))


floodFillGroup :: Coords -> Grid -> Writer (Sum Int) Grid
floodFillGroup coords grid =
    case getSquare grid coords of
        Nothing -> writer (grid, Sum 0)
        Just _ -> writer (
                (foldl
                    (\g c -> fst . runWriter . floodFillGroup c $ g)
                    (clearCell coords grid)
                    (getNeighbors coords)
                ),
                Sum 1)


grid = [[Just (), Just (), Nothing, Just (), Nothing, Just (), Nothing, Nothing],
    [Nothing, Just (), Nothing, Just (), Nothing, Just (), Nothing, Just ()],
    [Nothing, Nothing, Nothing, Nothing, Just (), Nothing, Just (), Nothing],
    [Just (), Nothing, Just (), Nothing, Just (), Just (), Nothing, Just ()],
    [Nothing, Just (), Just (), Nothing, Just (), Nothing, Nothing, Nothing],
    [Just (), Just (), Nothing, Nothing, Just (), Nothing, Nothing, Just ()],
    [Nothing, Just (), Nothing, Nothing, Nothing, Just (), Nothing, Nothing],
    [Just (), Just (), Nothing, Just (), Nothing, Just (), Just (), Nothing]]
--- KNOT HASH

decodeHash :: String -> String
decodeHash = foldl1 (++)
    . (map toHex)
    . denseHash
    . list
    . foldl iteration initState
    . foldl1 (++)
    . replicate 64
    . parseInput

type Length = Int
type Position = Int;
type List = [Int]
data State = State
    { position :: Position
    , skipSize :: Int
    , list :: List
    } deriving (Show)

listSize = 256;

initState :: State
initState =
    State { position = 0
    , skipSize = 0
    , list = [0..(listSize - 1)]
    }
-- Utils

chunk :: Int -> [a] -> [[a]]
chunk _ [] = []
chunk n l = (take n l) : (chunk n (drop n l))

--

parseInput :: String -> [Length]
parseInput = (++ [17, 31, 73, 47, 23]) . (map ord) . filter (not . (=='\n'))

iteration :: State -> Length -> State
iteration State { skipSize=skipSize, position=position, list=list } len = State
    { position = (mod (position + len + skipSize) listSize)
    , skipSize = skipSize + 1
    , list = doTwist position len list
    }

doTwist :: Position -> Length -> List -> List
doTwist p len list = let
        rotatedList = take listSize . drop p . cycle $ list
        reversedList = (reverse . take len $ rotatedList) ++ (drop len rotatedList)
        unrotatedList = take listSize . drop (listSize - p) . cycle $ reversedList
    in
        unrotatedList

denseHash :: [Int] -> [Int]
denseHash = (map $ foldl1 xor) . chunk 16

toHex :: Int -> String
toHex x = toHex' (x `div` 16):[toHex' (x `rem` 16)]

toHex' :: Int -> Char
toHex' i | i < 10 = head (show i) | i < 16 = chr (87 + i) | otherwise = 'X'
