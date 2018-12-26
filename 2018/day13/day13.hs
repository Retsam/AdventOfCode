import Control.Monad.Writer
import Control.Monad
import Data.List
import Data.Maybe

data Dir = North | South | East | West deriving (Show, Eq, Ord)
data NextTurn = L | S | R deriving (Show, Eq, Ord)
type Coord = (Int, Int)
type Cart = (Coord, Dir, NextTurn)

type Track = [String]

-- FIXME: Move carts one at a time, in order, not all at once

main = getContents >>= putStrLn . show . flipTuple . (uncurry run). runWriter . parseTrack
-- main = getContents >>= mapM (putStrLn . show) . (uncurry run). runWriter . parseTrack

flipTuple (a, b) = (b, a)

-- Track Running
run :: Track -> [Cart] -> Coord
run track carts = case step track carts of
    Left collision -> collision
    Right newCarts -> run track newCarts

step :: Track -> [Cart] -> Either Coord [Cart]
step track carts = foldM (step' track) carts (sort carts)

step' :: Track -> [Cart] -> Cart -> Either Coord [Cart]
step' track prevCarts cart =
    let
        newCarts = (delete cart prevCarts) ++ [moveCart track cart]
        maybeCollision = hasCollision newCarts
    in maybe (Right newCarts) Left maybeCollision


moveCart :: Track -> Cart -> Cart
moveCart track (coord, dir, nextTurn)
    = case (getSegment track coord) of
        '|' -> move dir nextTurn
        '-' -> move dir nextTurn
        '+' -> doTurn
        '/' -> move (neCorner dir) nextTurn
        '\\' -> move (nwCorner dir) nextTurn
        _ -> error "derailed"
    where
        move dir nextTurn = (go dir coord, dir, nextTurn)
        doTurn = case nextTurn of
            L -> move (ccw dir) S
            S -> move dir R
            R -> move (cw dir) L

getSegment :: Track -> Coord -> Char
getSegment track (row, col) = track !! row !! col

hasCollision :: [Cart] -> Maybe Coord
hasCollision =  fmap head . find ((>1) . length) . group . sort . map fst3

fst3 :: (a, b, c) -> a
fst3 (a, _, _) = a

-- Track Parsing

parseChar :: Coord -> Char -> Writer [Cart] Char
parseChar coord '>' = writer ('-', [(coord, East, L)])
parseChar coord '<' = writer ('-', [(coord, West, L)])
parseChar coord '^' = writer ('|', [(coord, North, L)])
parseChar coord 'v' = writer ('|', [(coord, South, L)])
parseChar _ char = return char

parseLine :: Int -> String -> Writer [Cart] String
parseLine row = mapM (uncurry parseChar) . zip (zip (repeat row) [0..])

parseTrack :: String -> Writer [Cart] Track
parseTrack = mapM (uncurry parseLine) . zip [0..] . lines

--- Directions

go :: Dir -> Coord -> Coord
go North (row, col) = (row-1, col)
go South (row, col) = (row+1, col)
go East (row, col) = (row, col+1)
go West (row, col) = (row, col-1)


neCorner :: Dir -> Dir
neCorner North = East
neCorner East = North
neCorner South = West
neCorner West = South

nwCorner :: Dir -> Dir
nwCorner North = West
nwCorner West = North
nwCorner South = East
nwCorner East = South

ccw :: Dir -> Dir
ccw North = West
ccw East = North
ccw South = East
ccw West = South

cw :: Dir -> Dir
cw = ccw . ccw . ccw
