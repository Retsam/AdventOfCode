import Debug.Trace
import Data.Char

main = interact $
    show
    . maximum
    . (map dist)
    . scanl move Coords { x=0, y=0 }
    . parseInput

data Dir = North | NorthEast | SouthEast | South | SouthWest | NorthWest deriving (Show)
data Coords = Coords {
    x :: Int,
    y :: Int
} deriving (Show)

parseInput :: String -> [Dir]
parseInput = (map parseDir) . split ',' . filter (not . isSpace)

parseDir :: String -> Dir
parseDir str
    | str == "n" = North
    | str == "ne" = NorthEast
    | str == "se" = SouthEast
    | str == "s" = South
    | str == "sw" = SouthWest
    | str == "nw" = NorthWest
    | otherwise = North

-- Utils
type Delimiter = Char
split :: Delimiter -> String -> [String]
split del s = case dropWhile (== del) s of
    "" -> []
    s' -> w : split del s''
          where (w, s'') = break (== del) s'


move :: Coords -> Dir -> Coords
move Coords {x=x', y=y'} d = case (trace (show (x', y')) d) of
    North -> Coords {
        x = x',
        y = y' + 1
    }
    NorthEast -> Coords {
        x = x' + 1,
        y = y'
    }
    SouthEast -> Coords {
        x = x' + 1,
        y = y' - 1
    }
    South -> Coords {
        x = x',
        y = y' - 1
    }
    SouthWest -> Coords {
        x = x' - 1,
        y = y'
    }
    NorthWest -> Coords {
        x = x' - 1,
        y = y' + 1
    }

dist :: Coords -> Int
dist Coords {x=x', y=y'} =
    maximum [(abs x'), (abs y'), (abs $ x' + y')]
