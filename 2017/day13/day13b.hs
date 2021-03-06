import Data.Char;
import Data.List;
import Data.Maybe
import Debug.Trace;
import Data.Monoid;

type Depth = Int
type Range = Int
type Height = Int

data Dir = Up | Down deriving (Show, Eq)

type LayerState = (Depth, Range, Height, Dir)
type State = (Depth, [LayerState])

-- main = putStrLn . show . take 10 . iterate updateFirewall $ (0, 4, 1, Down)
main = interact $ showLn . findPathThroughFirewall . parseInput

showLn :: Show a => a -> String
showLn x = show x ++ "\n"

parseInput :: String -> [LayerState]
parseInput = map parseLine . lines

parseLine :: String -> LayerState
parseLine = tuplify . map (read . filter isNumber) . words

tuplify [x, y] = (x, y, 1, Down)

depth :: LayerState -> Depth
depth (d, _, _, _) = d

findPathThroughFirewall :: [LayerState] -> Int
findPathThroughFirewall layers =
    let
        startPostions = iterate (map updateFirewall) layers
    in
        fromJust
        . findIndex (\(state, x) -> trace (show x) (walkThroughFirewall state))
        $ zip startPostions [0, 1..]

walkThroughFirewall :: [LayerState] -> Bool
walkThroughFirewall layers =
    isNothing
    . find isCaught
    . takeWhile (\(playerDepth, state) -> playerDepth <= (depth (last state)))
    . iterate updateState
    $ (0, layers)

updateState :: State -> State
updateState (playerDepth, firewalls) =
    (playerDepth + 1, (map updateFirewall firewalls))

updateFirewall :: LayerState -> LayerState
updateFirewall (depth, range, height, dir)
    | range == 1 = (depth, 1, 1, Down)
    | height == 1 = (depth, range, 2, Down)
    | dir == Up || height == range = (depth, range, height - 1, Up)
    | otherwise = (depth, range, height + 1, Down)

isCaught :: State -> Bool
isCaught (playerDepth, firewalls) =
    isJust (find (\(d,_,h, _) -> (d == playerDepth && h == 1)) firewalls)
