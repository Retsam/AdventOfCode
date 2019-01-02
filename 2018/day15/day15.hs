import Data.List
import Data.Function
import Data.Ord
import Data.Maybe (catMaybes, listToMaybe)
import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Sequence as Seq
import Data.Sequence (Seq (..), (><))
import Debug.Trace

data Race = Goblin | Elf deriving (Show, Ord, Eq)
type Pos = (Int, Int) -- Row, Col
type Path = [Pos]

data Mob = Mob
    { pos :: Pos
    , race :: Race
    } deriving (Show, Ord, Eq)

type IsOpen = Pos -> Bool

main = interact (show . tick . parseInput)

-- Readability aliases
type AllMobs = [Mob]
newtype GameState = GameState ([Pos], AllMobs)
instance Show GameState where
    show state = showState state

showState :: GameState -> String
showState state@(GameState (squares, mobs)) =
    let
        minRow = minimum $ fst <$> squares
        minCol = minimum $ snd <$> squares
        maxRow = maximum $ fst <$> squares
        maxCol = maximum $ snd <$> squares
    in
        unlines [[showSquare state (r, c)
            | c <- [(minCol - 1)..(maxCol + 1)]]
            | r <- [(minRow - 1)..(maxRow + 1)]
        ]

showSquare :: GameState -> Pos -> Char
showSquare (GameState (squares, mobs)) p =
    case find ((==p) . pos) mobs of
        Just Mob { race = r } -> case r of
            Goblin -> 'G'
            Elf -> 'E'
        Nothing -> if (p `elem` squares) then '.' else '#'


squares :: GameState -> [Pos]
squares (GameState (s, _)) = s
mobs :: GameState -> AllMobs
mobs (GameState (_, m)) = m

-- squares = [(x,y) | x <- [0..3], y <- [0..3]]
-- mobs = [Mob {race=Elf, pos=(1,2)}, Mob {race=Goblin, pos=(0,0)}, Mob {race=Elf, pos=(3,3)}]
_mobs = [Mob { race=Elf, pos=(0,0)}, Mob {race=Goblin, pos=(3,1)}]
_squares = [
    (0,0), (0,1), (0,2),
    (1,0),        (1,2),
    (2,0),        (2,2),
    (3,0), (3,1), (3,2)]
_state = GameState (_squares, _mobs)
_isOpen = squareIsOpen _state

tick :: GameState -> GameState
tick state = foldl takeTurn state (sort $ mobs $ state)

takeTurn :: GameState -> Mob -> GameState
takeTurn state mob =
    case adjacentTarget state mob of
        Just enemy -> trace (show mob ++ " attacks " ++ show enemy) state  -- Todo: attack
        Nothing -> doMove state mob

doMove :: GameState -> Mob -> GameState
doMove state mob =
    case moveTowardTarget state mob of
        Nothing -> state
        Just newPos -> updateMob state mob (mob { pos = newPos })

updateMob :: GameState -> Mob -> Mob -> GameState
updateMob (GameState (squares, mobs)) oldMob newMob =
    let
        newMobs = newMob:(delete oldMob mobs)
    in GameState (squares, newMobs)

isAdjacent :: Mob -> Mob -> Bool
isAdjacent m1 m2 =
    pos m1 `elem` (adjacentSquares $ pos m2)

adjacentTarget :: GameState -> Mob -> Maybe Mob
adjacentTarget state mob =
    listToMaybe $ sort
    $ filter (isAdjacent mob)
    $ findTargets (mobs state) mob


sortPaths :: [Path] -> [Path]
sortPaths = sortBy $ (comparing length) `mappend` compare

moveTowardTarget :: GameState -> Mob -> Maybe Pos
moveTowardTarget gameState mob =
    head <$> shortestPath
    where
        isOpen = squareIsOpen gameState
        targetSquares = findTargetSquares gameState mob
        possiblePaths = catMaybes $ findPath isOpen (pos mob) <$> targetSquares
        shortestPath = listToMaybe $ sortPaths $ possiblePaths

findTargetSquares :: GameState -> Mob -> [Pos]
findTargetSquares gameState =
    filter (squareIsOpen gameState)
        . nub
        . concatMap adjacentSquares
        . map pos
        . findTargets (mobs gameState)

findTargets :: AllMobs -> Mob -> [Mob]
findTargets allMobs mob =
    filter (((/=) `on` race) mob) allMobs

squareIsOpen :: GameState -> Pos -> Bool
squareIsOpen (GameState (squares, mobs)) p =
    p `elem` squares &&
    not (p `elem` (map pos mobs))

adjacentSquares :: Pos -> [Pos]
adjacentSquares (r, c) = [(r-1, c), (r, c-1), (r, c+1), (r+1, c)]

dist :: IsOpen -> Pos -> Pos -> Int
dist isOpen p1 p2 = length $ findPath isOpen p1 p2


type PrevNode = Maybe PathNode
newtype PathNode = PathNode (Pos, PrevNode) deriving (Show)
-- Ignores the PrevNode for equality checking and ordering
instance Eq PathNode where
    PathNode (p1, _) == PathNode (p2, _) = p1 == p2
instance Ord PathNode where
    PathNode (p1, _) <= PathNode (p2, _) = p1 <= p2

getAdjacentNodes :: IsOpen -> PathNode -> [PathNode]
getAdjacentNodes isOpen node@(PathNode (pos, _)) =
    let
        buildNode from to = PathNode (to, Just from)
    in buildNode node <$> filter isOpen (adjacentSquares pos)

getPath :: PathNode -> [Pos]
getPath = reverse . getPath'
getPath' :: PathNode -> [Pos]
getPath' (PathNode (p, Nothing)) = [] -- don't include the initial square in the path
getPath' (PathNode (p, Just p2)) = (p:(getPath' p2))

findPath :: IsOpen -> Pos -> Pos -> Maybe [Pos]
findPath isOpen from to =
    let
        isTo = (== PathNode (to, Nothing))
    in getPath <$> bfs isTo (getAdjacentNodes isOpen) (PathNode (from, Nothing))

-- Parse Input
parseInput :: String -> GameState
parseInput input =
    foldl parseLine (GameState ([], [])) (zip (lines input) [0..] )

parseLine :: GameState -> (String, Int) -> GameState
parseLine state (line, r) =
    foldl (parseChar r) state (zip line [0..])

parseChar :: Int -> GameState -> (Char, Int) -> GameState
parseChar r state@(GameState (squares, mobs)) (char, c) =
    case char of
        '#' -> state
        '.' -> GameState ((p:squares), mobs)
        'G' -> addChar state p Goblin
        'E' -> addChar state p Elf
        x -> error ("Unexpected char" ++ [x])
    where p = (r,c)

addChar :: GameState -> Pos -> Race -> GameState
addChar (GameState (squares, mobs)) pos race =
    GameState (
        (pos:squares),
        ((Mob {pos=pos, race=race}):mobs)
    )

--- BFS algorithm

bfs :: Show a => Ord a => (a -> Bool) -> (a -> [a]) -> a -> Maybe a
bfs test getNext init = bfs' test getNext  Set.empty (Seq.singleton init)

bfs' :: Show a => Ord a => (a -> Bool) -> (a -> [a]) -> Set a -> Seq a -> Maybe a
bfs' _ _ _ Empty = Nothing
bfs' test getNext visited (current :<| rest) =
    if test current then
        Just current
    else let
        newVisited = Set.insert current visited
        nextItems = filter (not . (`Set.member` newVisited)) (getNext current)
        newFrontier = rest >< (Seq.fromList nextItems)
    in bfs' test getNext newVisited newFrontier
