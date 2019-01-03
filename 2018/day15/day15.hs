{-# LANGUAGE NamedFieldPuns #-}
import Data.List
import Data.Function
import Data.Ord
import Data.Maybe (catMaybes, listToMaybe, isNothing, fromJust)
import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Sequence as Seq
import Data.Sequence (Seq (..), (><))
import Debug.Trace

-- main = interact (show . fromJust . find battleOver . (iterate tick) . parseInput)
main = interact (show . tick . parseInput)

data Race = Goblin | Elf deriving (Show, Ord, Eq)
type Pos = (Int, Int) -- Row, Col
type Path = [Pos]

data Mob = Mob
    { pos :: Pos -- First, so mobs sort on position by default
    , race :: Race
    , hp :: Int
    , mobId :: String
    } deriving (Show, Ord)
instance Eq Mob where
    (==) = ((==) `on` mobId)

newMob :: Race -> Pos -> Mob
newMob r p = Mob
    { race = r
    , pos = p
    , mobId = show p
    , hp = 200 }

type IsOpen = Pos -> Bool

type AllMobs = [Mob]
data GameState = GameState {
    squares :: Set Pos,
    mobs :: [Mob],
    roundCount :: Int
}

instance Show GameState where
    show state = showState state

initState :: GameState
initState = GameState {
    squares = Set.empty,
    mobs = [],
    roundCount = 0
}

-- buildState :: Set Pos -> AllMobs -> RoundCount -> GameState
-- buildState pos mobs r ->
    -- GameState pos mobs r

showState :: GameState -> String
showState state@(GameState {squares=squares, mobs=mobs, roundCount=t}) =
    let
        minRow = minimum $ fst `Set.map` squares
        minCol = minimum $ snd `Set.map` squares
        maxRow = maximum $ fst `Set.map` squares
        maxCol = maximum $ snd `Set.map` squares
    in
        "ROUND " ++ show t ++ "\n" ++
        unlines [[showSquare state (r, c)
            | c <- [(minCol - 1)..(maxCol + 1)]]
            | r <- [(minRow - 1)..(maxRow + 1)]
        ] ++ (
            unlines (showHealth <$> sort mobs)
        ) ++ (
            "Score: " ++ (show $ scoreBattle state) ++ " * (" ++ show t ++ " or " ++ show (t-1) ++ ")\n"
        )

showSquare :: GameState -> Pos -> Char
showSquare (GameState {squares, mobs}) p =
    case find ((==p) . pos) mobs of
        Just Mob { race = r } -> case r of
            Goblin -> 'G'
            Elf -> 'E'
        Nothing -> if (p `elem` squares) then '.' else '#'


showMob :: Mob -> String
showMob (Mob { pos = p, race = r}) =
    (show r) ++ " " ++ (show p)

showHealth :: Mob -> String
showHealth mob =
    showMob mob ++ " - " ++ (show $ hp mob)

-- squares = [(x,y) | x <- [0..3], y <- [0..3]]
-- mobs = [Mob {race=Elf, pos=(1,2)}, Mob {race=Goblin, pos=(0,0)}, Mob {race=Elf, pos=(3,3)}]
_mobs = [ newMob Elf (0,0), newMob Goblin (3,1)]
_squares = Set.fromList [
    (0,0), (0,1), (0,2),
    (1,0),        (1,2),
    (2,0),        (2,2),
    (3,0), (3,1), (3,2)]
_state = GameState {squares = _squares, mobs = _mobs, roundCount=0 }
_isOpen = squareIsOpen _state

tick :: GameState -> GameState
tick state =
    let
        newState =  foldl takeTurn state (sort $ mobs $ state)
    in newState {roundCount=roundCount state + 1}

battleOver :: GameState -> Bool
battleOver (GameState {mobs}) =
    (<2) $ length $ nub $ race <$> mobs

scoreBattle :: GameState -> Int
scoreBattle (GameState {mobs}) =
    (sum $ hp <$> mobs)


takeTurn :: GameState -> Mob -> GameState
takeTurn state mob =
    -- Check if its died since the round began
    case getSelf state mob of
        Nothing -> state -- You died, sorry
        Just me -> case adjacentTarget state me of
            Just enemy -> doAttack' me state enemy
            -- Using `me` not `mob` to ensure that the HP is up-to-date
            Nothing ->
                let
                    newState = doMove state me
                    newMe = fromJust $ getSelf newState me
                in case adjacentTarget newState newMe of
                    Nothing -> newState
                    Just enemy -> doAttack' newMe newState enemy

    where doAttack' me state enemy = trace (showHealth me ++ " attacks " ++ showHealth enemy) doAttack state enemy

getSelf :: GameState -> Mob -> Maybe Mob
getSelf (GameState {mobs}) mob = find (== mob) mobs

doMove :: GameState -> Mob -> GameState
doMove state mob =
    case moveTowardTarget state mob of
        Nothing -> trace (showHealth mob ++ " stands still") state
        Just newPos ->
            trace (showHealth mob ++ " moves to " ++ show newPos)
            (updateMob state (mob { pos = newPos }))

doAttack :: GameState -> Mob -> GameState
doAttack state victim =
    if (hp victim) > 3 then
        updateMob state (victim { hp = (hp victim) - 3 })
    else
        killMob state victim

updateMob :: GameState -> Mob -> GameState
updateMob state newMob =
    state { mobs = (newMob:(delete newMob (mobs state))) } -- Works since Eq on id

killMob :: GameState -> Mob -> GameState
killMob state victim =
    state { mobs = delete victim (mobs state) }


isAdjacent :: Mob -> Mob -> Bool
isAdjacent m1 m2 =
    pos m1 `elem` (adjacentSquares $ pos m2)

tapTrace t =
    trace (show t) t

adjacentTarget :: GameState -> Mob -> Maybe Mob
adjacentTarget state mob =
    listToMaybe
    $ (sortBy $ (comparing hp) `mappend` compare) -- Break ties in HP by position
    $ filter (isAdjacent mob)
    $ findTargets (mobs state) mob


sortPaths :: [Path] -> [Path]
sortPaths = sortBy $ (comparing length) `mappend` compare

moveTowardTarget :: GameState -> Mob -> Maybe Pos
moveTowardTarget gameState mob =
    head <$> getPath <$> path
    where
        path = bfs isGoal getNext (PathNode (pos mob, Nothing))
        isGoal (PathNode (p1, _)) = p1 `elem` targetSquares
        getNext = getAdjacentNodes (squareIsOpen gameState)
        targetSquares = findTargetSquares gameState mob


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
squareIsOpen (GameState {squares, mobs}) p =
    let
        isSquare = p `Set.member` squares
        isOccupied = p `elem` (map pos mobs)
    in isSquare && not isOccupied

adjacentSquares :: Pos -> [Pos]
adjacentSquares (r, c) = [(r-1, c), (r, c-1), (r, c+1), (r+1, c)]

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

-- Parse Input
parseInput :: String -> GameState
parseInput input =
    foldl parseLine initState (zip (lines input) [0..] )

parseLine :: GameState -> (String, Int) -> GameState
parseLine state (line, r) =
    foldl (parseChar r) state (zip line [0..])

parseChar :: Int -> GameState -> (Char, Int) -> GameState
parseChar r state (char, c) =
    case char of
        '#' -> state
        '.' -> state {squares = Set.insert p (squares state)}
        'G' -> addMob state p Goblin
        'E' -> addMob state p Elf
        x -> error ("Unexpected char" ++ [x])
    where p = (r,c)

addMob :: GameState -> Pos -> Race -> GameState
addMob state@(GameState {squares, mobs}) pos race =
    state {
        squares = Set.insert pos squares,
        mobs = ((newMob race pos):mobs)
    }

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


_input = "\n\
\###############G.G.#############\n\
\###############...G#############\n\
\##############.....#############\n\
\#############.G....#############\n\
\############.......#############\n\
\###########...##################\n\
\###########G.###################\n\
\#########....###################\n\
\##########..####################\n\
\##########G.###########...######\n\
\###########.G.G.......#....#####\n\
\###########...#####...#....#####\n\
\###########..#######..G.....##.#\n\
\###########.#########..........#\n\
\######..###.#########.G.#.....##\n\
\#####....#..#########....#######\n\
\#####.......#########..#.##..###\n\
\###..##.....#########..#.......#\n\
\###..........#######...........#\n\
\##..GG........#####G.......E...#\n\
\#...#....G...........G.......E.#\n\
\###.#...............E.EE.......#\n\
\###..............G#E......E...##\n\
\##.....G............#.....E..###\n\
\#.G........G..............E..###\n\
\#.#.....######.......E.......###\n\
\##...G...#####....#..#.#..######\n\
\#####..#...###....######..######\n\
\#####.......##..########..######\n\
\######....#.###########...######\n\
\################################"
