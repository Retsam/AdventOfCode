import Data.List
import Data.Function
import Data.Ord
import Data.Maybe (catMaybes, listToMaybe, isNothing, fromJust)
import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Sequence as Seq
import Data.Sequence (Seq (..), (><))
import Debug.Trace

main = interact (show . fromJust . find battleOver . (iterate tick) . parseInput)

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

-- Readability aliases
type AllMobs = [Mob]
type RoundCount = Int
data GameState = GameState ([Pos], AllMobs, RoundCount)
instance Show GameState where
    show state = showState state

showState :: GameState -> String
showState state@(GameState (squares, mobs, t)) =
    let
        minRow = minimum $ fst <$> squares
        minCol = minimum $ snd <$> squares
        maxRow = maximum $ fst <$> squares
        maxCol = maximum $ snd <$> squares
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
showSquare (GameState (squares, mobs, _)) p =
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

squares :: GameState -> [Pos]
squares (GameState (s, _, _)) = s
mobs :: GameState -> AllMobs
mobs (GameState (_, m, _)) = m


-- squares = [(x,y) | x <- [0..3], y <- [0..3]]
-- mobs = [Mob {race=Elf, pos=(1,2)}, Mob {race=Goblin, pos=(0,0)}, Mob {race=Elf, pos=(3,3)}]
_mobs = [ newMob Elf (0,0), newMob Goblin (3,1)]
_squares = [
    (0,0), (0,1), (0,2),
    (1,0),        (1,2),
    (2,0),        (2,2),
    (3,0), (3,1), (3,2)]
_state = GameState (_squares, _mobs, 0)
_isOpen = squareIsOpen _state

tick :: GameState -> GameState
tick state =
    let
        GameState (newSquares, newMobs, t) =  foldl takeTurn state (sort $ mobs $ state)
    in GameState (newSquares, newMobs, t + 1)

battleOver :: GameState -> Bool
battleOver (GameState (_, mobs, _)) =
    (<2) $ length $ nub $ race <$> mobs

scoreBattle :: GameState -> Int
scoreBattle (GameState (_, mobs, t)) =
    (sum $ hp <$> mobs)


takeTurn :: GameState -> Mob -> GameState
takeTurn state mob =
    -- Check if its died since the round began
    case tapTrace (getSelf state mob) of
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
getSelf (GameState (_, mobs, _)) mob = find (== mob) mobs

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
updateMob (GameState (squares, mobs, t)) newMob =
    let
        newMobs = newMob:(delete newMob mobs) -- Works since Eq on id
    in GameState (squares, newMobs, t)

killMob :: GameState -> Mob -> GameState
killMob (GameState (squares, mobs, t)) victim =
    GameState (squares, delete victim mobs, t)


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
squareIsOpen (GameState (squares, mobs, t)) p =
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
    foldl parseLine (GameState ([], [], 0)) (zip (lines input) [0..] )

parseLine :: GameState -> (String, Int) -> GameState
parseLine state (line, r) =
    foldl (parseChar r) state (zip line [0..])

parseChar :: Int -> GameState -> (Char, Int) -> GameState
parseChar r state@(GameState (squares, mobs, t)) (char, c) =
    case char of
        '#' -> state
        '.' -> GameState ((p:squares), mobs, t)
        'G' -> addMob state p Goblin
        'E' -> addMob state p Elf
        x -> error ("Unexpected char" ++ [x])
    where p = (r,c)

addMob :: GameState -> Pos -> Race -> GameState
addMob (GameState (squares, mobs, t)) pos race =
    GameState (
        (pos:squares),
        ((newMob race pos):mobs),
        t
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
