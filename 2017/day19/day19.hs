import Data.List
import Data.Maybe
import Control.Monad.Writer

main = getContents >>=
    lines
    |> \ls -> (
        let
            pos = findStartPosition ls
            states = takeUntilRepeat $ iterateM (move ls) (return (pos, D))
        in
            -- (sequence . map (putStrLn . show)) states
            putStrLn . show . length $ states
    )

a |> b = b . a
a .> b = b a

takeUntilRepeat [] = []
takeUntilRepeat [a] = [a]
takeUntilRepeat l =
    takeUntilRepeat' [head l] (tail l)

takeUntilRepeat' :: Eq a => [a] -> [a] -> [a]
takeUntilRepeat' checked toCheck =
    let
        (prev:ps) = checked
        (curr:cs) = toCheck
    in if prev == curr
        then reverse checked else
        takeUntilRepeat' (curr:prev:ps) cs


iterateM :: (Monad m) => (a -> m a) -> m a -> [m a]
iterateM f init =
    let
        next = init >>= f
    in next:(iterateM f next)

type Position = (Int, Int) --Row, Column
data Dir = D | L | R | U deriving (Eq, Show)
type State = (Position, Dir)

findStartPosition :: [String] -> Position
findStartPosition =
    head
    |> elemIndex '|'
    |> fromJust
    |> \x -> (0, x)

nth :: Int -> [a] -> Maybe a
nth n list =
    if n >= (length list) || n < 0 then
        Nothing
    else Just (list !! n)

readCharacter :: [String] -> Position -> Char
readCharacter lines (row, col) =
    fromMaybe ' ' ((nth row lines) >>= (nth col))


move :: [String] -> State -> Writer [Char] State
move lines (pos, dir) =
    let
        char = readCharacter lines pos
        forward = (doMove dir pos, dir)
        recur = move lines
    in case char of
        '|' -> return forward
        '-' -> return forward
        ' ' -> return (pos, dir)
        '+' -> let
                (l, r) = perpendicular dir
                (moveL, moveR) = (doMove l pos, doMove r pos)
            in if readCharacter lines moveL /= ' ' then
                    return (moveL, l)
                else if readCharacter lines moveR /= ' ' then
                    return (moveR, r)
                else
                    error ("Couldn't find a turn " ++ (show pos))

        c   -> writer (forward, [c])

-- doTurn :: [String] -> State -> State
-- doTurn lines

doMove :: Dir -> Position -> Position
doMove dir (r, c) =
    case dir of
        U -> (r - 1, c)
        L -> (r, c - 1)
        D -> (r + 1, c)
        R -> (r, c + 1)

perpendicular :: Dir -> (Dir, Dir)
perpendicular dir
    | dir == U || dir == D = (L, R)
    | dir == L || dir == R = (U, D)
