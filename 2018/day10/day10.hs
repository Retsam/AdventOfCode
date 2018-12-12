import Text.ParserCombinators.ReadP
import Data.Char
import Data.Function (on)

avg_dist_thresold = 8000

main = getContents >>= mapM_ putScene . filterScenes . zip [0..] . iterate tick . parseInput

putScene :: IndexedScene -> IO ()
putScene (i, scene) = do
    putStrLn (show i)
    putStrLn (draw scene)


parseInput = map (runParser pointParser) . lines

type V2 = (Int, Int)
type Position = V2;
type Velocity = V2;
type Point = (Position, Velocity)
type Scene = [Point]
type IndexedScene = (Int, Scene)

filterScenes :: [IndexedScene] -> [IndexedScene]
filterScenes =
    takeWhile (\(_, ps) -> avgDistance ps < avg_dist_thresold)
    . dropWhile (\(_, ps) -> avgDistance ps > avg_dist_thresold)

tickPoint :: Point -> Point
tickPoint ((p1, p2), (v1, v2)) = ((p1+v1, p2+v2), (v1, v2))

tick :: [Point] -> [Point]
tick = map tickPoint

pointParser :: ReadP Point
pointParser =
    fmap (\(p1:p2:v1:v2:[]) -> ((p1, p2), (v1, v2))) numbersParser

---

dist :: Position -> Position -> Int
dist (x1, y1) (x2, y2) = abs (y2 - y1) + abs (x2 - x1)

avg :: [Int] -> Int
avg x = sum x `div` length x

avgDistance' :: [Position] -> Int
avgDistance' points =
    let c = centerPoint points
    in sum (map (dist c) points)

avgDistance :: [Point] -> Int
avgDistance = avgDistance' . map fst

centerPoint :: [Position] -> Position
centerPoint points = (avg (map fst points), avg (map snd points))

---

cw = 100
ch = 6


canvas :: [Position] -> [[Position]]
canvas points =
    let (cx, cy) = centerPoint points
    in [
        [
            (x, y) | x <- [cx-cw..cx+cw]
        ] | y <- [cy-ch..cy+ch]
    ]


drawPoint :: [Position] -> Position -> Char
drawPoint points x = if x `elem` points then '#' else '.'

draw' :: [Position] -> String
draw' points = unlines (map (map (drawPoint points)) (canvas points))

draw :: [Point] -> String
draw = draw' . map fst

--- Numbers parser

skipNonNumberChars :: ReadP ()
skipNonNumberChars = skipMany $ satisfy (not . isMinusOrNumber)

isMinusOrNumber :: Char -> Bool
isMinusOrNumber x = x == '-' || isNumber x

numbersParser = between skipNonNumberChars (skipNonNumberChars >> eof) (sepBy1 numberParser skipNonNumberChars)

numberParser :: ReadP Int
numberParser = fmap read parser
    where
        parser = (++) <$> minusParser <*> munch1 isNumber
        minusParser = option "" (string "-")

runParser :: ReadP a -> String -> a
runParser parser input =
    case readP_to_S parser input of
        -- Should be exactly one result, which consumes the entire
        [(xs, "")] -> xs
        [(xs, _)] -> error "Didn't consume whole input"
        _ -> error "Input parse error"

