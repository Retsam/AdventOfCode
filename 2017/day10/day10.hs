import Data.Char

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

main :: IO ()
main = interact $
    showLn
    . foldl1 (*) . take 2 . list
    . foldl iteration initState . parseInput

showLn :: Show a => a -> String
showLn x = show x ++ "\n"

parseInput :: String -> [Length]
parseInput = (map read) . split ',' . filter (not . isSpace)

type Delimiter = Char
split :: Delimiter -> String -> [String]
split del s = case dropWhile (== del) s of
    "" -> []
    s' -> w : split del s''
          where (w, s'') = break (== del) s'

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
