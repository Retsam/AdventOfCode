import Data.Char
import Data.Bits

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
    (++ "\n")
    . foldl1 (++)
    . (map toHex)
    . denseHash
    . list
    . foldl iteration initState
    . foldl1 (++)
    . replicate 64
    . parseInput

-- Utils

showLn :: Show a => a -> String
showLn x = show x ++ "\n"

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
