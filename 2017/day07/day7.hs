import Data.Char
import qualified Data.Map.Strict as Map
import Data.Maybe

main = interact $ show . findRoot . claimChildren . buildMap . (map parseLine) . lines

tmpShow :: ProgMap -> [(String, String)]
tmpShow = (map (\x -> ((name x), (maybe "NA" name (parent x))))) . Map.elems


data ProgInfo = ProgInfo
    { name :: String
    , weight :: Int
    , children :: [String]
    , parent :: Maybe ProgInfo
    } deriving (Show)

parseWeight :: String -> Int
parseWeight = read . filter isNumber

parseChildren :: [String] -> [String]
parseChildren = map $ filter isAlpha

parseLine :: String -> ProgInfo
parseLine line =
    let
        parts = words line
    in ProgInfo
        { name = parts !! 0
        , weight = parseWeight (parts !! 1)
        , children = parseChildren (drop 3 parts) -- Drop "name (weight) ->"
        , parent = Nothing
        }

type ProgMap = Map.Map String ProgInfo

buildMap :: [ProgInfo] -> ProgMap
buildMap = foldl (\map info -> Map.insert (name info) info map) Map.empty

claimChildren' :: ProgMap -> ProgInfo -> ProgMap
claimChildren' map progInfo = foldl (\map child -> Map.update (Just . setParent progInfo) child map) map (children progInfo)

claimChildren :: ProgMap -> ProgMap
claimChildren map = foldl claimChildren' map map

type Parent = ProgInfo
type Child = ProgInfo
setParent :: Parent -> Child -> Child
setParent parent child = child { parent = Just parent }

findRoot :: ProgMap -> ProgInfo
findRoot = head . filter (isNothing . parent) . Map.elems
