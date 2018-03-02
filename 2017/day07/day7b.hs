import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Char
import Data.List
import Data.Maybe
import Debug.Trace

main = interact $ show . buildMap . Set.fromList . (map parseLine) . lines

data ProgInfo = ProgInfo
    { name :: String
    , weight :: Int
    , children :: [String]
    , parent :: Maybe ProgInfo
    } deriving (Show, Eq, Ord)

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

type ProgSet = Set.Set ProgInfo
type ProgMap = Map.Map String ProgInfo

buildMap :: Set.Set ProgInfo -> ProgMap
buildMap set = iteration (set, Map.empty)

iteration :: (ProgSet, ProgMap) -> ProgMap
iteration (set, progMap) =
    let
        (progsToAdd, rest) = Set.partition (allChildrenInMap progMap) set
        newMap = Set.foldl (\map prog -> addProgToMap map prog) progMap progsToAdd
    in if (length rest) > 0 then
        iteration (rest, newMap)
    else
        newMap


allChildrenInMap :: ProgMap -> ProgInfo -> Bool
allChildrenInMap progMap =
    all (\x -> Map.member x progMap) . children

addProgToMap :: ProgMap -> ProgInfo -> ProgMap
addProgToMap progMap prog =
    let
        childrenInfo = map (progMap Map.!) (children prog)
        unbalancedChild = find (\child -> (weight child) /= (weight (head childrenInfo))) childrenInfo

        childrenWeight = (sum (map weight childrenInfo))
        totalWeight = (weight prog) + childrenWeight
        newProg = prog { weight = totalWeight}
        newMap = Map.insert (name prog) newProg . (foldl (.) id (map Map.delete (children prog))) $ progMap
    in
        if isJust unbalancedChild then
            (trace ("UNBALANCE:" ++ show (map (\x -> (name x, weight x)) childrenInfo)) newMap)
        else newMap


