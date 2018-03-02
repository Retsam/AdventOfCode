import Data.Char
import Data.List
-- import Control.Monad.Writer
import qualified Data.Set as Set

main :: IO ()
main = interact $ showLn . length . allGroups . parseInput

showLn :: Show a => a -> String
showLn x = show x ++ "\n"

type Id = Int
type Edge = (Id, Id)
type AllEdges = [Edge]
type VisitedIds = Set.Set Id
type IdsToVisit = [Id]

parseInput :: String -> AllEdges
parseInput = concat . map parseLine . lines

parseLine :: String -> [Edge]
parseLine line =
    let
        parts = words line
        lhs = read $ head parts
        parseRhs = map (read . filter isNumber)
        rhs = parseRhs $ drop 2 parts
    in
        (,) <$> [lhs] <*> rhs

allGroups :: AllEdges -> [Set.Set Id]
allGroups edges = allGroups' edges []

allGroups' :: AllEdges -> [Set.Set Id] -> [Set.Set Id]
allGroups' edges currentGroups =
    case find (`Set.notMember` (mconcat currentGroups)) (map fst edges) of
        Nothing -> currentGroups
        Just notGrouped -> let
                newGroup = groupFromId edges notGrouped
            in allGroups' edges (newGroup : currentGroups)

groupFromId :: AllEdges -> Id -> Set.Set Id
groupFromId edges id = fst $ dfsIter edges (Set.empty, [id])

connectedIds :: AllEdges -> Id -> [Id]
connectedIds edges id = map snd . filter ((== id) . fst) $ edges

dfsIter :: [Edge] -> (VisitedIds, IdsToVisit) -> (VisitedIds, IdsToVisit)
dfsIter _ (idSet, []) = (idSet, [])
dfsIter edges (idSet, id:restOfIds)
    | id `Set.member` idSet = dfsIter edges (idSet, restOfIds)
    | otherwise = let
        newIds = filter (`Set.notMember` idSet) . connectedIds edges $ id
        newSet = id `Set.insert` idSet
    in
        dfsIter edges (newSet, newIds ++ restOfIds)
