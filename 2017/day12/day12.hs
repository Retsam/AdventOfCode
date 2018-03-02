import Data.Char
-- import Control.Monad.Writer
import qualified Data.Set as Set
import Debug.Trace

main :: IO ()
main = interact $ showLn . length . idsConnectedToZero . parseInput

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

idsConnectedToZero :: AllEdges -> [Id]
idsConnectedToZero edges = Set.toList . fst $ dfsIter edges (Set.empty, [0])

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
