import Data.Ord
import Data.List
import Data.Char as Char
import qualified Data.Set as Set

main = interact (part2 . filter (/= '\n'))

showLn :: Show a => a -> String
showLn = (++"\n") . show

part1 :: String -> String
part1 = showLn . length . completeReduce

part2 = showLn . length . findShortest

-- Part 1

completeReduce :: String -> String
completeReduce = firstDuplicate . iterate reduce

reduce :: String -> String
reduce [] = []
reduce (a:[]) = [a]
reduce (a:b:rest)
    | doesReact a b = reduce rest
    | otherwise = a:(reduce (b:rest))

doesReact :: Char -> Char -> Bool
doesReact a b = (a /= b) && (Char.toLower a == Char.toLower b)

-- Part 2
allPolymers :: String -> Set.Set Char
allPolymers = Set.map (Char.toLower) . Set.fromList

findShortest :: String -> String
findShortest seq = minimumBy (comparing length) . Set.map (completeReduce . excludeCharacter seq) $ allPolymers seq

excludeCharacter :: String -> Char -> String
excludeCharacter seq c = filter ((/= toLower c) . toLower) seq


-- [1,3,3,4] -> [(1,3) (3, 3), (3, 4)]
pairs :: [a] -> [(a, a)]
pairs l = zip l (tail l)

firstDuplicate :: Eq a => [a] -> a
firstDuplicate =
    fst . head . filter (uncurry (==)) . pairs
