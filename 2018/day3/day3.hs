import Control.Applicative
import Data.Char
import qualified Data.Map.Strict as Map
import Text.ParserCombinators.ReadP

main = interact (show . part2 . (map parseClaim) . lines)

data Claim = Claim {
    cid :: Int,
    left :: Int,
    top :: Int,
    width :: Int,
    height :: Int
} deriving (Show)

type ClaimMap = Map.Map (Int, Int) Int

part1 :: [Claim] -> Int
part1 = length . Map.filter (> 1) . markClaims

part2 :: [Claim] -> Claim
part2 claims =
    head $ filter (noOverlap (markClaims claims)) claims

-- Part 1

markClaims :: [Claim] -> ClaimMap
markClaims = (Map.unionsWith (+)) . (map markClaim)

markClaim :: Claim -> ClaimMap
markClaim = Map.fromList . (map (flip (,) 1)) . coveredSquares

coveredSquares :: Claim -> [(Int, Int)]
coveredSquares Claim { left = l, top = t, width = w, height = h }
    = liftA2 (,) [l..l+w-1] [t..t+h-1]

-- Part 2
noOverlap :: ClaimMap -> Claim -> Bool
noOverlap claimMap =
    all (== 1) . Map.intersection claimMap . markClaim

--- Parsing

parseClaim = runParser claimParser

claimParser :: ReadP Claim
claimParser = do
    string "#"
    cid <- parseInt
    string " @ "
    left <- parseInt
    string ","
    top <- parseInt
    string ": "
    width <- parseInt
    string "x"
    height <- parseInt
    return Claim { cid = cid, left = left, top = top , width = width, height = height}

runParser :: ReadP a -> String -> a
runParser parser input =
    case readP_to_S parser input of
        -- Should be exactly one result, which consumes the entire
        [(xs, "")] -> xs
        [(xs, _)] -> error "Didn't consume whoe input"
        _ -> error "Input parse error"

parseInt :: ReadP Int
parseInt = fmap read (munch1 isNumber)
