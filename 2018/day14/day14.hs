import Data.Sequence (Seq (..), fromList, (><), (|>))
import qualified Data.Sequence as Seq
import Data.Foldable (toList)
import Data.Maybe (fromJust)
import Data.List
import Data.Char

-- numRecipes = 59414 -- example
numRecipes = 825401 -- puzzle input

main = putStrLn $ show $ part2 initState

type Score = String
data State = State
    { recipes :: Seq Int
    , count :: Int
    , elf1 :: Int
    , elf2 :: Int
    } deriving (Show)

initState = State {
    recipes = fromList [3, 7],
    count = 2,
    elf1 = 0,
    elf2 = 1
}
part1 :: State -> Score
part1 state =
    case skillHasImproved state of
        True -> score state
        False -> part1 (step state)


part2 :: State -> Int
part2 state =
    let
        p = fromList (digits numRecipes)
        (rs :|> r) = recipes state
    in
        if matchesPattern p rs then (length rs) - (length p)
        else if matchesPattern p (rs |> r) then (length rs) - (length p) + 1
        else part2 (step state)

matchesPattern :: Seq Int -> Seq Int -> Bool
matchesPattern Empty _ = True
matchesPattern (ps :|> p) (rs :|> r) = (r == p) && matchesPattern ps rs
matchesPattern _ _ = False


step :: State -> State
step State {recipes = rs, count = c, elf1 = e1, elf2 = e2} =
    let
        r1 = fromJust $ Seq.lookup e1 rs
        r2 = fromJust $ Seq.lookup e2 rs
        addedRecipes = digits (r1 + r2)
        newLen = c + length addedRecipes
        newRecipes = rs >< (fromList addedRecipes)
    in State {
    recipes = newRecipes,
    count = newLen,
    elf1 = (e1 + 1 + r1) `mod` newLen,
    elf2 = (e2 + 1 + r2) `mod` newLen
}

skillHasImproved :: State -> Bool
skillHasImproved state =
    (count $ state) > (numRecipes + 10)

score :: State -> Score
score = undigits . toList . Seq.take 10 . Seq.drop numRecipes . recipes

digits :: Int -> [Int]
digits = map digitToInt . show

undigits :: [Int] -> String
undigits = map intToDigit
