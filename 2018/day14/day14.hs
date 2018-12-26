import Data.Sequence (Seq (..), fromList, (><))
import qualified Data.Sequence as Seq
import Data.Foldable (toList)
import Data.Maybe (fromJust)
import Data.List
import Data.Char

-- numRecipes = 20000 -- example
numRecipes = 825401 -- puzzle input

main = putStrLn $ run initState

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
run :: State -> Score
run state =
    case skillHasImproved state of
        True -> score state
        False -> run (step state)


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
