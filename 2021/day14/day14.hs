import Data.Either (fromRight)
import Data.List
import qualified Data.Map as M
import Data.Maybe (fromJust)
import Data.Monoid (Sum (Sum))
import Debug.Trace (trace)
import Text.Parsec

main = do
  input <- getContents
  let parsed = parse parseInput "input" input
  let (template, recipes) = fromRight (error "oops") parsed
  let counts = initCounts template
  print $ solve 10 template recipes
  print $ solve 40 template recipes

-- div 2 because letterCounts double counts everything
solve n template recipes = (maximum letters - minimum letters) `div` 2
  where
    letters = letterCounts template (steps !! n)
    init = initCounts template
    steps = iterate (step recipes) init

pairs text = zipWith (\a b -> [a, b]) text (drop 1 text)

initCounts template =
  M.fromListWith (+) counts
  where
    counts = zip (pairs template) (repeat 1)

pairInsertion recipes pair = pairs $ intersperse newLetter pair
  where
    newLetter = snd $ fromJust $ find ((pair ==) . fst) recipes

step recipes = M.fromListWith (+) . M.foldMapWithKey newCounts
  where
    newCounts pair count = zip (pairInsertion recipes pair) (repeat count)

letterCounts :: String -> M.Map String Integer -> M.Map Char Integer
letterCounts template = M.fromListWith (+) . (++ edges) . M.foldMapWithKey countsFromPair
  where
    edges = [(head template, 1), (last template, 1)]
    countsFromPair key value = zip key $ repeat value

parseInput = do
  template <- many letter
  recipes <- newline >> newline >> parseRecipe `endBy` newline
  return (template, recipes)

parseRecipe :: Parsec String () (String, Char)
parseRecipe = do
  [input, [output]] <- many letter `sepBy` string " -> "
  return (input, output)
