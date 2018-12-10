import Data.Sequence (Seq (..), fromList, viewl)
import Control.Monad
import Control.Monad.State
import qualified Data.Map.Strict as Map
import Data.Ord (comparing)


num_players = 419
num_marbles = 7216400

main = putStrLn $ show $ bestScore scores

type Player = Int
type Marble = Int
type Score = Int
type PlayerScores = Map.Map Player Score


bestScore :: PlayerScores -> Int
bestScore = maximum . map snd . Map.toList

scores :: PlayerScores
scores = execState (foldM placeMarble Empty [0..num_marbles]) Map.empty

forwardTwo :: Seq a -> Seq a
forwardTwo (x1 :<| x2 :<| _xs) = (_xs :|> x1 :|> x2)
forwardTwo x = x

backOne :: Seq a -> Seq a
backOne (_xs :|> x1) = (x1 :<| _xs)
backOne x = x

backSeven :: Seq a -> Seq a
backSeven = head . drop 7 . iterate backOne

removeMarble :: Seq a -> (a, Seq a)
removeMarble (x1 :<| xs) = (x1, xs)
removeMarble Empty = error "Removed from empty circle"

placeMarble :: Seq Int -> Int -> State PlayerScores (Seq Int)
placeMarble circle i
    | i > 0 && i `mod` 23 == 0 =
        let
            (removedMarble, newCircle) = removeMarble $ backSeven circle
            updateScore = updatePlayerScore i removedMarble
        in updateScore >> return newCircle
    | otherwise =  return $ i :<| forwardTwo circle

playerForCurrentMarble :: Marble -> Player
playerForCurrentMarble marble = marble `mod` num_players

updatePlayerScore :: Marble -> Marble -> State PlayerScores ()
updatePlayerScore currentMarble removedMarble =
    let player = playerForCurrentMarble currentMarble
        scoredPoints = currentMarble + removedMarble
    in modify $ Map.insertWith (+) player scoredPoints


-- foldM placeMarble Empty [0..25]
