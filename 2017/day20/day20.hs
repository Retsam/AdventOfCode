import Control.Applicative
import Text.ParserCombinators.ReadP hiding (get)
import Data.Char
import Data.Ord  (comparing)
import Data.List
import Data.Function

a |> b = b . a
a .> b = b a

main = getContents >>= (
        runParser parseInput
        |> zip [0, 1..]
        |> filter ((== 1) . absoluteAccellerationSum . snd )
        |> minimumBy (compareParticles `on` snd)
        |> show
        |> putStrLn
    )

type V3 = (Int, Int, Int)
data ParticleData = ParticleData {
    p :: V3,
    v :: V3,
    a :: V3
} deriving (Show, Eq)

runParser :: ReadP a -> String -> a
runParser parser input =
    case readP_to_S parser input of
        -- Should be exactly one result, which consumes the entire
        [(xs, "")] -> xs
        [(xs, _)] -> error "Didn't consume whole input"
        _ -> error "Input parse error"

parseInput :: ReadP [ParticleData]
parseInput = sepBy parseLine (munch1 isSpace) <* (munch isSpace) <* eof

parseNumber :: ReadP Int
parseNumber = read <$> ((++) <$> (option "" (string "-")) <*> (munch1 isNumber))

parseLine :: ReadP ParticleData
parseLine = do
    [p, v, a] <- sepBy parseV3 (string ", ")
    return ParticleData { p = p, v = v, a = a }

parseV3 :: ReadP V3
parseV3 = do
    char 'p' <|> char 'v' <|> char 'a'
    string "=<"
    [a, b, c] <- sepBy parseNumber (char ',')
    string ">"
    return (a, b, c)

compareParticles :: ParticleData -> ParticleData -> Ordering
compareParticles = (compare `on` absoluteAccellerationSum) `mappend`
                   (compare `on` weightedVelocitySum)

absoluteAccellerationSum :: ParticleData -> Int
absoluteAccellerationSum ParticleData {a = (a1, a2, a3)} =
    (abs a1) + (abs a2) + (abs a3)

absoluteVelocitySum :: ParticleData -> Int
absoluteVelocitySum ParticleData {v = (v1, v2, v3)} =
    (abs v1) + (abs v2) + (abs v3)

signum' = signum

weightedVelocitySum :: ParticleData -> Int
weightedVelocitySum ParticleData {a = (a1, a2, a3), v = (v1, v2, v3)} =
    (v1 * signum' a1) + (v2 * signum' a2) + (v3 * signum' a3)
