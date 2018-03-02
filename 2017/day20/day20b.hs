import Control.Applicative
import Text.ParserCombinators.ReadP hiding (get)
import Data.Char
import Data.Ord  (comparing)
import Data.List
import Data.Function
import Control.Monad.Writer
import Control.Monad
import Data.Maybe
import Debug.Trace


a |> b = b . a
a .> b = b a

iterateM :: (Monad m) => (a -> m a) -> m a -> [m a]
iterateM f init =
    let
        next = init >>= f
    in next:(iterateM f next)

main = getContents >>= (
        runParser parseInput
        |> passCount
        |> iterateM tickParticles
        |> find allParticlesGone |> fromJust
        -- |> take 5 |> mapM_ print
        |> runWriter
        |> (\(a, b) -> print (length a, getSum b))
        -- |> mapM_ print
    )

allParticlesGone :: Counter [ParticleData] -> Bool
allParticlesGone = ((==) 0) . length . fst . runWriter

type Counter a = Writer (Sum Int) a
incrCount :: a -> Writer (Sum Int) a
incrCount x = writer (x, 1)

passCount :: a -> Writer (Sum Int) a
passCount x = writer (x, 0)

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

-- tickParticles :: [ParticleData] -> [ParticleData]
tickParticles =
    removeEscapedParticles .
    uniqParticles .
    updateParticles

updateParticles =
    foldl (\list particle -> insertBy (compare `on` p) (tickParticle particle) list) []

-- uniqParticles [] = []
-- uniqParticles particles =
--     foldl (\(p1:ps) p2 -> if isCollision p1 p2 then p1:ps else p2:p1:ps) [head particles] particles

uniqParticles =
    concat . filter (((==) 1) . length) . groupBy ((==) `on` p)

removeEscapedParticles :: [ParticleData] -> Counter [ParticleData]
removeEscapedParticles particles =
    filterM ((liftM not) . hasEscaped stats) particles
    where stats = generateStats particles

isCollision :: ParticleData -> ParticleData -> Bool
isCollision = (==) `on` p

tickParticle :: ParticleData -> ParticleData
tickParticle ParticleData { p = (p1, p2, p3), v=(v1, v2, v3), a=(a1, a2, a3) }
    = ParticleData
        { p = (p1 + v1, p2 + v2, p3 + v3)
        , v = (v1 + a1, v2 + a2, v3 + a3)
        , a = (a1     , a2     , a3     )}

type MinMax = ((Int, Bool), (Int, Bool))
type MinMax3 = (MinMax, MinMax, MinMax)

fakeInf = 2000000
fakeNegInf = -2000000
emptyMinMax = ((fakeInf, True), (fakeNegInf, True))
emptyMM3 = (emptyMinMax, emptyMinMax, emptyMinMax)

data ParticleStats = ParticleStats {
    pStats :: MinMax3,
    vStats :: MinMax3,
    aStats :: MinMax3
} deriving (Show)

generateStats :: [ParticleData] -> ParticleStats
generateStats =
    foldl (\stats part -> ParticleStats {
        pStats = updateStat (pStats stats) (p part),
        vStats = updateStat (vStats stats) (v part),
        aStats = updateStat (aStats stats) (a part)
    }) (ParticleStats emptyMM3 emptyMM3 emptyMM3)

_1of3 (a, _, _) = a
_2of3 (_, b, _) = b
_3of3 (_, _, c) = c

updateStat :: MinMax3 -> V3 -> MinMax3
updateStat stat3 particleData =
    (updateStatDim (_1of3 stat3) (_1of3 particleData),
     updateStatDim (_2of3 stat3) (_2of3 particleData),
     updateStatDim (_3of3 stat3) (_3of3 particleData))

updateStatDim :: MinMax -> Int -> MinMax
updateStatDim prev stat
    = updateStatDimMin (updateStatDimMax prev stat) stat

updateStatDimMin :: MinMax -> Int -> MinMax
updateStatDimMin (prev@(min, minUnique), max) stat
    | stat == min = ((stat, False), max)
    | stat < min = ((stat, True), max)
    | otherwise = (prev, max)

updateStatDimMax :: MinMax -> Int -> MinMax
updateStatDimMax (min, prev@(max, maxUnique)) stat
    | stat == max = (min, (max, False))
    | stat > max = (min, (max, True))
    | otherwise = (min, prev)

hasEscaped :: ParticleStats -> ParticleData -> Counter Bool
hasEscaped ps pd = if hasEscaped' ps pd then incrCount True else passCount False

hasEscaped' :: ParticleStats -> ParticleData -> Bool
hasEscaped' stats particle =
    or ([hasEscapedXMin, hasEscapedXMax, hasEscapedYMin, hasEscapedYMax, hasEscapedZMin, hasEscapedZMax]
    <*> [stats] <*> [particle])

hasEscapedXMin = hasEscapedDim fst _1of3 _1of3
hasEscapedXMax = hasEscapedDim snd _1of3 _1of3
hasEscapedYMin = hasEscapedDim fst _2of3 _2of3
hasEscapedYMax = hasEscapedDim snd _2of3 _2of3
hasEscapedZMin = hasEscapedDim fst _3of3 _3of3
hasEscapedZMax = hasEscapedDim snd _3of3 _3of3

type MinOrMaxAcc = ((Int, Bool), (Int, Bool)) -> (Int, Bool)
type ParticleDimAcc = V3 -> Int
type StatsDimAcc = MinMax3 -> MinMax
hasEscapedDim :: MinOrMaxAcc -> ParticleDimAcc -> StatsDimAcc -> ParticleStats -> ParticleData -> Bool
hasEscapedDim fstOrSnd particleDimAcc statsDimAcc stats particle =
        hasEscapedDim' (particleDimAcc (p particle)) (fstOrSnd (statsDimAcc (pStats stats))) &&
        hasEscapedDimLoose' (particleDimAcc (v particle)) (fstOrSnd (statsDimAcc (vStats stats))) &&
        hasEscapedDimLoose' (particleDimAcc (a particle)) (fstOrSnd (statsDimAcc (aStats stats)))

hasEscapedDim' _ (_, False) = False
hasEscapedDim' v (m, _) = v == m

hasEscapedDimLoose' v (m, _) = v == m

particles = [
    ParticleData {p = (-6,0,0), v = (3,0,0), a = (0,0,0)},
    ParticleData {p = (-4,0,0), v = (2,0,0), a = (0,0,0)},
    ParticleData {p = (-2,0,0), v = (1,0,0), a = (0,0,0)},
    ParticleData {p = (3,0,0), v = (-1,0,0), a = (0,0,0)}
    ]

