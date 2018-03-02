import Control.Applicative hiding (many)
import Control.Monad
import Text.ParserCombinators.ReadP
import Data.Char
import Data.List
import Data.Maybe
import Debug.Trace

-- Part A
-- main =
--     getContents
--     >>= (parseInput
--     |> doDance "abcdefghijklmnop"
--     |> putStrLn)

chars = ['a'..'p']

main =
    getContents
    >>= (parseInput
    |> getCycle
    -- |> (\cycle -> let
    --         i = 1000000000 `mod` (length cycle)
    --     in cycle !! i)
    |> length
    |> show |> putStrLn)

getCycle :: [Move] -> [String]
getCycle moves =
    (iterate ((flip doDance) moves) chars)
    .> tail
    .> takeWhile (/= chars)
    .> (chars:)


f |> g = g . f
a .> b = b a

tapTrace :: Show x => x -> x
tapTrace x = trace (show x) x

data Move
    = Spin Int
    | Exchange Int Int
    | Partner Char Char deriving Show

parseInput :: String -> [Move]
parseInput input =
    case readP_to_S inputParser input of
        -- Should be exactly one result, which consumes the entire
        [(moves, "")] -> moves


inputParser :: ReadP [Move]
inputParser =
    let
        parseMore move = do
            satisfy (== ',')
            rest <- inputParser
            return (move:rest)
        parseLast move = do
            munch isSpace
            eof
            return [move]
    in do
        move <- parseMove
        (parseMore move) <|> (parseLast move)

parseInt :: ReadP Int
parseInt = fmap read (munch1 isNumber)

parseSpin :: ReadP Move
parseSpin = do
    satisfy (== 's')
    x <- parseInt
    return (Spin x)

parseExchange :: ReadP Move
parseExchange = do
    satisfy (== 'x')
    a <- parseInt
    string "/"
    b <- parseInt
    return (Exchange a b)

parsePartner :: ReadP Move
parsePartner = do
    satisfy (== 'p')
    a <- satisfy isLetter
    string "/"
    b <- satisfy isLetter
    return (Partner a b)

parseMove :: ReadP Move
parseMove = parseSpin <|> parseExchange <|> parsePartner

doDance :: String -> [Move] -> String
doDance s = foldl execMove s

execMove :: String -> Move -> String
execMove s move =
    case move of
        Spin x -> let
            (start, end) = splitAt ((length s) - x) s
            in end ++ start
        Exchange a b -> let
            (beforeA, tailA) = splitAt (min a b) s
            (justA, afterA) = splitAt 1 tailA
            (betweenAB, tailB) = splitAt ((max a b) - (min a b) - 1) afterA
            (justB, afterB) = splitAt 1 tailB
            in beforeA ++ justB ++ betweenAB ++ justA ++ afterB
        Partner a b -> execMove s (Exchange (fromJust (elemIndex a s)) (fromJust (elemIndex b s)))


