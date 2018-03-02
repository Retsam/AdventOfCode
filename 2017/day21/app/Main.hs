module Main where

import Text.ParserCombinators.Parsec
import Data.Char (isSpace)
import Data.List (dropWhileEnd)

main :: IO ()
main =  interact (
    (++ "\n")
    . show
    . (parse parseInput "(unknown)")
    . dropWhileEnd isSpace
    )

data Pixel = On | Off deriving Show
type Grid = [[Pixel]]

data Translation = Translation {
    from :: Grid,
    to :: Grid
} deriving Show

-- MATCHING

-- isMatch :: Grid -> Grid -> Boolean
-- isMatch = undefined

-- PARSING

parseInput = sepBy parseTranslation (char '\n')

parseTranslation = do
    from <- parseGrid
    string " => "
    to <- parseGrid
    return (Translation { from = from, to = to })


parseGrid = sepBy (many1 parseBit) (char '/')

parseBit = choice
    [ char '#' >> (return On)
    , char '.' >> (return Off)
    ]
