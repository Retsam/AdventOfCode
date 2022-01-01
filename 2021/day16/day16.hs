import Control.Monad
import Data.Char (digitToInt)
import Data.Functor ((<&>))
import Data.List (foldl')
import Numeric (readHex)
import Text.Parsec
import Text.Printf (printf)

main = do
  input <- hexToBin <$> getContents
  let parsed = parse parsePacket "input" input
  print $ part1 <$> parsed

type Packet = (Int, Payload)

data Payload
  = Value Int
  | Op Int [Packet]
  deriving (Show)

part1 (v, Value _) = v
part1 (v, Op _ packets) = v + sum (map part1 packets)

parsePacket :: Parsec String () Packet
parsePacket = (,) <$> binaryNumber 3 <*> parsePayload

parsePayload :: Parsec String () Payload
parsePayload = do
  kind <- binaryNumber 3
  if kind == 4 then Value <$> parseValue else parseOperator kind

parseOperator x = do
  lengthType <- binDigit
  let subPackets =
        if lengthType == '1'
          then binaryNumber 11 >>= (`count` parsePacket)
          else binaryNumber 15 >>= subpacketsByLength

  Op x <$> subPackets

sourcePos :: Parsec s () SourcePos
sourcePos = statePos `fmap` getParserState

subpacketsByLength :: Int -> Parsec String () [Packet]
subpacketsByLength n = do
  pos <- sourceColumn <$> sourcePos
  p <- parsePacket
  pos2 <- sourceColumn <$> sourcePos
  let r = n - (pos2 - pos)
  if r > 0 then subpacketsByLength r <&> ([p] ++) else return [p]

parseValue = toDec <$> parseValue'

parseValue' :: Parsec String () [Char]
parseValue' = do
  prefix <- binDigit
  seg <- count 4 binDigit
  -- parseValue' >>= (return . (seg ++))
  if prefix == '0' then return seg else parseValue' <&> (seg ++)

binDigit :: Parsec String () Char
binDigit = oneOf "01"

binaryNumber n = toDec <$> count n binDigit

toDec :: String -> Int
toDec = foldl' (\acc x -> acc * 2 + digitToInt x) 0

hexToBin c = c >>= fromHexDigit

fromHexDigit :: Char -> String
fromHexDigit digit = printf "%04b" (v :: Int)
  where
    [(v, _)] = readHex [digit]
