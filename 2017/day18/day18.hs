import Text.ParserCombinators.ReadP
import Control.Applicative hiding (many)
import Control.Monad.Writer
import Data.Char
import Data.List
import qualified Data.Map as Map
import Debug.Trace

a |> b = b . a
a .> b = b a

main = getContents >>=
    (
        putStrLn . show
        . runUntilRecover
        . runParser parseInput
        . filter (not . (=='\n'))
    )


type Reg = Char
data Val = Ref Reg | Lit Int deriving Show
data Instruction
    = Set Reg Val
    | Snd Val
    | Add Reg Val
    | Mul Reg Val -- Technically Reg Reg, but why not
    | Mod Reg Val
    | Rcv Val
    | Jgz Val Val deriving Show

type Registers = Map.Map Reg Int

data Event
    = Send Int
    | Recover deriving Show

type State = (Registers, Int)

initState :: State
initState = (Map.empty, 0)

runParser :: ReadP a -> String -> a
runParser parser input =
    case readP_to_S parser input of
        -- Should be exactly one result, which consumes the entire
        [(xs, "")] -> xs
        [(xs, _)] -> error "Didn't consume whoe input"
        _ -> error "Input parse error"

parseInput = (many parseInstruction) <* eof

parseRegister :: ReadP Reg
parseRegister = (many $ satisfy isSpace) >> satisfy isLetter

parseValue :: ReadP Val
parseValue =
    (many $ satisfy isSpace) >> choice [
        Lit . read <$> ((++) <$> (option "" (string "-")) <*> (munch1 isNumber)),
        Ref <$> (satisfy isLetter)
    ]

parseInstruction =
    choice [
        (string "set") >> Set <$> parseRegister <*> parseValue,
        (string "snd") >> Snd <$> parseValue,
        (string "add") >> Add <$> parseRegister <*> parseValue,
        (string "mul") >> Mul <$> parseRegister <*> parseValue,
        (string "mod") >> Mod <$> parseRegister <*> parseValue,
        (string "rcv") >> Rcv <$> parseValue,
        (string "jgz") >> Jgz <$> parseValue    <*> parseValue
    ]

readValue :: Registers -> Val -> Int
readValue regs val =
    case val of
        Lit x -> x
        Ref r -> Map.findWithDefault 0 r regs

executeInstruction :: State -> Instruction -> Writer [Event] State
executeInstruction (regs, i) instruction =
    let
        readVal = readValue regs
        continue = (\v -> return (v, i + 1))
    in case instruction of
        Set reg val -> continue (Map.insert reg (readVal val) regs)
        Snd val     -> writer   ((regs, i+1), [Send (readVal val)])
        Add reg val -> continue (Map.insertWith (+) reg (readVal val) regs)
        Mul reg ref -> continue (Map.insertWith (*) reg (readVal ref) regs)
        Mod reg ref -> continue (Map.insertWith (flip mod) reg (readVal ref) regs)
        Rcv val     -> if (readVal val) /= 0 then (writer ((regs, i+1), [Recover])) else continue regs
        Jgz val off -> if (readVal val)  > 0 then  return (regs, i + (readVal off)) else continue regs

step :: [Instruction] -> State -> Writer [Event] State
step instructions (regs, i) =
    let
        ins = instructions !! (i `mod` (length instructions))
    in
        executeInstruction (regs, i) ins


iterateM :: (Monad m) => (a -> m a) -> m a -> [m a]
iterateM f init =
    let
        m = init >>= f
    in
        m:(iterateM f m)

isRecover :: Event -> Bool
isRecover Recover = True
isRecover _ = False

isSend :: Event -> Bool
isSend (Send _) = True
isSend _ = False

hasRecovered' :: [Event] -> Bool
hasRecovered' =
    dropWhile isRecover
    |> any isRecover

hasRecovered :: Writer [Event] State -> Bool
hasRecovered =
    runWriter
    |> snd
    |> hasRecovered'

runUntilRecover instructions =
    find hasRecovered (iterateM (step instructions) (return initState))
