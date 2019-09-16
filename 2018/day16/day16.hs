{-# LANGUAGE NamedFieldPuns #-}
import Data.Bits ((.|.), (.&.))
import Data.Char (isNumber, isSpace)
import Text.ParserCombinators.ReadP (many, ReadP, munch, munch1, string, readP_to_S, sepBy, eof)

type Instruction = (Opcode, Int, Int, Int)
type UnknownInstruction = (Int, Int, Int, Int)

type Testcase = (Registers, UnknownInstruction, Registers)

data Opcode
    = Addr -- Reg A + Reg B
    | Addi -- Reg A + Val B
    | Mulr -- Reg A * Reg B
    | Muli -- Reg A * Val B
    | Banr -- Reg A & Reg B
    | Bani -- Reg A & Val B
    | Borr -- Reg A | Reg B
    | Bori -- Reg A | Val B
    | Setr -- Reg A
    | Seti -- Val A
    | Gtir -- Val A > Reg B
    | Gtri -- Reg A > Val B
    | Gtrr -- Reg A > Reg B
    | Eqri -- Reg A = Val B
    | Eqir -- Val A = Reg B
    | Eqrr -- Reg A = Reg B
    deriving (Show)

allOps = [Addr, Addi, Mulr, Muli, Banr, Bani, Borr, Bori, Setr, Seti, Gtir, Gtri, Gtrr, Eqri, Eqir, Eqrr]

main = do
    input <- getContents
    let (testcases, instructions) = runParser parseInput input
    print $ length $ filter (>=3) $ map (length . opsMatchingTestcase) $ testcases

-- tc :: Testcase
-- tc = (Registers {r0 = 3, r1 = 2, r2 = 1, r3 = 1}, (9,2,1,2), Registers {r0 = 3, r1 = 2, r2 = 2, r3 = 1})

opsMatchingTestcase :: Testcase -> [Opcode]
opsMatchingTestcase tc =
    filter (opMatchesTestcase tc) allOps

opMatchesTestcase :: Testcase -> Opcode -> Bool
opMatchesTestcase (before, (_, a, b, c), after) op =
    (doOp before (op, a, b, c)) == after

-- Parsing
parseInput = do
    testcases <- many testcaseParser
    instructions <- many unknownInstructionParser
    eof
    return (testcases, instructions)


testcaseParser :: ReadP Testcase
testcaseParser = do
    string "Before: "
    before <- registerParser
    parseWhitespace
    instruction <- unknownInstructionParser
    parseWhitespace
    string "After:  "
    after <- registerParser
    parseWhitespace
    return (before, instruction, after)

unknownInstructionParser = do
    [a, b, c, d] <- (sepBy parseInt (string " "))
    parseWhitespace
    return (a, b, c, d)

parseRegisters :: String -> Registers
parseRegisters = runParser registerParser

registerParser :: ReadP Registers
registerParser = do
    string "["
    r0 <- parseInt
    string ", "
    r1 <- parseInt
    string ", "
    r2 <- parseInt
    string ", "
    r3 <- parseInt
    string "]"
    return Registers { r0, r1, r2, r3 }

--- Registers implementation

empty = Registers { r0 = 0, r1 = 1, r2 = 2, r3 = 3}

data Registers
    = Registers {
        r0 :: Int,
        r1 :: Int,
        r2 :: Int,
        r3  :: Int
    } deriving (Show, Eq)

get :: Registers -> Int -> Int
get regs 0 = r0 regs
get regs 1 = r1 regs
get regs 2 = r2 regs
get regs 3 = r3 regs

set :: Registers -> Int -> Int -> Registers
set regs 0 x = regs { r0 = x }
set regs 1 x = regs { r1 = x }
set regs 2 x = regs { r2 = x }
set regs 3 x = regs { r3 = x }


-- Operator implementation
doOp :: Registers -> Instruction -> Registers
doOp state (opcode, a, b, c) =
    set state c (eval (get state) opcode a b)

type Lookup = Int -> Int

test :: Bool -> Int
test b = if b then 1 else 0

eval :: Lookup -> Opcode -> Int -> Int -> Int
eval get Addr a b = (get a) + (get b)
eval get Addi a b = (get a) + b
eval get Mulr a b = (get a) * (get b)
eval get Muli a b = (get a) * b
eval get Banr a b = (get a) .&. (get b)
eval get Bani a b = (get a) .&. b
eval get Borr a b = (get a) .|. (get b)
eval get Bori a b = (get a) .|. b
eval get Setr a b = (get a)
eval get Seti a b = a
eval get Gtir a b = test $ a > (get b)
eval get Gtri a b = test $ (get a) > b
eval get Gtrr a b = test $ (get a) > (get b)
eval get Eqri a b = test $ (get a) == b
eval get Eqir a b = test $ a == (get b)
eval get Eqrr a b = test $ (get a) == (get b)

---Parser utils
runParser :: Show a => ReadP a -> String -> a
runParser parser input =
    case readP_to_S parser input of
        -- Should be exactly one result, which consumes the entire
        [(xs, "")] -> xs
        [(xs, remainder)] -> error ("Didn't consume whole input" ++ remainder)
        r -> error ("Input parse error" ++ (show r))

parseInt :: ReadP Int
parseInt = fmap read (munch1 isNumber)

parseWhitespace :: ReadP String
parseWhitespace = (munch isSpace)
