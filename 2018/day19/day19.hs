import Data.Bits ((.|.), (.&.))
import qualified Text.ParserCombinators.ReadP as P
import Text.ParserCombinators.ReadP (ReadP)
import Data.Maybe (fromJust)
import Data.List (find)

main = interact (show . runUntilHalt . parseInput)

type Iptr = Int
type Instruction = (Opcode, Int, Int, Int)
type Program = (Iptr, [Instruction])

data Runstate = Running | Halted deriving (Show, Eq)
type State = (Registers, Runstate)
-- Part 1
-- initRegisters = Registers { r0 = 0, r1 = 0, r2 = 0, r3 = 0, r4 = 0, r5 = 0}
-- Part 2
initRegisters = Registers { r0 = 1, r1 = 0, r2 = 0, r3 = 0, r4 = 0, r5 = 0}
initState = (initRegisters, Running)

runUntilHalt :: Program -> State
runUntilHalt prog =
    fromJust $ find ((== Halted) . snd) $ iterate (stepProgram prog) initState

stepProgram :: Program -> State -> State
stepProgram _ (regs, Halted) = (regs, Halted)
stepProgram (iptr, instructions) (regs, _) =
    let
        iptrVal = get regs iptr
    in case instructions !? iptrVal of
        Just ins -> (incr (doOp regs ins) iptr, Running)
        Nothing -> (regs, Halted)

parseInput :: String -> Program
parseInput input =
    let (header:ll) = lines input
    in (parseHeader header, map parseLine ll)

parseHeader :: String -> Int
parseHeader = read . (!! 1) . words

parseLine :: String -> Instruction
parseLine line =
    let
        [op, a, b, c] = words line
    in (parseOpcode op, read a, read b, read c)

data Registers
    = Registers {
        r0 :: Int,
        r1 :: Int,
        r2 :: Int,
        r3 :: Int,
        r4 :: Int,
        r5 :: Int
    } deriving (Show, Eq)


get :: Registers -> Int -> Int
get regs 0 = r0 regs
get regs 1 = r1 regs
get regs 2 = r2 regs
get regs 3 = r3 regs
get regs 4 = r4 regs
get regs 5 = r5 regs

set :: Registers -> Int -> Int -> Registers
set regs 0 x = regs { r0 = x }
set regs 1 x = regs { r1 = x }
set regs 2 x = regs { r2 = x }
set regs 3 x = regs { r3 = x }
set regs 4 x = regs { r4 = x }
set regs 5 x = regs { r5 = x }

incr :: Registers -> Int -> Registers
incr regs i = set regs i . (+1) . get regs $ i


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
    deriving (Show, Eq)

parseOpcode :: String -> Opcode
parseOpcode "addr" = Addr
parseOpcode "addi" = Addi
parseOpcode "mulr" = Mulr
parseOpcode "muli" = Muli
parseOpcode "banr" = Banr
parseOpcode "bani" = Bani
parseOpcode "borr" = Borr
parseOpcode "bori" = Bori
parseOpcode "setr" = Setr
parseOpcode "seti" = Seti
parseOpcode "gtir" = Gtir
parseOpcode "gtri" = Gtri
parseOpcode "gtrr" = Gtrr
parseOpcode "eqri" = Eqri
parseOpcode "eqir" = Eqir
parseOpcode "eqrr" = Eqrr
parseOpcode _ = error "bad opcode"


(!?) :: [a] -> Int -> Maybe a
a !? b = if b >= 0 && b < (length a) then Just (a !! b) else Nothing
