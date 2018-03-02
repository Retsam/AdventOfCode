import Text.ParserCombinators.ReadP hiding (get)
import qualified Data.Map as Map
import Data.Char
import Data.List
import Control.Monad.Writer


main :: IO ()
main = getContents >>=
    (
        runParser parseInput
        |> (flip loopUntilDeadlock) emptyState
        |> show
        |> putStrLn
    )

type Reg = Char
data Val = Ref Reg | Lit Int deriving Show
data Instruction
    = Set Reg Val
    | Snd Val
    | Add Reg Val
    | Mul Reg Val -- Technically Reg Reg, but why not
    | Mod Reg Val
    | Rcv Reg
    | Jgz Val Val deriving Show
type Queue = [Int]

type Registers = Map.Map Reg Int

a |> b = b . a
a .> b = b a

data DuetState = DuetState {
    inputQueueA :: Queue,
    inputQueueB :: Queue,
    registersA :: Registers,
    registersB :: Registers,
    stackPointerA :: Int,
    stackPointerB :: Int
} deriving (Show)

data ProgramState = ProgramState {
    inputQueue :: Queue,
    outputQueue :: Queue,
    registers :: Registers,
    stackPointer :: Int
} deriving (Show)

emptyState :: DuetState
emptyState = DuetState {
    inputQueueA = [],
    inputQueueB = [],
    registersA = Map.singleton 'p' 0,
    registersB = Map.singleton 'p' 1,
    stackPointerA = 0,
    stackPointerB = 0
}

runParser :: ReadP a -> String -> a
runParser parser input =
    case readP_to_S parser input of
        -- Should be exactly one result, which consumes the entire
        [(xs, "")] -> xs
        [(xs, _)] -> error "Didn't consume whole input"
        _ -> error "Input parse error"

parseInput :: ReadP [ Instruction ]
parseInput = (many (parseInstruction <* (many $ satisfy isSpace))) <* eof

parseRegister :: ReadP Reg
parseRegister = (many $ satisfy isSpace) >> satisfy isLetter

parseValue :: ReadP Val
parseValue =
    (many $ satisfy isSpace) >> choice [
        Lit . read <$> ((++) <$> (option "" (string "-")) <*> (munch1 isNumber)),
        Ref <$> (satisfy isLetter)
    ]

parseInstruction :: ReadP Instruction
parseInstruction =
    choice [
        (string "set") >> Set <$> parseRegister <*> parseValue,
        (string "snd") >> Snd <$> parseValue,
        (string "add") >> Add <$> parseRegister <*> parseValue,
        (string "mul") >> Mul <$> parseRegister <*> parseValue,
        (string "mod") >> Mod <$> parseRegister <*> parseValue,
        (string "rcv") >> Rcv <$> parseRegister,
        (string "jgz") >> Jgz <$> parseValue    <*> parseValue
    ]

readValue :: Registers -> Val -> Int
readValue regs val =
    case val of
        Lit x -> x
        Ref r -> Map.findWithDefault 0 r regs

runProgram :: [Instruction] -> ProgramState -> ProgramState
runProgram instructions state =
    let
        ProgramState { stackPointer = sp, registers = regs, outputQueue = outQueue, inputQueue = inQueue } = state
        ins = instructions !! sp
        readVal = readValue regs
        continue = (\r -> recur $ state { registers = r, stackPointer = (sp + 1)})
        recur = (\s -> runProgram instructions s)
    in
        if sp > length instructions then
            state { inputQueue = [] } -- Terminate
        else case ins of
            Set reg val -> continue (Map.insert reg (readVal val) regs)
            Add reg val -> continue (Map.insertWith (+) reg (readVal val) regs)
            Mul reg ref -> continue (Map.insertWith (*) reg (readVal ref) regs)
            Mod reg ref -> continue (Map.insertWith (flip mod) reg (readVal ref) regs)
            Jgz val off -> if (readVal val)  > 0
                then recur $ state { stackPointer = (sp + readVal off)}
                else continue regs
            Snd val    -> recur $ state {
                stackPointer = (sp + 1),
                outputQueue = outQueue ++ [(readVal val)]
                }
            Rcv reg    -> case uncons inQueue of
                Just (x, xs) -> recur $ state {
                    stackPointer = (sp + 1),
                    outputQueue = outQueue,
                    inputQueue = xs,
                    registers = (Map.insert reg x regs)
                }
                Nothing -> state

runProgramA :: [Instruction] -> DuetState -> DuetState
runProgramA instructions duetState =
    let
        state = progAState duetState
        newState = runProgram instructions state
    in
        updateFromStateA duetState newState

runProgramB :: [Instruction] -> DuetState -> DuetState
runProgramB instructions duetState =
    let
        state = progBState duetState
        newState = runProgram instructions state
    in
        updateFromStateB duetState newState


loopUntilDeadlock :: [Instruction] -> DuetState -> (Int, DuetState)
loopUntilDeadlock instructions duetState =
    let
        state1 = runProgramA instructions duetState
        state2 = runProgramB instructions state1
        (state, sendCount) = runWriter (loopUntilDeadlock' instructions state2)
    in ( (getSum sendCount), state )


loopUntilDeadlock' :: [Instruction] -> DuetState -> Writer (Sum Int) DuetState
loopUntilDeadlock' instructions state0 =
    let
        inputQueueALength = length (inputQueueA state0)
        state1 = runProgramA instructions state0
        inputQueueBEmpty = length (inputQueueB state1) == 0
        state2 = runProgramB instructions state1
    in
        if inputQueueALength == 0 then writer (state0, 0) else
            if inputQueueBEmpty then writer (state0, (Sum inputQueueALength)) else
                (tell (Sum inputQueueALength)) >> loopUntilDeadlock' instructions state2


progAState :: DuetState -> ProgramState
progAState (DuetState {
    inputQueueA = queueA,
    inputQueueB = queueB,
    registersA = reg,
    stackPointerA = sp
}) = ProgramState {
    inputQueue = queueA,
    outputQueue = queueB,
    registers = reg,
    stackPointer = sp
}

progBState :: DuetState -> ProgramState
progBState (DuetState {
    inputQueueA = queueA,
    inputQueueB = queueB,
    registersB = reg,
    stackPointerB = sp
}) = ProgramState {
    inputQueue = queueB,
    outputQueue = queueA,
    registers = reg,
    stackPointer = sp
}

updateFromStateA :: DuetState -> ProgramState -> DuetState
updateFromStateA duetState (ProgramState {
    inputQueue = queueA,
    outputQueue = queueB,
    registers = reg,
    stackPointer = sp
}) = duetState {
    inputQueueA = queueA,
    inputQueueB = queueB,
    registersA = reg,
    stackPointerA = sp
}

updateFromStateB :: DuetState -> ProgramState -> DuetState
updateFromStateB duetState (ProgramState {
    inputQueue = queueB,
    outputQueue = queueA,
    registers = reg,
    stackPointer = sp
}) = duetState {
    inputQueueA = queueA,
    inputQueueB = queueB,
    registersB = reg,
    stackPointerB = sp
}

exampleInput =
    [Set 'a' (Lit 1)
    ,Add 'a' (Lit 2)
    ,Mul 'a' (Ref 'a')
    ,Mod 'a' (Lit 5)
    ,Snd (Ref 'a')
    ,Set 'a' (Lit 0)
    ,Rcv 'a'
    ,Jgz (Ref 'a') (Lit (-1))
    ,Set 'a' (Lit 1)
    ,Jgz (Ref 'a') (Lit (-2))]

exampleInput2 =
    [Snd (Lit 1)
    ,Snd (Lit 2)
    ,Snd (Ref 'p')
    ,Rcv 'a'
    ,Rcv 'b'
    ,Rcv 'c'
    ,Rcv 'd']
