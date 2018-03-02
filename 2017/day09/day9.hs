import Debug.Trace

initState :: StateMachine
initState = StateMachine
    { state = InGroup
    , groupStack = [ emptyGroup ]
    }

main :: IO ()
-- main = interact $ show . scoreGroup . buildGroup . filter (\x -> x /= '\n')
main = interact $ show . scoreGarbage . buildGroup . filter (\x -> x /= '\n')

data State
    = InGarbage
    | IgnoringNextCharacter
    | InGroup deriving (Show)

data Group = Group {
    contents :: [Group],
    garbageScore :: Int
} deriving (Show)
emptyGroup :: Group
emptyGroup = Group { contents = [], garbageScore = 0 }

data StateMachine = StateMachine {
    state :: State,
    groupStack :: [Group]
} deriving (Show)


updateHead :: (a -> a) -> [a] -> [a]
updateHead f a = (f . head $ a) : (drop 1 a)

type GroupToAdd = Group
addGroup :: GroupToAdd -> Group -> Group
addGroup newGroup group =
    group { contents = newGroup : (contents group) }

countGarbage :: Group -> Group
countGarbage group = group { garbageScore = (garbageScore group) + 1}

buildGroup :: String -> Group
buildGroup = head . contents . head -- Unwrap the extra group wrapper
    . groupStack
    . foldl nextState initState

scoreGarbage :: Group -> Int
scoreGarbage group = (garbageScore group) + (sum (map scoreGarbage (contents group)))

scoreGroup :: Group -> Int
scoreGroup = scoreGroup' 1

scoreGroup' :: Int -> Group -> Int
scoreGroup' n =
    (+ n) . sum . map (scoreGroup' (n + 1)) . contents

nextState :: StateMachine -> Char -> StateMachine
nextState machine char =
    -- case (trace (show machine) state machine) of
    case state machine of
        IgnoringNextCharacter -> machine { state = InGarbage }
        InGarbage -> case char of
            '!' -> machine { state = IgnoringNextCharacter }
            '>' -> machine { state = InGroup }
            _ -> machine { groupStack = updateHead countGarbage (groupStack machine)}
        InGroup -> case char of
            ',' -> machine
            '<' -> machine { state = InGarbage }
            '{' -> machine {
                state = InGroup,
                groupStack = emptyGroup : (groupStack machine)
            }
            '}' -> let
                newGroup = head (groupStack machine)
                newGroupStack = updateHead (addGroup newGroup) (drop 1 $ groupStack machine)
                in machine {
                    groupStack = newGroupStack
                }
            _ -> (trace ("Unexpected character " ++ [char]) machine)



