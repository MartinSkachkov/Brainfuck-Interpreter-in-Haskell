import Data.Char

-- a command in Brainfuck is one character of the source code
data BrainfuckCommand
  = GoRight -- >
  | GoLeft -- <
  | Incr -- +
  | Decr -- -
  | Print -- .
  | Read -- ,
  | LoopL -- [
  | LoopR -- ]
  | Comment Char -- everything else
  deriving (Show)

-- long line of cells, each holding a number
data Tape = Tape [Int] Int [Int]
  deriving (Show)

-- our program is represented as a list of commands
newtype Program = Program [BrainfuckCommand]
  deriving (Show)

------------------------------------------------------------------- Parser
-- sourceCode = "[->+<]"
parser :: String -> Program
parser sourceCode
  | checkSyntax sourceCode = Program $ convertToCmd sourceCode []
  | otherwise = error "Ivalid syntax error (mismatched brackets)!"

-- sourceCode = [LoopL,Decr,GoRight,Incr,GoLeft,LoopR]
convertToCmd :: String -> [BrainfuckCommand] -> [BrainfuckCommand]
convertToCmd [] cmdLst = cmdLst
convertToCmd (c : cs) cmdLst
  | c == '>' = convertToCmd cs $ cmdLst ++ [GoRight]
  | c == '<' = convertToCmd cs $ cmdLst ++ [GoLeft]
  | c == '+' = convertToCmd cs $ cmdLst ++ [Incr]
  | c == '-' = convertToCmd cs $ cmdLst ++ [Decr]
  | c == '.' = convertToCmd cs $ cmdLst ++ [Print]
  | c == ',' = convertToCmd cs $ cmdLst ++ [Read]
  | c == '[' = convertToCmd cs $ cmdLst ++ [LoopL]
  | c == ']' = convertToCmd cs $ cmdLst ++ [LoopR]
  | otherwise = convertToCmd cs cmdLst -- comments are not needed so we skip them

checkSyntax :: String -> Bool
checkSyntax sourceCode = go sourceCode 0
  where
    go "" cnt = cnt == 0
    go ('[' : cs) cnt = go cs (cnt + 1)
    go (']' : cs) cnt =
      if cnt == 0
        then False -- e.g. "[] ]" or "]["
        else go cs (cnt - 1)
    go (_ : cs) cnt = go cs cnt -- skip non brackets

-----------------------------------------------------------------
-- Tape
emptyTape :: Tape
emptyTape = Tape zeroes 0 zeroes
  where
    zeroes = repeat 0

moveRight :: Tape -> Tape
moveRight (Tape left curr (r : right)) = Tape (left ++ [curr]) r right

moveLeft :: Tape -> Tape
moveLeft (Tape left curr right) = Tape (init left) (last left) (curr : right)

-----------------------------------------------------------------
-- Evaluation