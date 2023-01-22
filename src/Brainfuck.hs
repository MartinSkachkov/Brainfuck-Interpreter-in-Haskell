module Brainfuck where

import Data.Char hiding (isNumber)
import System.IO
import Prelude hiding (Left, Right)

-- a command in Brainfuck is one character of the source code
data BrainfuckCommand
  = Right -- >
  | Left -- <
  | Incr -- +
  | Decr -- -
  | Print -- .
  | Read -- ,
  | LoopL -- [
  | LoopR -- ]
  | Comment Char -- everything else
  deriving (Show)

-- long line of cells, each holding a number
data Tape a = Tape [a] a [a]
  deriving (Show)

-- our program is represented as a list of commands
newtype Program = Program [BrainfuckCommand]
  deriving (Show)

-------------------------------------------------------------------
-- Parser
-- sourceCode = "[->+<]"
parser :: String -> Program
parser "" = error "Empty program!"
parser sourceCode
  | checkSyntax sourceCode = validate $ Program $ convertToCmd sourceCode []
  | otherwise = error "Ivalid syntax error (mismatched brackets)!"

-- sourceCode = [LoopL,Decr,Right,Incr,Left,LoopR]
convertToCmd :: String -> [BrainfuckCommand] -> [BrainfuckCommand]
convertToCmd "" cmdLst = cmdLst
convertToCmd (c : cs) cmdLst
  | c == '>' = convertToCmd cs $ cmdLst ++ [Right]
  | c == '<' = convertToCmd cs $ cmdLst ++ [Left]
  | c == '+' = convertToCmd cs $ cmdLst ++ [Incr]
  | c == '-' = convertToCmd cs $ cmdLst ++ [Decr]
  | c == '.' = convertToCmd cs $ cmdLst ++ [Print]
  | c == ',' = convertToCmd cs $ cmdLst ++ [Read]
  | c == '[' = convertToCmd cs $ cmdLst ++ [LoopL]
  | c == ']' = convertToCmd cs $ cmdLst ++ [LoopR]
  | otherwise = convertToCmd cs cmdLst -- comments are not needed so we skip them

-- if the program only contains comments then there is nothing to interpret
validate :: Program -> Program
validate (Program []) = error "Program contains only comments!"
validate (Program instrs) = Program instrs

-----------------------------------------------------------------
-- Utils

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

isNumber :: String -> Bool
isNumber "" = False
isNumber "." = False
isNumber xs =
  case dropWhile isDigit xs of
    "" -> True
    ('.' : ys) -> all isDigit ys
    _ -> False

findLoopL :: Int -> Tape Int -> Tape BrainfuckCommand -> IO ()
findLoopL 1 dataTape instrTape@(Tape _ LoopL _) = next dataTape instrTape
findLoopL balance dataTape instrTape@(Tape _ LoopL _) = findLoopL (balance - 1) dataTape (moveLeft instrTape)
findLoopL balance dataTape instrTape@(Tape _ LoopR _) = findLoopL (balance + 1) dataTape (moveLeft instrTape)
findLoopL balance dataTape instrTape = findLoopL balance dataTape (moveLeft instrTape)

findLoopR :: Int -> Tape Int -> Tape BrainfuckCommand -> IO ()
findLoopR 1 dataTape instrTape@(Tape _ LoopR _) = next dataTape instrTape
findLoopR balance dataTape instrTape@(Tape _ LoopR _) = findLoopR (balance - 1) dataTape (moveRight instrTape)
findLoopR balance dataTape instrTape@(Tape _ LoopL _) = findLoopR (balance + 1) dataTape (moveRight instrTape)
findLoopR balance dataTape instrTape = findLoopR balance dataTape (moveRight instrTape)

-----------------------------------------------------------------
-- Tape & operations
-- emptyTape = [0 ..] 0 [0 ..]
emptyTape :: Tape Int
emptyTape = Tape zeroes 0 zeroes
  where
    zeroes = repeat 0

moveLeft :: Tape a -> Tape a
moveLeft (Tape (l : left) curr right) = Tape left l (curr : right)

moveRight :: Tape a -> Tape a
moveRight (Tape left curr (r : right)) = Tape (curr : left) r right

incrData :: Tape Int -> Tape Int
incrData (Tape left curr right) = Tape left (curr + 1) right

decrData :: Tape Int -> Tape Int
decrData (Tape left curr right) = Tape left (curr - 1) right

printData :: Tape Int -> Tape BrainfuckCommand -> IO ()
printData dataTape@(Tape _ curr _) instrTape = do
  putChar $ chr curr -- print the current char in the cell
  hFlush stdout -- flush the buffer
  next dataTape instrTape -- move to the next instruction

readData :: Tape Int -> Tape BrainfuckCommand -> IO ()
readData dataTape@(Tape left _ right) instrTape = do
  putStrLn "Please enter a number:"
  input <- getLine
  if isNumber input
    then next (Tape left (read input :: Int) right) instrTape
    else error "Input is not a number!"

loopL :: Tape Int -> Tape BrainfuckCommand -> IO ()
loopL dataTape@(Tape _ curr _) instrTape
  | curr == 0 = findLoopR 0 dataTape instrTape -- if the current data at the cell is zero then go to the correspomding LoopR
  | otherwise = next dataTape instrTape -- execute the instructions in the [...] loop

loopR :: Tape Int -> Tape BrainfuckCommand -> IO ()
loopR dataTape@(Tape _ curr _) instrTape
  | curr /= 0 = findLoopL 0 dataTape instrTape -- if the curr data in not zero then we should continue executing the loop
  | otherwise = next dataTape instrTape

-----------------------------------------------------------------
-- Evaluation

-- execute the program(instructions) onto the dataTape
-- sourceCode = "->+<"
-- Program [Decr,Right,Incr,Left]
-- instrTape = [] Decr [Right,Incr,Left]
runBrainfuck :: Program -> IO ()
runBrainfuck (Program (c : cs)) = execute emptyTape instrTape
  where
    instrTape = Tape [] c cs

-- execute function evaluates one instruction
execute :: Tape Int -> Tape BrainfuckCommand -> IO ()
execute dataTape instrTape@(Tape _ Left _) = next (moveLeft dataTape) instrTape
execute dataTape instrTape@(Tape _ Right _) = next (moveRight dataTape) instrTape
execute dataTape instrTape@(Tape _ Incr _) = next (incrData dataTape) instrTape
execute dataTape instrTape@(Tape _ Decr _) = next (decrData dataTape) instrTape
execute dataTape instrTape@(Tape _ Print _) = printData dataTape instrTape
execute dataTape instrTape@(Tape _ Read _) = readData dataTape instrTape
execute dataTape instrTape@(Tape _ LoopL _) = loopL dataTape instrTape
execute dataTape instrTape@(Tape _ LoopR _) = loopR dataTape instrTape
execute dataTape instrTape@(Tape _ (Comment _) _) = next dataTape instrTape

-- next function moves on to the next instruction in the instruction tape
next :: Tape Int -> Tape BrainfuckCommand -> IO ()
next dataTape (Tape _ _ []) = return () -- reached the end of the instruction tape
next dataTape instrTape = execute dataTape (moveRight instrTape)

-----------------------------------------------------------------
-- Running

run :: String -> IO ()
run input = runBrainfuck $ parser input
