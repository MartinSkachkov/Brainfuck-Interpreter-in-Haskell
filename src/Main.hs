module Main (main) where

import Brainfuck (run)

main :: IO ()
main = do
  putStrLn "Filename: "
  fileName <- getLine
  contents <- readFile fileName
  run contents
