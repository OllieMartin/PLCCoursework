module Main where

import Tokens
import Grammar
import Interpreter2

main = do text <- readFile "program.txt"
          result <- (eval (parseCalc (alexScanTokens text)))
          putStrLn result