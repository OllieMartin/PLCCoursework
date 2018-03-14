module Main where

import Tokens
import MonadicGrammar
import Interpreter2

main = do text <- readFile "challenge.eql"
          result <- (eval (parseCalc (alexScanTokens text)))
          putStrLn result
