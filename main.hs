module Main where

import Tokens
import MonadicGrammar
import Interpreter2

main = do text <- readFile "challenge.eql"
          result <- (errorHandle (parseCalc (alexScanTokens text)))
          putStrLn result

errorHandle :: E Prog -> IO String
errorHandle (Ok t) = eval t
errorHandle (Failed s) = return s
