module Main where

import Lexer
import Parser
import Interpreter
import System.Environment
import System.Directory

main = do args <- getArgs
          interpret args

interpret :: [String] -> IO ()
interpret [] = error "Missing argument: Source file\n Usage: myinterpreter <filename>.cql"
interpret [p] = do b <- doesFileExist p
                   interpret' p b
interpret _ = error "Too many arguments, Expected: 1\n Usage: myinterpreter <filename>.cql"

interpret' :: String -> Bool -> IO ()
interpret' p b | b = do text <- readFile p
                        errorHandle (parseCalc (alexScanTokens text))
               | otherwise = error ("Source file does not exist: " ++ p)

errorHandle :: E Prog -> IO ()
errorHandle (Ok t) = do result <- eval t
                        putStrLn result
errorHandle (Failed s) = error s
