import Tokens
import Grammar
main = do text <- readFile "program.txt"
          putStrLn (show (parseCalc (alexScanTokens text)))