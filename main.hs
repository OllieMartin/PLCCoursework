import Tokens
main = do text <- readFile "program.txt"
          putStrLn (show (alexScanTokens text))