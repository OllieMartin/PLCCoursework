import Tokens
import Grammar
interpret :: Prog -> String
interpret (ProgLink p1 p2) = (interpret p1) ++ (interpret p2)