import Tokens
import Grammar

type Relation = (String,[VarItem],[[String]])

eval :: Prog -> String
eval (ProgLink p1 p2) = (eval p1) ++ (eval p2)
eval (Query svl cl) = query svl cl

query :: StrictVarList -> ConstraintList -> String
query (StrictVarListSingleton var) cl = 

parseCSV :: [[String]] -> String -> [VarItem] -> Relation
parseCSV xss rel cols =

readCSV :: String -> IO [[String]]
readCSV relName = --TODO







convertVarList :: VarList -> [VarItem]
convertVarList (VarListSingleton vi) = [vi]
convertVarList (VarListLink vi vl) = [vi] ++ convertVarList vl

convertStrictVarList :: StrictVarList -> [String]
convertStrictVarList (StrictVarListSingleton vi) = [vi]
convertStrictVarList (StrictVarListLink vi vl) = [vi] ++ convertStrictVarList vl