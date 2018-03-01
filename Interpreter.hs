import Tokens
import Grammar

--type Relation = (String,[VarItem],[[String]])

--eval :: Prog -> String
--eval (ProgLink p1 p2) = (eval p1) ++ (eval p2)
--eval (Query svl cl) = query svl cl

--query :: StrictVarList -> ConstraintList -> String
--query (StrictVarListSingleton var) cl = 

--parseCSV :: [[String]] -> String -> [VarItem] -> Relation
--parseCSV xss rel cols =

--readCSV :: String -> IO [[String]]
--readCSV relName = ??--TODO

type Assignment = [(String, String)]
type Relation = [Assignment]

join :: Relation -> Relation -> Relation
join (x:xs) ys = map (merge x) (filter (match x) ys) ++ join xs ys
join [] ys = []

merge :: Assignment -> Assignment -> Assignment
merge xs ys = xs ++ (filter (\y -> not (y `elem` xs)) ys)

match :: Assignment -> Assignment -> Bool
match ((var,val):xs) ys = filter (\(vary,valy) -> var == vary && val /= valy) ys == [] && match xs ys
match [] ys = True







convertVarList :: VarList -> [VarItem]
convertVarList (VarListSingleton vi) = [vi]
convertVarList (VarListLink vi vl) = [vi] ++ convertVarList vl

convertStrictVarList :: StrictVarList -> [String]
convertStrictVarList (StrictVarListSingleton vi) = [vi]
convertStrictVarList (StrictVarListLink vi vl) = [vi] ++ convertStrictVarList vl
