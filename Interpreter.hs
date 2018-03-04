import Tokens
import Grammar

--type Relation = (String,[VarItem],[[String]])

eval :: Prog -> String
eval (ProgLink p1 p2) = (eval p1) ++ (eval p2)
eval (Query svl cl) = query svl cl

query :: [String] -> [Constraint] -> String
query vs cs = printRel vs (applyEqs (foldl (relJoin) r rs) es)
  where
    (r:rs) = getRelations cs
    es = filter (isConstraintEq) cs

getRelations :: [Constraint] -> [Relation]
getRelations cs = map (evalConstraintRel) (filter (isConstraintRel) cs ++ filter (isConstraintRelEnhanced) cs)

evalConstraintRel :: Constraint -> Relation
evalConstraintRel (ConstraintRel rn vs) = TODO
evalConstraintRel (ConstraintRelEnhanced rn cs vs) = TODO

isConstraintRel :: Constraint -> Bool
isConstraintRel (ConstraintRel _ _) = True
isConstraintRel _ = False

isConstraintRelEnhanced :: Constraint -> Bool
isConstraintRelEnhanced (ConstraintRelEnhanced _ _ _) = True
isConstraintRelEnhanced _ = False

isConstraintEq :: Constraint -> Bool
isConstraintEq (ConstraintEq _ _) = True
isConstraintEq _ = False

applyEqs :: Relation -> [Constraint] -> Relation
applyEqs r (c:cs) = applyEqs (filter (satisfiesEq c) r) cs
applyEqs r [] = r

satisfiesEq :: Assignment -> Constraint -> Bool
satisfiesEq (v1,v2) ps = getVal ps v1 == getVal ps v2

getVal :: Assignment -> String -> String
getVal ps v = fst (head (filter (matchesV) ps))
  where
    matchesV (v',val) = v == v'

printRel :: [String] -> Relation -> String
printRel vs r = concat (sort (map (printRelLine vs) r))

printRelLine :: [String] -> Assignment -> String
printRelLine [v] a = getVal v ++ "\n"
printRelLine (v:vs) a = getVal v ++ "," ++ printRelLine vs a

--parseCSV :: [[String]] -> String -> [VarItem] -> Relation
--parseCSV xss rel cols =

--readCSV :: String -> IO [[String]]
--readCSV relName = ??--TODO

type Assignment = [(String, String)]
type Relation = [Assignment]

relJoin :: Relation -> Relation -> Relation
relJoin (x:xs) ys = map (merge x) (filter (match x) ys) ++ relJoin xs ys
relJoin [] ys = []

merge :: Assignment -> Assignment -> Assignment
merge xs ys = xs ++ (filter (\y -> not (y `elem` xs)) ys)

match :: Assignment -> Assignment -> Bool
match ((var,val):xs) ys = filter (\(vary,valy) -> var == vary && val /= valy) ys == [] && match xs ys
match [] ys = True
