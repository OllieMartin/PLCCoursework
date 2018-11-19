import Tokens
import Grammar

import System.Environment

--type Relation = (String,[VarItem],[[String]])

eval :: Prog -> IO String
eval (ProgLink p1 p2) = (eval p1) ++ (eval p2)
eval (Query svl cl) = query svl cl

query :: [String] -> [Constraint] -> IO String
query vs cs = printRel vs (applyEqs (foldl (relJoin) r rs) es)
  where
    (r:rs) = getRelations cs
    es = filter (isConstraintEq) cs

getRelations :: [Constraint] -> IO [Relation]
getRelations cs = map (evalConstraintRel) (filter (isConstraintRel) cs ++ filter (isConstraintRelEnhanced) cs)

evalConstraintRel :: Constraint -> IO Relation
evalConstraintRel (ConstraintRel rn vs) = parseCSV rn vs [0..(length vs)-1]
evalConstraintRel (ConstraintRelEnhanced rn cs vs) = parseCSV rn vs cs

parseCSV :: String -> [VarItem] -> [Int] -> IO Relation
parseCSV relName vs cs = do r <- result
                            return ( map (parseCSVLine vs) r )
                where result = readCSV relName cs

-- Line -> VarName -> Assignment
parseCSVLine :: [VarItem] -> [String] -> Assignment
parseCSVLine vs xs = map varItemToString (filter isNotBlank ys)
              where ys = zip vs xs

isNotBlank :: (VarItem,String) -> Bool
isNotBlank ((VarItemBlank),_) = False
isNotBlank (_,_) = True

varItemToString :: (VarItem,String) -> (String,String)
varItemToString ((VarItemVar v),x) = (v,x)

readCSV :: String -> [Int] -> IO [[String]]
readCSV relName cs = do result <- readFile ( relName ++ ".csv" )
                        return [ (map (splitOn ",") (lines result)) !! n | n <- cs ]

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

-- TODO
satisfiesEq :: Assignment -> Constraint -> Bool
satisfiesEq (v1,v2) ps = getVal ps v1 == getVal ps v2

-- Gets value of a specific variable from an assignment
getVal :: Assignment -> String -> String
getVal ps v = fst (head (filter (matchesV) ps))
  where
    matchesV (v',val) = v == v'

-- Creates a string of an entire relation according to printRelLine
printRel :: [String] -> Relation -> String
printRel vs r = concat (sort (map (printRelLine vs) r))

-- Creates a string of one line of a relation in the order of [String] i.e. ["x3","x2"] (filtering out any cols not required)
printRelLine :: [String] -> Assignment -> String
printRelLine [v] a = getVal v ++ "\n"
printRelLine (v:vs) a = getVal v ++ "," ++ printRelLine vs a

type Assignment = [(String, String)]
type Relation = [Assignment]

-- Merges two relations where compatible (using the merge function below)
relJoin :: Relation -> Relation -> Relation
relJoin (x:xs) ys = map (merge x) (filter (match x) ys) ++ relJoin xs ys
relJoin [] ys = []

-- Merges two records where compatible
merge :: Assignment -> Assignment -> Assignment
merge xs ys = xs ++ (filter (\y -> not (y `elem` xs)) ys)

-- Determines if two assignments compatible
match :: Assignment -> Assignment -> Bool
match ((var,val):xs) ys = filter (\(vary,valy) -> var == vary && val /= valy) ys == [] && match xs ys
match [] ys = True
