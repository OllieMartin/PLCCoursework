import Tokens
import Grammar

import System.Environment
import Data.List.Split

--type Relation = (String,[VarItem],[[String]])

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
