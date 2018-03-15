module Interpreter3 where

import Tokens
import MonadicGrammar

import System.Environment
import Control.Monad
import Data.List
import System.Directory

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn x xs | leftOver == [] = [taken]
             | otherwise = taken : splitOn x (tail leftOver)
  where
    taken = takeWhile (/= x) xs
    leftOver = dropWhile (/= x) xs

eval :: Prog -> IO String
eval (ProgLink p1 p2) = do a <- (eval p1)
                           b <- (eval p2)
                           return (a ++ b)
eval (Query svl cl) = query svl cl

query :: [String] -> [Constraint] -> IO String
query vs cs = do (r:rs) <- getRelations cs
                 r2 <- (let r1 = (foldl (relJoin) r rs) in applyEqs r1 es)
                 return (printRel vs r2)
              where es = filter (isConstraintEq) cs

getRelations :: [Constraint] -> IO [Relation]
getRelations cs = mapM (evalConstraintRel) ( (filter (isConstraintRel) cs) ++ (filter (isConstraintRelEnhanced) cs))

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
readCSV relName cs = do b <- doesFileExist filename
                        readCSV' b
  where
    filename = relName ++ ".csv"
    readCSV' b | b = do result <- readFile filename
                        return [ [ record !! n | n <- cs ] | record <- (map (splitOn ',') (lines result))]
               | otherwise = errorWithoutStackTrace ("Relation " ++ relName ++ " does not have matching file: " ++ filename)

isConstraintRel :: Constraint -> Bool
isConstraintRel (ConstraintRel _ _) = True
isConstraintRel _ = False

isConstraintRelEnhanced :: Constraint -> Bool
isConstraintRelEnhanced (ConstraintRelEnhanced _ _ _) = True
isConstraintRelEnhanced _ = False

isConstraintEq :: Constraint -> Bool
isConstraintEq (ConstraintEq _ _) = True
isConstraintEq _ = False



applyEqs :: Relation -> [Constraint] -> IO Relation
applyEqs r (c:cs) = do result <- applyEqs (filter (satisfiesEq c) r) cs
                       return result;
applyEqs r [] = return r

satisfiesEq :: Constraint -> Assignment -> Bool
satisfiesEq (ConstraintEq v1 v2) ps = getVal ps v1 == getVal ps v2
satisfiesEq _ ps = True

-- Gets value of a specific variable from an assignment
getVal :: Assignment -> String -> String
getVal ps v | fs /= [] = snd (head fs)
            | otherwise = errorWithoutStackTrace ("Variable unconstrained: " ++ v)
  where
    matchesV (v',val) = v == v'
    fs = filter (matchesV) ps

-- Creates a string of an entire relation according to printRelLine
printRel :: [String] -> Relation -> String
printRel vs r = (concat (sort (map (printRelLine vs) r)))

-- Creates a string of one line of a relation in the order of [String] i.e. ["x3","x2"] (filtering out any cols not required)
printRelLine :: [String] -> Assignment -> String
printRelLine [v] a = (getVal a v) ++ "\n"
printRelLine (v:vs) a = (getVal a v) ++ "," ++ printRelLine vs a

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
