module Oblig1 where

import Data.Char

tokenise :: String -> [String]
tokenise [] = []
tokenise (x:xs)
  | elem x "+*/()-" = [x] : tokenise xs
  | x == ' ' = tokenise xs
  | isDigit x = (takeWhile isDigit (x:xs)) : tokenise (dropWhile isDigit (x:xs))
  | isAlpha x = (takeWhile isAlpha (x:xs)) : tokenise (dropWhile isAlpha (x:xs))
  | otherwise = error ("Wrong character: " ++ [x])

data Op = Add | Sub | Mult | Div
  deriving (Show, Eq)

data Ast = BinOp Op Ast Ast | Tall Int | Var String
  deriving (Show, Eq)

parseExpr :: [String] -> (Ast, [String])
parseExpr s = let (a, z) = parseFactor s in
            if null z then (a, z)
            else if head z == "+" then
                let (c, rest) = parseExpr (tail z) in (BinOp Add a c, rest)
            else if head z == "-" then
                let (c, rest) = parseExpr (tail z) in (BinOp Sub a c, rest)
            else (a, z)

parseFactor :: [String] -> (Ast, [String])
parseFactor s = let (a, z) = parseTerm s in 
            if null z then (a, z)
            else if head z == "*" then
                let (c, rest) = parseFactor (tail z) in (BinOp Mult a c, rest)
            -- else if head z == "/" then
            --     let (c, rest) = parseFactor (tail z) in
            --     if c == Tall 0 then error "Division by zero" -- Check for division by zero
            --     else (BinOp Div a c, rest)
            else if head z == "/" then
                let (c, rest) = parseFactor (tail z) in (BinOp Div a c, rest)
            else (a, z)                                 

parseTerm :: [String] -> (Ast, [String])
parseTerm ("(":s) = let (a, ")":b) = parseExpr s in (a, b)
parseTerm (x:s)
  | all isDigit x = (Tall (read x :: Int), s)
  | otherwise = (Var x, s)
parseTerm _ = error "Unexpected token" 

parse :: String -> Ast
parse s = let (tre,rest) = parseExpr (tokenise s) in
        if (null rest) then tre
        else error ("Error at: " ++ head(rest))

eval :: Ast -> Int
eval (Tall x) = x
eval (BinOp Add v h) = (eval v) + (eval h)
eval (BinOp Sub v h) = (eval v) - (eval h)
eval (BinOp Mult v h) = (eval v) * (eval h)
eval (BinOp Div v h) = (eval v) `div` (eval h)

ppInfix :: Ast -> String
ppInfix (Tall x) = show x
ppInfix (Var x) = x
ppInfix (BinOp Add v h) = "(" ++ (ppInfix v) ++ " + " ++ (ppInfix h) ++ ")"
ppInfix (BinOp Sub v h) = "(" ++ (ppInfix v) ++ " - " ++ (ppInfix h) ++ ")"
ppInfix (BinOp Mult v h) = "(" ++ (ppInfix v) ++ " * " ++ (ppInfix h) ++ ")"
ppInfix (BinOp Div v h) = "(" ++ (ppInfix v) ++ " / " ++ (ppInfix h) ++ ")"

ppPN :: Ast -> String
ppPN (Tall x) = show x
ppPN (Var x) = x
ppPN (BinOp Add v h) = "+ " ++ (ppPN v) ++ " " ++ (ppPN h)
ppPN (BinOp Sub v h) = "- " ++ (ppPN v) ++ " " ++ (ppPN h)
ppPN (BinOp Mult v h) = "* " ++ (ppPN v) ++ " " ++ (ppPN h)
ppPN (BinOp Div v h) = "/ " ++ (ppPN v) ++ " " ++ (ppPN h)

ppOPN :: Ast -> String
ppOPN (Tall x) = show x
ppOPN (Var x) = x
ppOPN (BinOp Add v h) = (ppOPN v) ++ " " ++ (ppOPN h) ++ " +"
ppOPN (BinOp Sub v h) = (ppOPN v) ++ " " ++ (ppOPN h) ++ " -"
ppOPN (BinOp Mult v h) = (ppOPN v) ++ " " ++ (ppOPN h) ++ " *"
ppOPN (BinOp Div v h) = (ppOPN v) ++ " " ++ (ppOPN h) ++ " /"

findVar :: [(String,Int)] -> String -> Int
findVar [] var = error ("Variablen: " ++ var ++ "finnes ikke")
findVar ((key, value):rest) var
  | key == var = value
  | otherwise = findVar rest var 

evalVar :: Ast -> [(String,Int)] -> Int
evalVar (Tall x) _ = x
evalVar (Var x) varList = findVar varList x
evalVar (BinOp Add v h) varList = (evalVar v varList) + (evalVar h varList)
evalVar (BinOp Sub v h) varList = (evalVar v varList) - (evalVar h varList)
evalVar (BinOp Mult v h) varList = (evalVar v varList) * (evalVar h varList)
evalVar (BinOp Div v h) varList = (evalVar v varList) `div` (evalVar h varList)

