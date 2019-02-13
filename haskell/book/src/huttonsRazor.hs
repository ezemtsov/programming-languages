-- huttonsRazor.hs
module HuttonsRazor where

-- 1. Your first task is to write the "eval" function which reduces
--    an expression to a final sum
data Expr
  = Lit Integer
  | Add Expr Expr

eval :: Expr -> Integer
eval (Lit i) = i
eval (Add a b) = eval a + eval b

-- 2. Write a printer for the expressions
printExpr :: Expr -> String
printExpr (Lit i) = show i
printExpr (Add a b) = printExpr a ++ " + " ++ printExpr b
