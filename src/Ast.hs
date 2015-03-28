module Ast where

data Program = Program { name  :: String
                       , args  :: [String]
                       , decls :: [([String], Type)]
                       , body  :: [Statement]
                       } deriving(Show, Eq)

data Type = TyInt | TyBool deriving(Show, Eq)

data Statement = Assign String Expr
               | CompoundStatement [Statement]
               | If Expr Statement (Maybe Statement)
               | While Expr Statement
               deriving(Show, Eq)

data Expr = Num Int
          | Var String
          | Op BinOp Expr Expr
          | Not Expr
          | T
          | F
          | Neg Expr
          | Pos Expr
          deriving(Show, Eq)

data BinOp = Plus
           | Minus
           | Times
           | Div
           | Mod
           | And
           | Or
           | Eq
           | NEq
           | Lt
           | LtEq
           | Gt
           | GtEq
           deriving(Show, Eq)
