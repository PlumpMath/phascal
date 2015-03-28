module Ast where

data Program = Program { name  :: String
                       , args  :: [String]
                       , decls :: [([String], Type)]
                       , body  :: [Statement]
                       } deriving(Show)

data Type = TyInt | TyBool deriving(Show)

data Statement = Assign String Expr deriving(Show)

data Expr = Num Int
          | Var String
          | Op BinOp Expr Expr
          deriving(Show)

data BinOp = Plus | Minus | Times | Div | Mod deriving(Show)
