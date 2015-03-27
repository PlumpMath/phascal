module Ast where

data Statement = Assign String Expr deriving(Show)

data Expr = Num Int
          | Var String
          | Op BinOp Expr Expr
          deriving(Show)

data BinOp = Plus | Minus | Times | Div | Mod deriving(Show)
