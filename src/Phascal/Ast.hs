module Phascal.Ast where

-- | An ast for a pascal program
data Program = Program { name  :: String
                         -- ^ The name of the program
                       , args  :: [String]
                         -- ^ The names of the arguments to the program
                       , decls :: [([String], Type)]
                         -- ^ Declared variables
                       , body  :: [Statement]
                         -- ^ Body of the program
                       } deriving(Show, Eq)

data Type = TyInt | TyBool deriving(Show, Eq)

-- | A pascal statement
data Statement = Assign String Expr
               | CompoundStatement [Statement]
               | If Expr Statement (Maybe Statement)
               | While Expr Statement
               deriving(Show, Eq)

-- | A pascal expression
data Expr = Num Int            -- ^ numeric constant
          | Var String         -- ^ variable reference
          | Op BinOp Expr Expr -- ^ binary operator
          | Not Expr           -- ^ @Not e@ is the boolean negation of @e@
          | T                  -- ^ constant @true@
          | F                  -- ^ constant @false@
          | Neg Expr           -- ^ @Neg e@ additive inverse of @e@, i.e. @-e@
          | Pos Expr           -- ^ @+ e@, i.e. @e@ with a positive sign
          deriving(Show, Eq)

-- | A binary operator
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
