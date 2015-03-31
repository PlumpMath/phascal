module Arm where

import qualified Data.Map.Strict as M
import Control.Monad (liftM, liftM2, join)
import Ast

data SymInfo = SymInfo { frameOffset :: Int
                       , ty          :: Type
                       } 

data CompileError = UndefinedVar String
                  deriving(Show)

type SymTable = M.Map String SymInfo

-- TODO: We should check for duplicate entries.
makeSymTable :: Program -> SymTable
makeSymTable prog = foldl M.union M.empty (map makeOneType (decls prog))

makeOneType :: ([String], Type) -> SymTable
makeOneType (vs, ty) = M.fromList $ zip
    vs
    (zipWith SymInfo [0..] (repeat ty))

varAddr :: SymTable -> String -> Either CompileError Address
varAddr syms v = case M.lookup v syms of
    Nothing -> Left (UndefinedVar v)
    Just (SymInfo offset _) -> Right (RegOffset "fp" offset)

type Reg = String

data Instr = Ldr Reg Address
           | Str Reg Address
data Address = RegOffset Reg Int

instance Show Address where
    show (RegOffset reg off) = "[" ++ reg ++ ",#" ++ (show off) ++ "]"

instance Show Instr where
    show (Ldr reg addr) = "ldr " ++ reg ++ ", " ++ (show addr)
    show (Str reg addr) = "str " ++ reg ++ ", " ++ (show addr)

compileExpr :: SymTable -> Expr -> Either CompileError [Instr]
compileExpr syms (Var v) = varAddr syms v >>= \addr -> return [Ldr "r0" addr]

compileStatement :: SymTable -> Statement -> Either CompileError [Instr]
compileStatement syms (Assign v ex) = do
    addr <- varAddr syms v
    liftM2 (++) (compileExpr syms ex)
                (return [Str "r0" addr])

compileProgram :: Program -> Either CompileError [Instr]
compileProgram p = (sequence $ map (compileStatement syms) (body p)) >>= return . join
  where syms = makeSymTable p
