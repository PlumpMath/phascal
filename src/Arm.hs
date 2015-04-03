module Arm where

import qualified Data.Map.Strict as M
import Control.Monad (liftM, liftM2, join)
import Ast
import Data.Bits
import Data.Word

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
           deriving(Show)
data Address = RegOffset Reg Int deriving(Show)

data Directive = Instruction Instr
               | Label String
               deriving(Show)


-- | (canImmediate n) indicates whether the number n is representable as
-- an arm assembly immediate.
canImmediate :: Int -> Bool
canImmediate n = any (\rot -> n' `rotate` rot < 256) [0,2..30]
  where n' = fromIntegral n :: Word32


formatInstr :: Instr -> String
formatInstr (Ldr reg addr) = "ldr " ++ reg ++ ", " ++ formatAddr addr
formatInstr (Str reg addr) = "str " ++ reg ++ ", " ++ formatAddr addr

formatAddr :: Address -> String
formatAddr (RegOffset reg off) = "[" ++ reg ++ ",#" ++ show off ++ "]"

formatDirective :: Directive -> String
formatDirective (Instruction instr) = "\t" ++ formatInstr instr ++ "\n"
formatDirective (Label lbl) = lbl ++ ":\n"

compileExpr :: SymTable -> Expr -> Either CompileError [Directive]
compileExpr syms (Var v) = varAddr syms v >>= \addr -> return [Instruction $ Ldr "r0" addr]

compileStatement :: SymTable -> Statement -> Either CompileError [Directive]
compileStatement syms (Assign v ex) = do
    addr <- varAddr syms v
    liftM2 (++) (compileExpr syms ex)
                (return [Instruction $ Str "r0" addr])

compileProgram :: Program -> Either CompileError [Directive]
compileProgram p = do
    body' <- mapM (compileStatement syms) (body p)
    return $ (Label $ name p) : join body'
  where syms = makeSymTable p
