module Phascal.Arm where

import Control.Monad (liftM, liftM2, join)
import Data.Bits
import Data.Word
import Prelude hiding (lookup)
import Data.List (intersperse)

import Phascal.Ast
import Phascal.SymbolTable

data CompileError = UndefinedVar String
                  deriving(Show, Eq)

varAddr :: SymTable -> String -> Either CompileError Address
varAddr syms v = case lookup v syms of
    Nothing -> Left (UndefinedVar v)
    Just (SymInfo offset _) -> Right (RegOffset "fp" offset)

type Reg = String

data Instr = Ldr Reg Address
           | Str Reg Address
           | Push [Reg]
           | Pop [Reg]
           | Add Reg Reg Reg
           | Svc Int
           | Mov Reg Int -- This will be more general eventually, but for now
                         -- we just need reg := immediate.
           deriving(Show, Eq)
data Address = RegOffset Reg Int deriving(Show, Eq)

data Directive = Instruction Instr
               | Label String
               | Globl String
               deriving(Show, Eq)


-- | @canImmediate n@ indicates whether the number @n@ is representable as
-- an arm assembly immediate.
--
-- Arm uses an 8 bit immediate with a 4 bit rotate field (where the rotation is
-- in units of 2 bits). There's a good description of the whole thing here:
--
-- <http://alisdair.mcdiarmid.org/2014/01/12/arm-immediate-value-encoding.html>
canImmediate :: Int -> Bool
canImmediate n = any (\rot -> n' `rotate` rot < 256) [0,2..30]
  where n' = fromIntegral n :: Word32


formatInstr :: Instr -> String
formatInstr (Ldr reg addr) = "ldr " ++ reg ++ ", " ++ formatAddr addr
formatInstr (Str reg addr) = "str " ++ reg ++ ", " ++ formatAddr addr
formatInstr (Push regs) = "push {" ++ join (intersperse "," regs) ++ "}"
formatInstr (Pop regs) = "pop {" ++ join (intersperse "," regs) ++ "}"
formatInstr (Add ret lhs rhs) = "add " ++ (join $ intersperse ", " [ret, lhs, rhs])
formatInstr (Svc n) = "svc #" ++ show n
formatInstr (Mov reg val) = "mov " ++ reg ++ ", #" ++ show val

formatAddr :: Address -> String
formatAddr (RegOffset reg off) = "[" ++ reg ++ ",#" ++ show off ++ "]"

formatDirective :: Directive -> String
formatDirective (Instruction instr) = "\t" ++ formatInstr instr ++ "\n"
formatDirective (Label lbl) = lbl ++ ":\n"
formatDirective (Globl sym) = ".globl " ++ sym ++ "\n"

compileExpr :: SymTable -> Expr -> Either CompileError [Directive]
compileExpr syms (Var v) = varAddr syms v >>= \addr -> return [Instruction $ Ldr "r0" addr]
compileExpr syms (Op op lhs rhs) = do
    [lAsm, rAsm] <- mapM compileSubExpr [lhs, rhs]
    opAsm <- compileBinOp op
    return $ lAsm ++ rAsm ++ [Instruction $ Pop ["r0", "r1"]] ++ opAsm
  where
    compileSubExpr ex = do
        sub <- compileExpr syms ex
        return $ sub ++ [Instruction $ Push ["r0"]]

compileBinOp :: BinOp -> Either CompileError [Directive]
compileBinOp Plus = Right $ [Instruction $ Add "r0" "r0" "r1"]

compileStatement :: SymTable -> Statement -> Either CompileError [Directive]
compileStatement syms (Assign v ex) = do
    addr <- varAddr syms v
    liftM2 (++) (compileExpr syms ex)
                (return [Instruction $ Str "r0" addr])

compileProgram :: Program -> Either CompileError [Directive]
compileProgram p = do
    body' <- mapM (compileStatement syms) (body p)
    return $ (Globl "_start"):(Label "_start"):(Label $ name p):(join body') ++
                (map Instruction [Mov "r7" 1, Svc 0])
  where syms = makeSymTable p
