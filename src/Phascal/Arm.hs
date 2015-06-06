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
           | Ldm Reg [Reg]
           | Add Reg Reg Reg
           | Svc Int
           | MovRR Reg Reg -- reg := reg
           | MovRI Reg Int -- reg := immediate
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
formatInstr (Ldm base regs) = "ldm " ++ base ++ ", {" ++ join (intersperse "," regs) ++ "}"
formatInstr (Add ret lhs rhs) = "add " ++ join (intersperse ", " [ret, lhs, rhs])
formatInstr (Svc n) = "svc #" ++ show n
formatInstr (MovRI reg val) = "mov " ++ reg ++ ", #" ++ show val
formatInstr (MovRR lhs rhs) = "mov " ++ lhs ++ ", " ++ rhs

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
compileBinOp Plus = Right [Instruction $ Add "r0" "r0" "r1"]

compileStatement :: SymTable -> Statement -> Either CompileError [Directive]
compileStatement syms (Assign v ex) = do
    addr <- varAddr syms v
    liftM2 (++) (compileExpr syms ex)
                (return [Instruction $ Str "r0" addr])

compileProgram :: Program -> Either CompileError [Directive]
compileProgram p = do
    body' <- mapM (compileStatement $ makeSymTable p) (body p)
    return $ join [ [ Globl "_start"
                    , Label "_start"
                    , Label (name p)
                    ]
                  , map Instruction functionPrologue
                  , join body'
                  , map Instruction [ MovRI "r7" 1
                                    , Svc 0
                                    ]
                  -- Putting this here is a little silly; the above is an exit,
                  -- so the function epilouge is dead code. Eventually we'll
                  -- have different notions of function vs. program though.
                  , map Instruction functionEpilouge
                  ]


functionPrologue :: [Instr]
functionPrologue = [ MovRR "ip" "sp"
                   , Push ["fp", "ip", "lr", "pc"]
                   ]

functionEpilouge :: [Instr]
functionEpilouge = [ Ldm "sp" ["fp", "sp", "lr"]
                   , MovRR "pc" "lr"
                   ]
