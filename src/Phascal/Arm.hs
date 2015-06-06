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
           | Bl String
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
formatInstr (Ldr reg addr) = formatApply "ldr" [reg, formatAddr addr]
formatInstr (Str reg addr) = formatApply "str" [reg, formatAddr addr]
formatInstr (Push regs) = formatApply "push" [formatRegList regs]
formatInstr (Pop regs) = formatApply "pop" [formatRegList regs]
formatInstr (Ldm base regs) = formatApply "ldm" [base, formatRegList regs]
formatInstr (Add ret lhs rhs) = formatApply "add" [ret, lhs, rhs]
formatInstr (Svc n) = formatApply "svc" [formatInt n]
formatInstr (MovRI reg val) = formatApply "mov" [reg, formatInt val]
formatInstr (MovRR lhs rhs) = formatApply "mov" [lhs, rhs]
formatInstr (Bl label) = formatApply "bl" [label]

formatAddr :: Address -> String
formatAddr (RegOffset reg off) = join ["[", commaSep [reg, formatInt off], "]"]

formatInt :: Int -> String
formatInt value = "#" ++ show value

formatRegList :: [Reg] -> String
formatRegList regs = join ["{", commaSep regs, "}"]

-- | @formatApply op args@ is the string represention of pneumonic @op@
-- applied to the (already formatted) list of arguments @args@.
formatApply :: String -> [String] -> String
formatApply op args = op ++ " " ++ commaSep args

commaSep :: [String] -> String
commaSep items = join (intersperse ", " items)

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
    return $ join [ entryPoint (name p)
                  , [Label (name p)]
                  , functionPrologue
                  , join body'
                  , functionEpilouge
                  ]


functionPrologue :: [Directive]
functionPrologue = instrs [ MovRR "ip" "sp"
                          , Push ["fp", "ip", "lr", "pc"]
                          ]

functionEpilouge :: [Directive]
functionEpilouge = instrs [ Ldm "sp" ["fp", "sp", "lr"]
                          , MovRR "pc" "lr"
                          ]


entryPoint :: String -> [Directive]
entryPoint mainSym = [ Globl "_start"
                     , Label "_start"
                     ] ++ instrs [ MovRI "fp" 0
                                 , Bl mainSym
                                 , MovRI "r7" 1
                                 , Svc 0
                                 ]

instrs :: [Instr] -> [Directive]
instrs = map Instruction
