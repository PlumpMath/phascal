{-# LANGUAGE DeriveGeneric #-}
module Phascal.Arm where

import Control.Monad (liftM, liftM2, join)
import Data.Bits
import Data.Word
import Prelude hiding (lookup)
import Data.List (intersperse)
import Data.Maybe (fromJust)

import GHC.Generics
import Control.DeepSeq

import Phascal.Ast
import Phascal.SymbolTable

data CompileError = UndefinedVar String
                  deriving(Show, Eq)

varAddr :: SymTable -> String -> Address
varAddr syms v = RegOffset "fp" (slotnum * 4)
    where slotnum = frameOffset $ fromJust (lookup v syms)

type Reg = String

data Instr = Ldr Reg Address
           | Str Reg Address
           | Push [Reg]
           | Pop [Reg]
           | Ldm Reg [Reg]
           | Add Reg Reg Reg
           | OrR Reg Reg Reg
           | SubRRR Reg Reg Reg
           | SubRRI Reg Reg Int -- reg := reg - immediate
           | Svc Int
           | EorRI Reg Reg Int
           | MovRR Reg Reg -- reg := reg
           | MovRI Reg Int -- reg := immediate
           | Bl String
           deriving(Show, Eq, Generic)

data Address = RegOffset Reg Int
             | AddrContaining Int -- =0x1242 notation, i.e. put this value in the
                                  -- binary and refer to it by its address
             deriving(Show, Eq, Generic)

data Directive = Instruction Instr
               | Label String
               | Globl String
               deriving(Show, Eq, Generic)

instance NFData Instr
instance NFData Address
instance NFData Directive


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
formatInstr (OrR ret lhs rhs) = formatApply "orr" [ret, lhs, rhs]
formatInstr (SubRRR rd rm rs) = formatApply "sub" [rd, rm, rs]
formatInstr (SubRRI ret lhs rhs) = formatApply "sub" [ret, lhs, formatInt rhs]
formatInstr (Svc n) = formatApply "svc" [formatInt n]
formatInstr (EorRI rd rs int) = formatApply "eor" [rd, rs, formatInt int]
formatInstr (MovRI reg val) = formatApply "mov" [reg, formatInt val]
formatInstr (MovRR lhs rhs) = formatApply "mov" [lhs, rhs]
formatInstr (Bl label) = formatApply "bl" [label]

formatAddr :: Address -> String
formatAddr (RegOffset reg off) = join ["[", commaSep [reg, formatInt off], "]"]
formatAddr (AddrContaining addr) = "=" ++ show addr

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

compileExpr :: SymTable -> Expr -> [Directive]
compileExpr syms (Var v) = [Instruction $ Ldr "r0" (varAddr syms v)]
compileExpr syms (Num n) = [Instruction $ if canImmediate n
                                           then MovRI "r0" n
                                           else Ldr "r0" (AddrContaining n)]
compileExpr _ T = [Instruction (MovRI "r0" 1)]
compileExpr _ F = [Instruction (MovRI "r0" 0)]
compileExpr syms (Not ex) =
    (compileExpr syms ex) ++ [Instruction $ EorRI "r0" "r0" 1]
compileExpr syms (Pos ex) = compileExpr syms ex
compileExpr syms (Neg ex) =
    (compileExpr syms ex) ++ (map Instruction [ MovRI "r1" 0
                                              , SubRRR "r0" "r1" "r0"
                                              ])
compileExpr syms (Op op lhs rhs) =
    let
        [lAsm, rAsm] = map compileSubExpr [lhs, rhs]
        opAsm = compileBinOp op
    in
        lAsm ++ rAsm ++ [Instruction $ Pop ["r0", "r1"]] ++ opAsm
  where
    compileSubExpr ex = (compileExpr syms ex) ++ [Instruction $ Push ["r0"]]

compileBinOp :: BinOp -> [Directive]
compileBinOp Plus = [Instruction $ Add "r0" "r0" "r1"]
compileBinOp Or = [Instruction $ OrR "r0" "r0" "r1"]

compileStatement :: SymTable -> Statement -> [Directive]
compileStatement syms (Assign v ex) = do
    let addr = varAddr syms v in
        (compileExpr syms ex) ++ [Instruction $ Str "r0" addr]

compileProgram :: Program -> [Directive]
compileProgram p =
    let body' = map (compileStatement $ makeSymTable p) (body p) in
        join [ entryPoint (name p)
             , [Label (name p)]
             , functionPrologue
             , join body'
             , functionEpilogue
             ]


functionPrologue :: [Directive]
functionPrologue = instrs [ MovRR "ip" "sp"
                          , Push ["fp", "ip", "lr", "pc"]
                          , SubRRI "fp" "ip" 4
                          ]

functionEpilogue :: [Directive]
functionEpilogue = instrs [ Ldm "sp" ["fp", "sp", "lr"]
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
