module ArmTests (armTests) where

import Test.HUnit
import Data.Bits
import Phascal.Arm
import Phascal.Ast
import Phascal.Types
import qualified Phascal.SymbolTable as S

        -- bytes in any position:
yes = [ 0x000000ff
      , 0x0000ff00
      , 0x00ff0000
      , 0xff000000
        -- misc:
      , 0x0003fc00
      , 0xc0000034
        -- any power of two:
      ] ++ map (shiftL 1) [0..31]

no = [ 0x00000102
     , 0xff0000ff
     , 0x00100001
     ]

instrTests = [ (Push ["r0", "r5", "lr"], "push {r0, r5, lr}")
             , (Pop  ["r1", "r3"],       "pop {r1, r3}")
             , (Pop  ["r1"],             "pop {r1}")
             , (Add "r1" "r2" "r3",     "add r1, r2, r3")
             , (Add "r0" "r0" "r1",     "add r0, r0, r1")
             ]


exprTests = [ (Var "x"
              , S.fromList [("x", S.SymInfo 1 TyInt)]
              , Right [Instruction (Ldr "r0" (RegOffset "fp" 4))]
              )
            , ( Op Plus (Var "x") (Var "y")
              , S.fromList [ ("x", S.SymInfo 1 TyInt)
                           , ("y", S.SymInfo 2 TyInt)
                           ]
              , Right [ Instruction (Ldr "r0" (RegOffset "fp" 4))
                      , Instruction (Push ["r0"])
                      , Instruction (Ldr "r0" (RegOffset "fp" 8))
                      , Instruction (Push ["r0"])
                      , Instruction (Pop ["r0", "r1"])
                      , Instruction (Add "r0" "r0" "r1")
                      ]
              )
            , ( Num 62 -- fits in an immediate
              , S.empty
              , Right [Instruction (MovRI "r0" 62)]
              )
            , ( Num 12345678 -- doesn't fit in an immediate
              , S.empty
              , Right [Instruction (Ldr "r0" (AddrContaining 12345678))]
              )
            ]

armTests = TestList $
    [TestLabel (show num)   (immediateTestCase num True)  | num <- yes] ++
    [TestLabel (show num)   (immediateTestCase num False) | num <- no]  ++
    [TestLabel (show instr) (formatInstrTestCase instr output)
             | (instr, output) <- instrTests] ++
    [TestLabel (show (expr, syms)) (exprTestCase expr syms output)
             | (expr, syms, output) <- exprTests]
  where
    immediateTestCase num expected =
        TestCase $ assertEqual
                    ("expected " ++ show expected ++ " for " ++ show num)
                    (canImmediate num) expected
    formatInstrTestCase instr output =
        TestCase $ assertEqual
                    ("expected " ++ show output ++ " for " ++ show instr)
                    output
                    (formatInstr instr)
    exprTestCase expr syms output =
        TestCase $ assertEqual
                    ("expected " ++ show output ++ " for " ++ show expr)
                    output
                    (compileExpr syms expr)
