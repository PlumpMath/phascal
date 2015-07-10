module TypeTests where

import Test.HUnit
import Phascal.Ast
import Phascal.SymbolTable
import Phascal.TypeChecker

typeTests = TestList
                (map mktest [ (empty, Not (Num 0))
                            , (empty, (Pos T))
                            ])
  where
    mktest (syms, expr) = TestLabel "should-type-fail" (TestCase (assertTypeFail syms expr))

assertTypeFail syms expr = case typeOf syms expr of
    Left _ -> return ()
    Right t -> failTest (show expr ++ "should not have typechecked, but got type " ++ show t)

failTest msg = assertEqual msg 1 2
