module CodegenTypeTests where

import Phascal.TypeChecker
import Phascal.Arm
import Phascal.Ast
import Phascal.SymbolTable (SymTable, empty)

import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen

instance Arbitrary Expr where
    arbitrary = oneof [ Num <$> arbitrary
--                      , Var <$> arbitrary -- Random variable names are
--                                          -- something I'm not quite ready to
--                                          -- deal with
                      , Op  <$> arbitrary <*> arbitrary <*> arbitrary
                      , Not <$> arbitrary
                      , return T
                      , return F
                      , Neg <$> arbitrary
                      , Pos <$> arbitrary
                      ]

instance Arbitrary SymTable where
    arbitrary = return empty

instance Arbitrary BinOp where
    arbitrary = oneof (map return [ Plus
                                  , Minus
                                  , Times
                                  , Div
                                  , Mod
                                  , And
                                  , Or
                                  , Eq
                                  , NEq
                                  , Lt
                                  , LtEq
                                  , Gt
                                  , GtEq
                                  ])

ifTypeThenCode :: SymTable -> Expr -> Bool
ifTypeThenCode syms expr = case (typeOf syms expr) of
    Left _ -> True
    Right _ -> case (compileExpr syms expr) of
        Left _ -> False
        Right _ -> True
