module CodegenTypeTests where

import Phascal.TypeChecker
import Phascal.Arm
import Phascal.Ast
import Phascal.SymbolTable (SymTable, empty)

import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen

import Control.DeepSeq

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

-- | @ifTypeThenCode syms expr@ returns @True@ if:
--
-- * @expr@ does not pass the type checker, or
-- * @compileExpr syms expr@ successfully generates code.
--
-- Note that the type of @compileExpr@ looks like it guarantees this, but there
-- are cases which invoke @error@, which should only occur if the @expr@ is
-- ill-typed.
ifTypeThenCode :: SymTable -> Expr -> Bool
ifTypeThenCode syms expr = case (typeOf syms expr) of
    Left _ -> True
    Right _ -> compileExpr syms expr `deepseq` True
