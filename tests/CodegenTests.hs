module CodegenTests where

import Phascal.TypeChecker
import Phascal.Arm
import Phascal.Ast
import Phascal.SymbolTable (SymTable, empty)

import System.Exit
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen
import Test.QuickCheck (Property)

import Test.QuickCheck.Monadic

import Control.DeepSeq

import Control.Monad (join)
import TestUtil

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



intBinOpProps = [ (Plus,  ((+), "+"))
                , (Minus, ((-), "-"))
                , (Times, ((*), "*"))
                , (Div,   (div, "div"))
                , (Mod,   (mod, "mod"))
                ]

-- | @binopsWork@ is a quickcheck property which verifies that combining two
-- numbers with a binary operator in compiled has the same results as the
-- corresponding haskell function.
--
-- FIXME: We aren't accounting for the fact that exit stauts are small, and thus
-- our results are likely to be truncated.
binopsWork :: Property
binopsWork = monadicIO $ do
    op <- pick arbitrary
    let _ = op :: BinOp
    lhs <- pick arbitrary
    rhs <- pick arbitrary
    result <- case (lookup op intBinOpProps) of
        Nothing -> return True
        Just (f, str) -> run $ do
            let testprog = compileText $
                    unlines [
                        "program testBinopWorks;",
                        "var a : integer;",
                        "begin",
                        "  a := "
                            ++ show (lhs :: Int)
                            ++ " " ++ str ++ " "
                            ++ show (rhs :: Int),
                        "end."
                    ]
            status <- runAsm
                        (join ["testBinopWorks_", show op,  "_", show lhs,  "_", show rhs])
                        testprog
            let statusNum = case status of
                                ExitSuccess -> 0
                                ExitFailure statusNum' -> statusNum'
            return (statusNum == (f lhs rhs))
    assert result
