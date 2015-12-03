module ExecTests (execTests) where

import Test.HUnit
import Phascal.Arm
import Phascal.Parser
import Phascal.Build

import System.IO
import System.Posix.Process
import System.FilePath.Posix
import System.Exit
import System.Directory

import TestUtil

execTest testName status asmSrc =
    TestLabel testName $ TestCase $ do
        status' <- runAsm testName asmSrc
        assertEqual ("unexpected exit status " ++ show status' ++ ", " ++
                     "expecting " ++ show status) status status'


execTests = TestList [
    execTest "empty-program" ExitSuccess
        (compileText "program empty; begin end."),

    -- Exit status should be the value of the last expression.
    -- Eventaully we'll have a less hacky way to get output;
    -- this isn't the semantics we want long-term.
    execTest "return 1" (ExitFailure 1)
        (compileText "program return1; var a : integer; begin a := 1 end."),

    -- write enough to the stack that if we're going the wrong way we'll
    -- probably break something (make sure we're not overwriting the saved lr or
    -- something).
    execTest "many-zero-vars" ExitSuccess
        (compileText $ unlines [
            "program manyZeroVars;",
            "var a, b, c, d, e, f : integer;",
            "begin",
            "  a := 0;",
            "  b := 0;",
            "  c := 0;",
            "  d := 0;",
            "  e := 0;",
            "  f := 0",
            "end."
         ]),

    -- Make sure values propogate through assignments correctly.
    execTest "propogate-values-assignemnt" (ExitFailure 1)
        (compileText $ unlines [
            "program propVars;",
            "var a, b, c : integer;",
            "begin",
            "  a := 1;",
            "  b := 0;",
            "  c := a",
            "end."
         ]),

    -- Make sure things are aligned properly; test with multi-byte values:
    execTest "multi-byte-values" ExitSuccess
        (compileText $ unlines [
            "program multiByteValues;",
            "var a, b :integer;",
            "begin",
            "  b := 0;",
            "  a := 123456789;",
            "  b := b",
            "end."
         ])
    ]
