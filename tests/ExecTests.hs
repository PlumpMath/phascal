module ExecTests (execTests) where

import Test.HUnit
import Phascal.Arm
import Phascal.Parser
import Phascal.Build

import System.IO
import System.Process
import System.Posix.Process
import System.FilePath.Posix
import System.Exit
import System.Directory

import Control.Monad (join)


getPathFor :: String -> IO String
getPathFor name = do
    pid <- getProcessID
    let path = "test-scratch" </> show pid </> name
    createDirectoryIfMissing True path
    return path

buildAsm :: String -> String -> IO String
buildAsm testName src = do
    path <- getPathFor testName
    let [asm, obj, exe] = map (path </>) ["test.s", "test.o", "test"]
    writeFile asm src
    assemble obj asm
    link exe obj
    return exe

runAsm :: String -> String -> IO ExitCode
runAsm testName src = do
    exepath <- buildAsm testName src
    spawnProcess "qemu-arm" [exepath] >>= waitForProcess

execTest testName status asmSrc =
    TestLabel testName $ TestCase $ do
        status' <- runAsm testName asmSrc
        assertEqual ("unexpected exit status " ++ show status' ++ ", " ++
                     "expecting " ++ show status) status status'


compileText :: String -> String
compileText src = do
    let
        (Right [ast]) = parse "<test-program>" src
        (Right directives) = compileProgram ast
      in
        join (map formatDirective directives)


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
         ])
    ]
