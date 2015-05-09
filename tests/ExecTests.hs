module ExecTests (execTests) where

import Test.HUnit
import Phascal.Arm
import Phascal.Parser

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
    let asmpath = path </> "test.s"
    let objpath = path </> "test.o"
    let exepath = path </> "test"
    writeFile asmpath src
    callProcess "arm-none-eabi-as" ["-c", "-o", objpath, asmpath]
    callProcess "arm-none-eabi-ld" ["-o", exepath, objpath]
    return exepath

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
        (compileText "program empty; begin end.")
    ]
