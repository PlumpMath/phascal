module Main where

import System.Environment
import System.IO
import System.Exit
import Control.Monad (join)

import Phascal.Parser (parse)
import Phascal.Arm

main :: IO ()
main = do
    args <- getArgs
    contents <- getContents
    case args of
        ["ast"] -> do
            case parse "<stdin>" contents of
                Right ast -> print ast
                Left e -> hPutStrLn stderr ("error: " ++ show e) >> exitFailure
        ["asm"] -> do
            let result = case parse "<stdin>" contents of
                            Left e -> Left e
                            Right progs -> Right $ mapM compileProgram progs
            case result of
                Right (Right asm) -> mapM_ (putStr . formatDirective) (join asm)
                Right (Left e) -> hPutStrLn stderr ("error: " ++ show e)
                Left e -> hPutStrLn stderr ("error: " ++ show e) >> exitFailure
