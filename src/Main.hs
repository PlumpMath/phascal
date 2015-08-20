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
                            -- TODO: We *need* to typecheck before we invoke
                            -- compileProgram. This could invoke error
                            -- otherwise.
                            Right progs -> Right $ mapM compileProgram progs
            case result of
                Right asm -> mapM_ (putStr . formatDirective) (join asm)
                Left e -> hPutStrLn stderr ("error: " ++ show e) >> exitFailure
        _ -> hPutStrLn stderr "Usage: phascal [ asm | ast ]" >> exitFailure
