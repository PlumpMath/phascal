import Parser (parse)
import System.Environment
import Control.Monad (join)

import Arm

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["ast"] -> interact (show . parse "<stdin>")
        ["asm"] -> do
            contents <- getContents
            let result = case parse "<stdin>" contents of
                            Left e -> Left e
                            Right progs -> Right $ mapM compileProgram progs
            case result of
                Right (Right asm) -> mapM_ (putStr . formatDirective) (join asm)
                -- TODO: we should send this to stdout and exit non-zero:
                Right (Left e) -> putStrLn ("error: " ++ show e)
                Left e -> putStrLn ("error: " ++ show e)
