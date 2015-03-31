import TestSuite (runTests)
import Parser (parse)
import System.Environment

import Arm

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["test"] -> runTests >>= print
        ["ast"] -> interact (show . parse "<stdin>")
        ["asm"] -> do
            contents <- getContents
            print $ case parse "<stdin>" contents of
                Left e -> Left e
                Right progs -> Right $ sequence (map compileProgram progs) 
