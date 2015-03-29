import TestSuite (runTests)
import Parser (parse)
import System.Environment

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["test"] -> runTests >>= print
        [] -> interact (show . parse "<stdin>")
