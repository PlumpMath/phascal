import Ast
import Parser

main :: IO ()
main = interact (show . parse "<stdin>")
