module Parser (parse) where

import Ast

import Text.ParserCombinators.Parsec hiding (token, parse)
import Control.Applicative((<*),(*>))
import Control.Monad(void, liftM)

keywords = [ "begin"
           , "end"
           ]

whitespace :: Parser ()
whitespace = void $ many $ oneOf "\t\r\n\f "

token :: Parser a -> Parser a
token p = p <* whitespace

semi :: Parser()
semi = void $ token (char ';')

parens :: Parser a -> Parser a
parens p = token (char '(') *> p <* token (char ')')

ident :: Parser String
ident = token $ do
    c <- letter
    cs <- many (letter <|> digit)
    return (c:cs)

idlist :: Parser [String]
idlist = ident `sepBy` token (char ',')

num :: Parser Int
num = token $ do
    cs <- many1 digit
    return (read cs)

binOp str op = token (string str) >> return op

addop, mulop :: Parser BinOp
addop = choice $ zipWith binOp
    ["+",  "-",  "or"]
    [Plus, Minus, Or]
mulop = choice $ zipWith binOp
    ["*",   "/", "div", "mod", "and"]
    [Times, Div, Div,   Mod,   And]


expr, simpleExpr, term, factor :: Parser Expr

expr = simpleExpr

factor = choice [ liftM Var ident
                , parens simpleExpr
                , liftM Num num
                ]

term = do
    lhs <- factor
    try $ do
        op <- mulop
        rhs <- term
        return (Op op lhs rhs)
      <|> return lhs 

simpleExpr = do
    lhs <- term
    try $ do
        op <- addop
        rhs <- simpleExpr
        return (Op op lhs rhs)
      <|> return lhs

statement, simpleStatement, ifStatement, whileStatement :: Parser Statement
statement = choice $ map try [ simpleStatement
                             , liftM CompoundStatement compoundStatement
                             , ifStatement
                             , whileStatement
                             ]

simpleStatement = do
    lhs <- ident
    token (string ":=")
    rhs <- expr
    return (Assign lhs rhs)

compoundStatement :: Parser [Statement]
compoundStatement = do
    token (string "begin")
    stmts <- try statement `sepBy` semi
    token (string "end")
    return stmts

ifStatement = do
    token (string "if")
    e <- expr
    token (string "then")
    ifTrue <- statement
    ifFalse <- try (liftM Just $ token (string "else") >> statement)
                <|> return Nothing
    return (If e ifTrue ifFalse)

whileStatement = do
    token (string "while")
    e <- expr
    token (string "do")
    stmt <- statement
    return (While e stmt)

program :: Parser Program
program = do
    token (string "program")
    id <- ident
    args <- option [] (parens idlist)
    semi
    decls <- option [] (try declarations)
    stmts <- compoundStatement
    token (char '.')
    return (Program id args decls stmts)

declarations :: Parser [([String], Type)] 
declarations = do
    token (string "var")
    many1 $ try $ do
        vars <- idlist
        token (char ':')
        t <- ty
        semi
        return (vars, t)

ty, tyInt, tyBool :: Parser Type
ty = tyInt <|> tyBool

tyInt  = token (string "integer") >> return TyInt
tyBool = token (string "boolean") >> return TyBool

file :: Parser [Program]
file = spaces *> many program

parse :: String -> String -> Either ParseError [Program]
parse = runParser file ()
