module Parser (parse) where

import Ast

import Text.ParserCombinators.Parsec hiding (token, parse)
import Control.Applicative((<*),(*>))
import Control.Monad(void, liftM)

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

plus, minus, times, divide, modulo :: Parser BinOp

plus = token (char '+') >> return Plus
minus = token (char '-') >> return Minus
times = token (char '*') >> return Times
divide = token (string "/" <|> string "div") >> return Div
modulo = token (string "mod") >> return Mod

addop, mulop :: Parser BinOp
addop = choice [plus, minus]
mulop = choice [times, divide, modulo]


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

statement :: Parser Statement
statement = do
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
    many1 $ do
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
