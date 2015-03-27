module Parser (parse) where

import Ast

import Text.ParserCombinators.Parsec hiding (token, parse)
import Control.Applicative((<*),(*>))

token :: Parser a -> Parser a
token p = p <* spaces

ident :: Parser String
ident = token $ do
    c <- letter
    cs <- many (letter <|> digit)
    return (c:cs)

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

factor = choice [ ident >>= (return . Var)
                , token (char '(') *> simpleExpr <* token (char ')')
                , num >>= (return . Num)
                ]

term = do
    lhs <- factor
    try $ do
        op <- mulop
        rhs <- factor 
        return (Op op lhs rhs)
      <|> return lhs 

simpleExpr = do
    lhs <- term
    try $ do
        op <- addop
        rhs <- term
        return (Op op lhs rhs)
      <|> return lhs

statement :: Parser Statement
statement = do
    lhs <- ident
    token (string ":=")
    rhs <- expr
    return (Assign lhs rhs)

file :: Parser [Statement]
file = spaces *> statement `sepBy` (token (char ';'))

parse :: String -> String -> Either ParseError [Statement]
parse filename input = runParser file () filename input
