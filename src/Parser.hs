module Parser (parse) where

import Ast

import Text.ParserCombinators.Parsec hiding (token, parse)
import Control.Applicative((<*),(*>),empty)
import Control.Monad(void, liftM)

keywords = [ "and"
           , "begin"
           , "div"
           , "do"
           , "else"
           , "end"
           , "false"
           , "if"
           , "mod"
           , "not"
           , "or"
           , "program"
           , "then"
           , "true"
           , "var"
           , "while"
           ]

keyword :: String -> Parser String
keyword = try . token . string

whitespace :: Parser ()
whitespace = void $ many $ oneOf "\t\r\n\f "

token :: Parser a -> Parser a
token p = p <* whitespace

semi :: Parser ()
semi = void $ keyword ";"

parens :: Parser a -> Parser a
parens p = keyword "(" *> p <* keyword ")"

ident :: Parser String
ident = try $ token $ do
    c <- letter
    cs <- many (letter <|> digit)
    let id = c:cs
    if id `elem` keywords then
        empty
    else
        return id

idlist :: Parser [String]
idlist = ident `sepBy` keyword ","

num :: Parser Int
num = token $ do
    cs <- many1 digit
    return (read cs)

sym str val = keyword str >> return val

addop, mulop, relop :: Parser BinOp
addop = choice $ zipWith sym
    ["+",  "-",  "or"]
    [Plus, Minus, Or]
mulop = choice $ zipWith sym
    ["*",   "/", "div", "mod", "and"]
    [Times, Div, Div,   Mod,   And]
relop = choice $ zipWith sym
    ["=", "<>", "<=", ">=",  ">", "<"] -- order is important here; parsec will
    [Eq,  NEq,   LtEq, GtEq, Gt,  Lt]  -- choose the first match.


expr, relExpr, simpleExpr, term, factor :: Parser Expr

expr = relExpr

factor = choice [ liftM Var ident
                , parens expr
                , liftM Num num
                , sym "true" T
                , sym "false" F
                , liftM Not $ keyword "not" >> factor
                , liftM Pos $ keyword "+" >> factor
                , liftM Neg $ keyword "-" >> factor
                ]

relExpr = do
    lhs <- simpleExpr
    try $ do
        op <- relop
        rhs <- simpleExpr
        return (Op op lhs rhs)
      <|> return lhs

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

statement, assignmentStatement, ifStatement, whileStatement :: Parser Statement
statement = choice $ map try [ assignmentStatement
                             , liftM CompoundStatement compoundStatement
                             , ifStatement
                             , whileStatement
                             ]

assignmentStatement = do
    lhs <- ident
    keyword ":="
    rhs <- expr
    return (Assign lhs rhs)

compoundStatement :: Parser [Statement]
compoundStatement = do
    keyword "begin"
    stmts <- try statement `sepBy` semi
    keyword "end"
    return stmts

ifStatement = do
    keyword "if"
    e <- expr
    keyword "then"
    ifTrue <- statement
    ifFalse <- try (liftM Just $ keyword "else" >> statement)
                <|> return Nothing
    return (If e ifTrue ifFalse)

whileStatement = do
    keyword "while"
    e <- expr
    keyword "do"
    stmt <- statement
    return (While e stmt)

program :: Parser Program
program = do
    keyword "program"
    id <- ident
    args <- option [] (parens idlist)
    semi
    decls <- option [] (try declarations)
    stmts <- compoundStatement
    keyword "."
    return (Program id args decls stmts)

declarations :: Parser [([String], Type)]
declarations = do
    keyword "var"
    many1 $ try $ do
        vars <- idlist
        keyword ":"
        t <- ty
        semi
        return (vars, t)

ty, tyInt, tyBool :: Parser Type
ty = tyInt <|> tyBool

tyInt  = sym "integer" TyInt
tyBool = sym "boolean" TyBool

file :: Parser [Program]
file = spaces *> many program

parse :: String -> String -> Either ParseError [Program]
parse = runParser file ()
