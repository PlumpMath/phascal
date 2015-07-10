module Phascal.TypeChecker where

import Prelude hiding (lookup)
import Phascal.Ast
import Phascal.Types
import Phascal.SymbolTable

data TypeError = TypeError { expected :: Type
                           , actual   :: Type
                           }

data Side = L | R

requireType :: Type -> SymTable -> Expr -> Either TypeError Type
requireType typ syms expr = typeOf syms expr >>= \t -> case t of
    typ' | typ' == typ -> Right typ'
    typ' -> Left (TypeError typ typ')

typeOf :: SymTable -> Expr -> Either TypeError Type
typeOf syms expr = case expr of
    T -> return TyBool
    F -> return TyBool
    (Not expr') -> requireType TyBool syms expr' >> return TyBool
    (Num _) -> return TyInt
    (Neg expr') -> requireType TyInt syms expr' >> return TyInt
    (Pos expr') -> requireType TyInt syms expr' >> return TyInt
    (Var name) -> case lookup name syms of
        Nothing -> error ("No such variable " ++ name ++
                           "! This should have been caught before " ++
                           "typechecking!")
        Just info -> return (ty info)
    (Op op lhs rhs) -> do
        let lhTy = operandType L op
        let rhTy = operandType R op
        requireType lhTy syms lhs
        requireType rhTy syms rhs
        return (operatorType op)

operatorType op = case op of
    Plus -> TyInt
    Minus -> TyInt
    Times -> TyInt
    Div -> TyInt
    Mod -> TyInt
    And -> TyBool
    Or -> TyBool
    Eq -> TyBool
    NEq -> TyBool
    Lt -> TyBool
    LtEq -> TyBool
    Gt -> TyBool
    GtEq -> TyBool

operandType side op = case (side, op) of
    (_,Plus) -> TyInt
    (_,Minus) -> TyInt
    (_,Times) -> TyInt
    (_,Div) -> TyInt
    (_,Mod) -> TyInt
    (_,And) -> TyBool
    (_,Or) -> TyBool
    (_,Eq) -> TyInt
    (_,NEq) -> TyInt
    (_,Lt) -> TyInt
    (_,LtEq) -> TyInt
    (_,Gt) -> TyInt
    (_,GtEq) -> TyInt
