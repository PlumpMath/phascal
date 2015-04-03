module Phascal.SymbolTable where

import qualified Data.Map.Strict as M

import Phascal.Ast

data SymInfo = SymInfo { frameOffset :: Int
                       , ty          :: Type
                       } 

type SymTable = M.Map String SymInfo

-- TODO: We should check for duplicate entries.
makeSymTable :: Program -> SymTable
makeSymTable prog = foldl M.union M.empty (map makeOneType (decls prog))
  where
    makeOneType :: ([String], Type) -> SymTable
    makeOneType (vs, ty) = M.fromList $ zip
        vs
        (zipWith SymInfo [0..] (repeat ty))


lookup :: String -> SymTable -> Maybe SymInfo
lookup = M.lookup
