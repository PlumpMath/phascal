module Phascal.SymbolTable (
    SymInfo(..),
    SymTable,
    makeSymTable,
    lookup
) where

import Prelude hiding (lookup)
import qualified Data.Map.Strict as M

import Phascal.Ast

data SymInfo = SymInfo { frameOffset :: Int
                       , ty          :: Type
                       } 

newtype SymTable = SymTable (M.Map String SymInfo)

-- TODO: We should check for duplicate entries.
makeSymTable :: Program -> SymTable
makeSymTable prog = SymTable $ foldl M.union M.empty
                                     (map makeOneType (decls prog))
  where
    makeOneType (vs, ty) = M.fromList $ zip
        vs
        (zipWith SymInfo [0..] (repeat ty))


lookup :: String -> SymTable -> Maybe SymInfo
lookup key (SymTable syms) = M.lookup key syms
