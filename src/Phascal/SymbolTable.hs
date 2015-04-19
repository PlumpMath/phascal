module Phascal.SymbolTable (
    SymInfo(..),
    SymTable,
    makeSymTable,
    lookup,
    fromList,
    empty
) where

import Prelude hiding (lookup)
import qualified Data.Map.Strict as M

import Phascal.Ast
import Phascal.Types

data SymInfo = SymInfo { frameOffset :: Int
                       , ty          :: Type
                       } deriving(Show)

newtype SymTable = SymTable (M.Map String SymInfo)

-- | @(makeSymTable prog)@ is a symbol table constructed from prog.
-- It contains SymInfo entries for each variable declared at top level.
--
-- This doesn't do much sanity checking, though it should and probably will in
-- the future. In particular, the value is unspecified if:
--
-- * The same variable is declared more than once.
-- * A variable is used but not declared.
makeSymTable :: Program -> SymTable
makeSymTable prog = SymTable $ foldl M.union M.empty
                                     (map makeOneType (decls prog))
  where
    makeOneType (vs, ty) = M.fromList $ zip
        vs
        (zipWith SymInfo [0..] (repeat ty))


lookup :: String -> SymTable -> Maybe SymInfo
lookup key (SymTable syms) = M.lookup key syms

fromList :: [(String, SymInfo)] -> SymTable
fromList = SymTable . M.fromList

empty :: SymTable
empty = SymTable M.empty


instance (Show SymTable) where
    show (SymTable m) = "SymTable " ++ (show $ M.toList m)
