module Main (main) where

import ParserTests
import ArmTests
import TypeTests
import ExecTests
import CodegenTypeTests

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2

main :: IO ()
main = defaultMain [ testGroup "Parser tests" $ hUnitTestToTests parserTests
                   , testGroup "Arm tests"    $ hUnitTestToTests    armTests
                   , testGroup "Type tests"   $ hUnitTestToTests   typeTests
                   , testGroup "Exec Tests"   $ hUnitTestToTests   execTests

                   , testProperty "If typechecks then generates code" ifTypeThenCode
                   ]
