module Main (main) where

import ParserTests
import ArmTests

import Test.Framework
import Test.Framework.Providers.HUnit

main :: IO ()
main = defaultMain [ testGroup "Parser tests" $ hUnitTestToTests parserTests
                   , testGroup "Arm tests"    $ hUnitTestToTests    armTests
                   ]
