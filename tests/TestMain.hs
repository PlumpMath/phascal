module Main (main) where

import ParserTests

import Test.Framework
import Test.Framework.Providers.HUnit

main :: IO ()
main = defaultMain [testGroup "HUnit Testss" $ hUnitTestToTests hunitTests]
