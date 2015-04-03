module ArmTests (armTests) where

import Test.HUnit
import Data.Bits
import Arm

        -- bytes in any position:
yes = [ 0x000000ff
      , 0x0000ff00
      , 0x00ff0000
      , 0xff000000
        -- misc:
      , 0x0003fc00
      , 0xc0000034
        -- any power of two:
      ] ++ map (shiftL 1) [0..31]

no = [ 0x00000102
     , 0xff0000ff
     , 0x00100001
     ]

armTests = TestList $
    [TestLabel (show num) (testCase num True)  | num <- yes] ++
    [TestLabel (show num) (testCase num False) | num <- no]
  where
    testCase num expected =
        TestCase $ assertEqual
                    ("expected " ++ show expected ++ " for " ++ show num)
                    (canImmediate num) expected
