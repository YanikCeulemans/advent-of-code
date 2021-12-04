module Test.Day3 where

import Prelude

import Day3 (solve)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

input :: String
input = """00100
11110
10110
10111
10101
01111
00111
11100
10000
11001
00010
01010

"""

main :: Spec Unit
main = do
  describe "Day 3" do
    describe "solve" do
      it "should return the correct result" do
        solve input `shouldEqual` { pt1: 198, pt2: 230 }
