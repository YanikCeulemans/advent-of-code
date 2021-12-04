module Test.Day2 where

import Prelude

import Day2 (solve)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

input :: String
input = """forward 5
down 5
forward 8
up 3
down 8
forward 2

"""

main :: Spec Unit
main = do
  describe "Day 2" do
    describe "solve" do
      it "should return the correct result" do
        solve input `shouldEqual` { pt1: 150, pt2: 900 }
