module Main where

import Prelude

import Day1 as Day1
import Day2 as Day2
import Day3 as Day3
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class.Console as Console
import Node.Encoding (Encoding(..))
import Node.FS.Aff as FS

main :: Effect Unit
main = do
  launchAff_ do
    day1Solution <- Day1.solve <$> FS.readTextFile UTF8 "./input.txt"
    Console.log $ "Day 1 solution: " <> show day1Solution

    day2Solution <- Day2.solve <$> FS.readTextFile UTF8 "./input2.txt"
    Console.log $ "Day 2 solution: " <> show day2Solution

    day3Solution <- Day3.solve <$> FS.readTextFile UTF8 "./input3.txt"
    Console.log $ "Day 3 solution: " <> show day3Solution

