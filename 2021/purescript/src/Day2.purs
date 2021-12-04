module Day2 where

import Prelude

import Data.Array (filter, foldl)
import Data.Int as Int
import Data.Maybe as Maybe
import Data.String as String
import Data.String.Utils as SUtils
import Partial.Unsafe (unsafeCrashWith)

type State = { horizontal :: Int, depth :: Int }
type StateWithAim = { horizontal :: Int, depth :: Int, aim :: Int }

solve :: String -> { pt1 :: Int, pt2 :: Int }
solve input = 
  let
    directions = 
      SUtils.lines input
        # filter (not <<< String.null)
        # map directionFromString
    pt1 =
      directions
        # foldl updateState initState
    pt2 =
      directions
        # foldl updateStateWithAim initStateWithAim
   in
   { pt1: pt1.horizontal * pt1.depth
   , pt2:  pt2.horizontal * pt2.depth
   }

unsafeIntFromString :: String -> Int
unsafeIntFromString =
  Int.fromString
    >>> Maybe.fromMaybe' (\_ -> unsafeCrashWith "invalid input")

data Direction
  = Forward Int
  | Up Int
  | Down Int

directionFromString :: String -> Direction
directionFromString directionText = 
  case String.split (String.Pattern " ") directionText of
    ["forward", n] -> Forward $ unsafeIntFromString n
    ["up", n] -> Up $ unsafeIntFromString n
    ["down", n] -> Down $ unsafeIntFromString n
    _ -> unsafeCrashWith "invalid input"

initState :: State
initState = { horizontal: 0, depth: 0 }

initStateWithAim :: StateWithAim
initStateWithAim = { horizontal: 0, depth: 0, aim: 0 }

updateState :: State -> Direction -> State
updateState s@{ horizontal, depth } = case _ of
  Forward n -> s { horizontal = horizontal + n }
  Down n -> s { depth = depth + n }
  Up n -> s { depth = depth - n }

updateStateWithAim :: StateWithAim -> Direction -> StateWithAim
updateStateWithAim s@{ horizontal, depth, aim } = case _ of
  Forward n -> 
    s { horizontal = horizontal + n, depth = depth + aim * n }
  Down n -> s { aim = aim + n }
  Up n -> s { aim = aim - n }
