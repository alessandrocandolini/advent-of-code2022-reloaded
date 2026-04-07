{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}

module Day5Spec where

import qualified Data.IntMap as M
import Data.Stack
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Day5
import NeatInterpolation (trimming)
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

input :: T.Text
input =
  [trimming|
   [D]
[N] [C]
[Z] [M] [P]
 1   2   3

move 1 from 2 to 1
move 3 from 1 to 3
move 2 from 2 to 1
move 1 from 1 to 2
|]

cargo :: Cargo String
cargo = [(1, ["N", "Z"]), (2, ["D", "C", "M"]), (3, ["P"])]

move1, move2, move3, move4 :: Move
move1 = Move 1 2 1
move2 = Move 3 1 3
move3 = Move 2 2 1
move4 = Move 1 1 2

moves :: [Move]
moves = [move1, move2, move3, move4]

spec :: Spec
spec = describe "Day 5" $ do
  it "confirm cargo IsList instance for syntax override works as intended" $
    let
      expected :: Cargo String
      expected =
        Cargo
          ( M.fromList
              [ (1, NonEmptyStack "N" (NonEmptyStack "Z" EmptyStack))
              , (2, NonEmptyStack "D" (NonEmptyStack "C" (NonEmptyStack "M" EmptyStack)))
              , (3, NonEmptyStack "P" EmptyStack)
              ]
          )
     in
      cargo `shouldBe` expected

  it "perform a valid single LIFO re-arrange" $
    let
      expected :: Cargo String
      expected = [(1, ["D", "N", "Z"]), (2, ["C", "M"]), (3, ["P"])]
     in
      rearrange Lifo (Move 1 2 1) cargo `shouldBe` expected

  it "perform a valid multi element LIFO re-arrange" $
    let
      initial, final :: Cargo String
      initial = [(1, ["D", "N", "Z"]), (2, ["C", "M"]), (3, ["P"])]
      final = [(1, []), (2, ["C", "M"]), (3, ["Z", "N", "D", "P"])]
     in
      rearrange Lifo (Move 3 1 3) initial `shouldBe` final

  it "perform a valid single FIFO re-arrange (behaviour must be the same as LIFO for a single element)" $
    let
      expected :: Cargo String
      expected = [(1, ["D", "N", "Z"]), (2, ["C", "M"]), (3, ["P"])]
     in
      rearrange Fifo (Move 1 2 1) cargo `shouldBe` expected

  it "perform a valid multi element FIFO re-arrange" $
    let
      initial, final :: Cargo String
      initial = [(1, ["D", "N", "Z"]), (2, ["C", "M"]), (3, ["P"])]
      final = [(1, []), (2, ["C", "M"]), (3, ["D", "N", "Z", "P"])]
     in
      rearrange Fifo (Move 3 1 3) initial `shouldBe` final
