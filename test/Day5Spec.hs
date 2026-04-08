{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}

module Day5Spec where

import qualified Data.IntMap as M
import Data.List.NonEmpty (NonEmpty)
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

cargo :: Cargo Crate
cargo = [(1, Crate <$> ['N', 'Z']), (2, Crate <$> ['D', 'C', 'M']), (3, Crate <$> ['P'])]

move1, move2, move3, move4 :: Move
move1 = Move 1 2 1
move2 = Move 3 1 3
move3 = Move 2 2 1
move4 = Move 1 1 2

moves :: NonEmpty Move
moves = [move1, move2, move3, move4]

spec :: Spec
spec = describe "Day 5" $ do
  it "confirm cargo IsList instance for syntax override works as intended" $
    let
      expected :: Cargo Crate
      expected =
        Cargo
          ( M.fromList
              [ (1, NonEmptyStack (Crate 'N') (NonEmptyStack (Crate 'Z') EmptyStack))
              , (2, NonEmptyStack (Crate 'D') (NonEmptyStack (Crate 'C') (NonEmptyStack (Crate 'M') EmptyStack)))
              , (3, NonEmptyStack (Crate 'P') EmptyStack)
              ]
          )
     in
      cargo `shouldBe` expected

  it "perform a valid single LIFO re-arrange between two different existing stacks" $
    let
      expected :: Cargo Crate
      expected = [(1, Crate <$> ['D', 'N', 'Z']), (2, Crate <$> ['C', 'M']), (3, Crate <$> ['P'])]
     in
      rearrange Lifo (Move 1 2 1) cargo `shouldBe` expected

  it "perform a valid multi element LIFO re-arrange between two different existing stacks" $
    let
      initial, final :: Cargo Crate
      initial = [(1, Crate <$> ['D', 'N', 'Z']), (2, Crate <$> ['C', 'M']), (3, Crate <$> ['P'])]
      final = [(1, []), (2, Crate <$> ['C', 'M']), (3, Crate <$> ['Z', 'N', 'D', 'P'])]
     in
      rearrange Lifo (Move 3 1 3) initial `shouldBe` final

  it "perform a valid multi element LIFO re-arrange between two different stacks, one of which does not exist" $
    let
      initial, final :: Cargo Crate
      initial = [(1, Crate <$> ['D', 'N', 'Z']), (2, Crate <$> ['C', 'M']), (3, Crate <$> ['P'])]
      final = [(1, []), (2, Crate <$> ['C', 'M']), (3, Crate <$> ['P']), (4, Crate <$> ['Z', 'N', 'D'])]
     in
      rearrange Lifo (Move 3 1 4) initial `shouldBe` final

  it "perform a valid multi element LIFO re-arrange between two different stacks, one of which is empty" $
    let
      initial, final :: Cargo Crate
      initial = [(1, Crate <$> ['D', 'N', 'Z']), (2, Crate <$> ['C', 'M']), (3, Crate <$> ['P']), (4, EmptyStack)]
      final = [(1, []), (2, Crate <$> ['C', 'M']), (3, Crate <$> ['P']), (4, Crate <$> ['Z', 'N', 'D'])]
     in
      rearrange Lifo (Move 3 1 4) initial `shouldBe` final

  it "perform a valid single FIFO re-arrange between two different existing stacks (behaviour must be the same as LIFO for a single element)" $
    let
      expected :: Cargo Crate
      expected = [(1, Crate <$> ['D', 'N', 'Z']), (2, Crate <$> ['C', 'M']), (3, Crate <$> ['P'])]
     in
      rearrange Fifo (Move 1 2 1) cargo `shouldBe` expected

  it "perform a valid multi element FIFO re-arrange between two different existing stacks" $
    let
      initial, final :: Cargo Crate
      initial = [(1, Crate <$> ['D', 'N', 'Z']), (2, Crate <$> ['C', 'M']), (3, Crate <$> ['P'])]
      final = [(1, []), (2, Crate <$> ['C', 'M']), (3, Crate <$> ['D', 'N', 'Z', 'P'])]
     in
      rearrange Fifo (Move 3 1 3) initial `shouldBe` final

  it "perform a valid multi element FIFO re-arrange between two different stacks, one of which does not exist" $
    let
      initial, final :: Cargo Crate
      initial = [(1, Crate <$> ['D', 'N', 'Z']), (2, Crate <$> ['C', 'M']), (3, Crate <$> ['P'])]
      final = [(1, []), (2, Crate <$> ['C', 'M']), (3, Crate <$> ['P']), (4, Crate <$> ['D', 'N', 'Z'])]
     in
      rearrange Fifo (Move 3 1 4) initial `shouldBe` final

  it "perform a valid multi element FIFO re-arrange between two different stacks, one of which is empty" $
    let
      initial, final :: Cargo Crate
      initial = [(1, Crate <$> ['D', 'N', 'Z']), (2, Crate <$> ['C', 'M']), (3, Crate <$> ['P']), (4, EmptyStack)]
      final = [(1, []), (2, Crate <$> ['C', 'M']), (3, Crate <$> ['P']), (4, Crate <$> ['D', 'N', 'Z'])]
     in
      rearrange Fifo (Move 3 1 4) initial `shouldBe` final

  it "solve1 on a cargo that contains empty lines" $
    let
      moves' = [move1, move2, move3]
     in
      solve1 (Input cargo moves') `shouldBe` "MZ"

  it "solve1" $
    solve1 (Input cargo moves) `shouldBe` "CMZ"

  it "solve2 on a cargo that contains empty lines" $
    let
      moves' = [move1, move2, move3]
     in
      solve2 (Input cargo moves') `shouldBe` "CD"

  it "solve2" $
    solve2 (Input cargo moves) `shouldBe` "MCD"
