{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}

module Day1Spec (spec) where

import Data.List.NonEmpty (NonEmpty)
import qualified Data.Text as T
import Day1 (
  Calories (Calories),
  Elf (Elf),
  Solution (Solution),
  parse,
  pretty,
  solve,
  solve1,
  solve2,
  winner,
  winners,
 )
import NeatInterpolation (trimming)
import Test.Hspec (Spec, describe, it, shouldBe)

input :: T.Text
input =
  [trimming|
        1000
        2000
        3000

        4000

        5000
        6000

        7000
        8000
        9000

        10000|]

elf1, elf2, elf3, elf4, elf5 :: Elf
elf1 = Elf [1_000, 2_000, 3_000]
elf2 = Elf [4_000]
elf3 = Elf [5_000, 6_000]
elf4 = Elf [7_000, 8_000, 9_000]
elf5 = Elf [10_000]

elfs :: NonEmpty Elf
elfs = [elf1, elf2, elf3, elf4, elf5]

spec :: Spec
spec = describe "Day 1" $ do
  it "winner" $
    winner elfs `shouldBe` elf4

  it "solve1" $
    solve1 elfs `shouldBe` (Calories 24_000)

  it "winners" $
    winners 5 elfs `shouldBe` [elf4, elf3, elf5, elf1, elf2]

  it "solve2" $
    solve2 elfs `shouldBe` (Calories 45_000)

  it "parse" $
    parse input `shouldBe` Right elfs

  it "parse input ending with newline" $
    let
      inputWithTrailingNewline = input <> "\n"
     in
      parse inputWithTrailingNewline `shouldBe` Right elfs

  it "round trip" $
    pretty elfs `shouldBe` Just input

  it "solution" $
    solve input `shouldBe` Right (Solution 24_000 45_000)

-- xit "solve the puzzle" $ do
--   input <- T.readFile "resources/input1"
--   logic input `shouldBe` Answer
