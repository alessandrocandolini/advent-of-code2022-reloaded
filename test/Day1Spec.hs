{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}

module Day1Spec (spec) where

import Data.Either (isLeft)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.Text as T
import Day1 (
  Calories (Calories),
  Elf (Elf),
  Solution (Solution),
  parse,
  solve,
  solve1,
  solve2,
  topWinners,
  winner,
 )
import NeatInterpolation (trimming)
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

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
    topWinners 5 elfs `shouldBe` [elf4, elf3, elf5, elf1, elf2]

  it "topWinners deliberately always returns at least the winner" $
    topWinners 0 elfs `shouldBe` [elf4]

  it "solve2" $
    solve2 elfs `shouldBe` (Calories 45_000)

  it "parse" $
    parse input `shouldBe` Right elfs

  it "parse input ending with newline" $
    parse (input <> "\n") `shouldBe` Right elfs

  it "fail to parse with invalid end of file input" $
    parse (input <> "\nxyz") `shouldSatisfy` isLeft

  it "fail to parse empty input" $
    parse "" `shouldSatisfy` isLeft

  it "fail to parse input containing non numeric characters" $
    parse "123\n123x\n123" `shouldSatisfy` isLeft

  it "fail to parse input containing wrong multi elf separator" $
    parse "1000\n\n\n2000" `shouldSatisfy` isLeft

  it "solution" $
    solve input `shouldBe` Right (Solution 24_000 45_000)
