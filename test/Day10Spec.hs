{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}

module Day10Spec where

import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as N
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Day10
import NeatInterpolation (trimming, untrimming)
import Stats (StatsError (Not200))
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

smallInput :: T.Text
smallInput =
  [trimming|
noop
addx 3
addx -5
|]

simpleInstructions :: NonEmpty Instruction
simpleInstructions = [NoOpt, AddX 3, AddX (-5)]

input :: T.Text
input =
  [trimming|
addx 15
addx -11
addx 6
addx -3
addx 5
addx -1
addx -8
addx 13
addx 4
noop
addx -1
addx 5
addx -1
addx 5
addx -1
addx 5
addx -1
addx 5
addx -1
addx -35
addx 1
addx 24
addx -19
addx 1
addx 16
addx -11
noop
noop
addx 21
addx -15
noop
noop
addx -3
addx 9
addx 1
addx -3
addx 8
addx 1
addx 5
noop
noop
noop
noop
noop
addx -36
noop
addx 1
addx 7
noop
noop
noop
addx 2
addx 6
noop
noop
noop
noop
noop
addx 1
noop
noop
addx 7
addx 1
noop
addx -13
addx 13
addx 7
noop
addx 1
addx -33
noop
noop
noop
addx 2
noop
noop
noop
addx 8
noop
addx -1
addx 2
addx 1
noop
addx 17
addx -9
addx 1
addx 1
addx -3
addx 11
noop
noop
addx 1
noop
addx 1
noop
noop
addx -13
addx -19
addx 1
addx 3
addx 26
addx -30
addx 12
addx -1
addx 3
addx 1
noop
noop
noop
addx -9
addx 18
addx 1
addx 2
noop
noop
addx 9
noop
noop
noop
addx -1
addx 2
addx -37
addx 1
addx 3
noop
addx 15
addx -21
addx 22
addx -6
addx 1
noop
addx 2
addx 1
noop
addx -10
noop
noop
addx 20
addx 1
addx 2
addx 2
addx -6
addx -11
noop
noop
noop
|]

spec :: Spec
spec = describe "Day 10" $ do
  it "first 6 samples" $
    all isSampleCycle ([20, 60, 100, 140, 180, 220] :: NonEmpty Cycle) `shouldBe` True

  it "parse small input" $
    parse smallInput `shouldBe` Right simpleInstructions

  it "sampledEnv1" $
    fmap sampledEnv1 (parse input)
      `shouldBe` Right
        [ Env 21 20
        , Env 19 60
        , Env 18 100
        , Env 21 140
        , Env 16 180
        , Env 18 220
        ]

  it "solve" $
    solve input `shouldBe` Right (Solution 13140 13140)

  prop "" $
    \l -> reverse (reverse l) == (l :: [Int])
