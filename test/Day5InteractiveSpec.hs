{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}
module Day5InteractiveSpec where

import Data.List.NonEmpty (NonEmpty)
import qualified Data.Text as T
import Day5
import NeatInterpolation (untrimming)
import Test.Hspec (Spec, describe, it, shouldBe)

input :: T.Text
input =
  T.drop
    1
    [untrimming|
    [D]
[N] [C]
[Z] [M] [P]
 1   2   3

move 1 from 2 to 1
move 3 from 1 to 3
move 2 from 2 to 1
move 1 from 1 to 2
|]

sampleCargo :: Cargo Crate
sampleCargo = [(1, Crate <$> ['N', 'Z']), (2, Crate <$> ['D', 'C', 'M']), (3, Crate <$> ['P'])]

move1, move2, move3, move4 :: Move
move1 = Move 1 2 1
move2 = Move 3 1 3
move3 = Move 2 2 1
move4 = Move 1 1 2

moves :: NonEmpty Move
moves = [move1, move2, move3, move4]

spec :: Spec
spec = describe "Day 5" $ do
  it "placeholder" $
    True `shouldBe` True
