{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}

module Day1 (parse, solve, program, solve1, solve2, Elf (Elf), Calories (Calories), Solution (Solution), pretty) where

import Control.Additive ((<|>))
import qualified Control.Monad.Indexed as I
import qualified Control.Monad.Indexed.Cont2 as Cont2
import Control.Monad.Indexed.Cont2.Lead.Generic (lead)
import Control.Monad.Indexed.Cont2.Lead.Labels ()
import Data.Foldable1 (Foldable1 (fold1))
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as N
import Data.Monoid (Sum (Sum))
import Data.Ord (Down (Down), comparing)
import qualified Data.Text as T
import GHC.Generics (Generic)
import qualified Prettyprinter as PP
import qualified Prettyprinter.Render.Text as RT
import qualified Text.Pup.MPR as P

data Solution = Solution {solution1 :: Calories, solution2 :: Calories} deriving (Eq, Show)

newtype Calories = Calories Int
  deriving (Eq, Ord, Show, Num) via Int
  deriving (Semigroup) via (Sum Int)
  deriving stock (Generic)

data Elf = Elf (NonEmpty Calories)
  deriving (Eq, Show)
  deriving stock (Generic)

totalCalories :: Elf -> Calories
totalCalories (Elf calories) = fold1 calories

solve1 :: NonEmpty Elf -> Calories
solve1 = maximum . fmap totalCalories

solve2 :: NonEmpty Elf -> Calories
solve2 = fold1 . takeFirst3 . N.sortBy (comparing Down) . fmap totalCalories
 where
  takeFirst3 = (N.:|) <$> N.head <*> (take 2 . N.tail)

solve :: T.Text -> Either String Solution
solve = fmap (Solution <$> solve1 <*> solve2) . parse

program :: T.Text -> IO ()
program = print . solve

-- bidirectional parsing

caloriesParser :: P.Pup' (Calories -> r) r Calories
caloriesParser =
  (lead @"Calories" @Calories :: P.Pup' (Calories -> r) (Int -> r) (Int -> Calories))
    I.<*> P.nat

elfParser :: P.Pup' (Elf -> r) r Elf
elfParser =
  (lead @"Elf" @Elf :: P.Pup' (Elf -> r) (NonEmpty Calories -> r) (NonEmpty Calories -> Elf))
    I.<*> foodParser
 where
  foodParser :: P.Pup' (NonEmpty Calories -> r) r (NonEmpty Calories)
  foodParser =
    ( ( lead @":|" @(NonEmpty Calories) ::
          P.Pup'
            (NonEmpty Calories -> r)
            (Calories -> [Calories] -> r)
            (Calories -> [Calories] -> NonEmpty Calories)
      )
        I.<*> caloriesParser
        I.<*> Cont2.many (P.try (P.hardline I.*> caloriesParser))
    )

elfsParser :: P.Pup' (NonEmpty Elf -> r) r (NonEmpty Elf)
elfsParser =
  ( ( lead @":|" @(NonEmpty Elf) ::
        P.Pup'
          (NonEmpty Elf -> r)
          (Elf -> [Elf] -> r)
          (Elf -> [Elf] -> NonEmpty Elf)
    )
      I.<*> elfParser
      I.<*> Cont2.many (P.try (P.hardline I.*> P.hardline I.*> elfParser))
  )
    I.<* (P.eof <|> (P.hardline I.<* P.eof))

parse :: T.Text -> Either String (NonEmpty Elf)
parse = P.parse elfsParser "Day 1 parsing"

pretty :: NonEmpty Elf -> Maybe T.Text
pretty =
  fmap (RT.renderStrict . PP.layoutPretty PP.defaultLayoutOptions)
    . P.print elfsParser
