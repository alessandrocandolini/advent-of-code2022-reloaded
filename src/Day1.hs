{-# LANGUAGE DerivingVia #-}

module Day1 (parse, solve, program, solve1, solve2, Elf (Elf), Calories (Calories), Solution (Solution)) where

import Control.Monad.Combinators.NonEmpty (sepEndBy1)
import Data.Bifunctor (first)
import Data.Foldable1 (Foldable1 (fold1))
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as N
import Data.Monoid (Sum (Sum))
import Data.Ord (Down (Down), comparing)
import qualified Data.Text as T
import Data.Void (Void)
import Text.Megaparsec (MonadParsec (eof), Parsec, errorBundlePretty, runParser)
import Text.Megaparsec.Char (newline)
import Text.Megaparsec.Char.Lexer (decimal)

data Solution = Solution {solution1 :: Calories, solution2 :: Calories} deriving (Eq, Show)

newtype Calories = Calories Int
  deriving (Eq, Ord, Show, Num) via Int
  deriving (Semigroup) via (Sum Int)

data Elf = Elf (NonEmpty Calories) deriving (Eq, Show)

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

type Parser = Parsec Void T.Text

caloriesParser :: Parser Calories
caloriesParser = Calories <$> decimal

elfParser :: Parser Elf
elfParser = Elf <$> sepEndBy1 caloriesParser newline

elfsParser :: Parser (NonEmpty Elf)
elfsParser = sepEndBy1 elfParser newline <* eof

parse :: T.Text -> Either String (NonEmpty Elf)
parse = first errorBundlePretty . runParser elfsParser "Day 1 parsing"
