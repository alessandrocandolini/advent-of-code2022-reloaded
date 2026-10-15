{-# LANGUAGE DerivingVia #-}

module Day10 where

import Control.Applicative ((<|>))
import Control.Monad.Combinators.NonEmpty (sepEndBy1)
import Data.Bifunctor (Bifunctor (first))
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as N
import Data.Monoid (Endo (..))
import Data.Semigroup (Sum (Sum))
import qualified Data.Text as T
import Data.Void (Void)
import Text.Megaparsec (MonadParsec (eof), Parsec, errorBundlePretty, runParser)
import Text.Megaparsec.Char (newline)
import Text.Megaparsec.Char.Lexer (decimal, signed)

data Solution = Solution Strength Strength deriving (Eq, Show)

newtype Register = Register Int
  deriving (Eq, Show, Num) via Int
  deriving (Semigroup, Monoid) via (Sum Int)

newtype Cycle = Cycle Int
  deriving (Eq, Show, Num) via Int
  deriving (Semigroup) via (Sum Int)

newtype Strength = Strength Int
  deriving (Eq, Show, Num) via Int
  deriving (Semigroup, Monoid) via (Sum Int)

strength :: Register -> Cycle -> Strength
strength (Register x) (Cycle c) = Strength (c * x)

data Cpu = Cpu Register Cycle deriving (Eq, Show)

initCpu :: Cpu
initCpu = Cpu 1 1

data Instruction = NoOpt | AddX Register deriving (Eq, Show)

runInstruction :: Instruction -> Cpu -> NonEmpty Cpu
runInstruction NoOpt (Cpu x c) = N.singleton $ Cpu x (c <> 1)
runInstruction (AddX value) (Cpu x c) = Cpu x (c <> 1) N.:| [Cpu (x <> value) (c <> 2)]

runInstructions :: NonEmpty Instruction -> NonEmpty Cpu -> NonEmpty Cpu
runInstructions = appEndo . foldMap (Endo . runInstruction') . N.reverse
 where
  runInstruction' :: Instruction -> NonEmpty Cpu -> NonEmpty Cpu
  runInstruction' i env = env <> runInstruction i (N.last env)

isSampleCycle :: Cycle -> Bool
isSampleCycle (Cycle 20) = True
isSampleCycle (Cycle n) = (n - 20) `mod` 40 == 0

isSample :: Cpu -> Bool
isSample (Cpu _ c) = isSampleCycle c

strengthFromCpu :: Cpu -> Strength
strengthFromCpu (Cpu x c) = strength x c

sampledCpu1 :: NonEmpty Instruction -> [Cpu]
sampledCpu1 = N.filter isSample . flip runInstructions (N.singleton initCpu)

solve1 :: NonEmpty Instruction -> Strength
solve1 = foldMap strengthFromCpu . sampledCpu1

solve2 :: NonEmpty Instruction -> Strength
solve2 = solve1

program :: T.Text -> IO ()
program = print . solve

solve :: T.Text -> Either String Solution
solve = fmap (Solution <$> solve1 <*> solve2) . parse

type Parser = Parsec Void T.Text

xParser :: Parser Register
xParser = Register <$> signed (return ()) decimal

instructionParser :: Parser Instruction
instructionParser = (NoOpt <$ "noop") <|> (AddX <$> ("addx " *> xParser))

instructionsParser :: Parser (NonEmpty Instruction)
instructionsParser = sepEndBy1 instructionParser newline <* eof

parse :: T.Text -> Either String (NonEmpty Instruction)
parse = first errorBundlePretty . runParser instructionsParser "Day 10 parsing"
