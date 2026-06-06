{-# LANGUAGE DerivingVia #-}

module Day10 where

import Args (withInfo)
import Control.Applicative ((<|>))
import Control.Monad.Combinators.NonEmpty (sepEndBy1)
import Data.Bifunctor (Bifunctor (first))
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as N
import Data.Monoid (Endo (..))
import Data.Semigroup (Sum (Sum))
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Void (Void)
import Text.Megaparsec (MonadParsec (eof), Parsec, errorBundlePretty, runParser)
import Text.Megaparsec.Char (newline)
import Text.Megaparsec.Char.Lexer (decimal, signed)

data Solution = Solution Signal Signal deriving (Eq, Show)

newtype Signal = Signal Int
  deriving (Eq, Show, Num) via Int
  deriving (Semigroup, Monoid) via (Sum Int)

newtype X = X Int
  deriving (Eq, Show, Num) via Int
  deriving (Semigroup, Monoid) via (Sum Int)

newtype Cycle = Cycle Int
  deriving (Eq, Show, Num) via Int
  deriving (Semigroup) via (Sum Int)

strength :: X -> Cycle -> Signal
strength (X x) (Cycle c) = Signal (c * x)

data Env = Env X Cycle
  deriving (Eq, Show)

initEnv :: Env
initEnv = Env 1 0

data Instruction = NoOpt | AddX X deriving (Eq, Show)

runInstruction :: Instruction -> Env -> NonEmpty Env
runInstruction NoOpt (Env x c) = N.singleton $ Env x (c <> 1)
runInstruction (AddX value) (Env x c) = Env x (c <> 1) N.:| [Env (x <> value) (c <> 2)]

runInstruction' :: Instruction -> NonEmpty Env -> NonEmpty Env
runInstruction' i env = env <> runInstruction i (N.last env)

runInstructions :: NonEmpty Instruction -> NonEmpty Env -> NonEmpty Env
runInstructions = appEndo . foldMap (Endo . runInstruction') . N.reverse

isSampleCycle :: Cycle -> Bool
isSampleCycle (Cycle 20) = True
isSampleCycle (Cycle n) = (n - 20) `mod` 40 == 0

isSample :: Env -> Bool
isSample (Env _ c) = isSampleCycle c

strengthFromEnv :: Env -> Signal
strengthFromEnv (Env x c) = strength x c

sampledEnv1 :: NonEmpty Instruction -> [Env]
sampledEnv1 = N.filter isSample . flip runInstructions (N.singleton initEnv)

solve1 :: NonEmpty Instruction -> Signal
solve1 = foldMap strengthFromEnv . sampledEnv1

solve2 :: NonEmpty Instruction -> Signal
solve2 = solve1

program :: T.Text -> IO ()
program = print . solve

solve :: T.Text -> Either String Solution
solve = fmap (Solution <$> solve1 <*> solve2) . parse

type Parser = Parsec Void T.Text

xParser :: Parser X
xParser = X <$> signed (return ()) decimal

instructionParser :: Parser Instruction
instructionParser = (NoOpt <$ "noop") <|> (AddX <$> ("addx " *> xParser))

instructionsParser :: Parser (NonEmpty Instruction)
instructionsParser = sepEndBy1 instructionParser newline <* eof

parse :: T.Text -> Either String (NonEmpty Instruction)
parse = first errorBundlePretty . runParser instructionsParser "Day 10 parsing"
