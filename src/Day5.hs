{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeFamilies #-}

module Day5 where

import Control.Applicative (optional)
import Control.Applicative.Combinators (between, (<|>))
import Control.Applicative.Combinators.NonEmpty (endBy1, sepBy1, sepEndBy1)
import Data.Bifunctor (Bifunctor (first))
import qualified Data.IntMap as M
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as N
import Data.Maybe (fromMaybe)
import Data.Monoid (Endo (Endo, appEndo))
import Data.Stack (
  Stack (EmptyStack),
  StackOrder (..),
  move,
  peek,
  shuffle,
 )
import qualified Data.Text as T
import Data.Void (Void)
import GHC.Exts (IsList (fromList, toList), Item)
import Text.Megaparsec (MonadParsec (eof), Parsec, errorBundlePretty, runParser)
import Text.Megaparsec.Char (char, letterChar, newline)
import Text.Megaparsec.Char.Lexer (decimal)
import Witherable (catMaybes, mapMaybe)

data Solution = Solution {solution1 :: String, solution2 :: String} deriving (Eq, Show)

newtype Crate = Crate {unwrapCrate :: Char} deriving (Eq, Show)

newtype Cargo a = Cargo {stacks :: M.IntMap (Stack a)} deriving (Eq, Show)

instance IsList (Cargo a) where
  type Item (Cargo a) = (Int, Stack a)
  fromList = Cargo . M.fromList
  toList = M.toList . stacks

peeks :: Cargo a -> [a]
peeks = mapMaybe peek . M.elems . stacks

data Move = Move {numberOfElements :: Int, from :: Int, to :: Int} deriving (Eq, Show)

getStackAtPosition :: Int -> Cargo a -> Stack a
getStackAtPosition p = fromMaybe EmptyStack . M.lookup p . stacks

insertOrUpdateStackAtPosition :: Int -> Stack a -> Cargo a -> Cargo a
insertOrUpdateStackAtPosition p s = Cargo . M.insert p s . stacks

getStacksByMove :: Move -> Cargo a -> (Stack a, Stack a)
getStacksByMove m c = (getStackAtPosition (from m) c, getStackAtPosition (to m) c)

insertOrUpdateStacksByMove :: Move -> Stack a -> Stack a -> Cargo a -> Cargo a
insertOrUpdateStacksByMove m s d c =
  let
    c' = insertOrUpdateStackAtPosition (from m) s c
    c'' = insertOrUpdateStackAtPosition (to m) d c'
   in
    c''

rearrange :: StackOrder -> Move -> Cargo a -> Cargo a
rearrange o m c
  | from m == to m =
      let
        pos = from m
        s = getStackAtPosition pos c
        s' = shuffle o (numberOfElements m) s
       in
        insertOrUpdateStackAtPosition pos s' c
  | otherwise =
      let
        (s, d) = getStacksByMove m c
        (s', d') = move o (numberOfElements m) s d
       in
        insertOrUpdateStacksByMove m s' d' c

rearrangeAll :: StackOrder -> NonEmpty Move -> Cargo a -> Cargo a
rearrangeAll o = appEndo . foldMap (Endo . rearrange o) . N.reverse

data Input = Input (Cargo Crate) (NonEmpty Move) deriving (Eq, Show)

solve1 :: Input -> String
solve1 = solveCommon Lifo

solve2 :: Input -> String
solve2 = solveCommon Fifo

solveCommon :: StackOrder -> Input -> String
solveCommon order (Input cargo moves) = fmap unwrapCrate (peeks (rearrangeAll order moves cargo))

solve :: T.Text -> Either String Solution
solve = fmap (Solution <$> solve1 <*> solve2) . parseInput

program :: T.Text -> IO ()
program = print . solve

type Parser = Parsec Void T.Text

crateParser :: Parser Crate
crateParser = Crate <$> between (char '[') (char ']') letterChar

maybeCrateParser :: Parser (Maybe Crate)
maybeCrateParser = (Just <$> crateParser) <|> (Nothing <$ "   ")

cratesRowParser :: Parser (NonEmpty (Maybe Crate))
cratesRowParser = sepBy1 maybeCrateParser (char ' ')

cratesParser :: Parser (NonEmpty (NonEmpty (Maybe Crate)))
cratesParser = endBy1 cratesRowParser newline

numbersParser :: Parser (NonEmpty Int)
numbersParser = sepBy1 indexParser (char ' ')
 where
  indexParser = char ' ' *> decimal <* optional (char ' ')

cargoParser :: Parser (Cargo Crate)
cargoParser = (flip postProcessing) <$> cratesParser <*> numbersParser

postProcessing :: NonEmpty Int -> NonEmpty (NonEmpty (Maybe Crate)) -> Cargo Crate
postProcessing numbers = fromList . toList . N.zip numbers . fmap (fromList . catMaybes . toList) . N.transpose

moveParser :: Parser Move
moveParser = Move <$> ("move " *> decimal <* " from ") <*> (decimal <* " to ") <*> decimal

movesParser :: Parser (NonEmpty Move)
movesParser = sepEndBy1 moveParser newline

parser :: Parser Input
parser = Input <$> (cargoParser <* newline <* newline) <*> (movesParser <* eof)

parseInput :: T.Text -> Either String Input
parseInput = parse parser

parse :: Parser a -> T.Text -> Either String a
parse p = first errorBundlePretty . runParser p "Day 5 parsing"
