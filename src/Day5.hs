{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeFamilies #-}

module Day5 where

import Data.Bifunctor (Bifunctor (first))
import qualified Data.IntMap as M
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as N
import Data.Maybe (fromMaybe)
import Data.Monoid (Endo (Endo, appEndo), Sum (Sum))
import Data.Ord (Down (Down), comparing)
import Data.Stack
import qualified Data.Text as T
import Data.Void (Void)
import GHC.Exts (IsList (fromList, toList), Item)
import Text.Megaparsec (MonadParsec (eof), Parsec, errorBundlePretty, runParser)
import Text.Megaparsec.Char (newline)
import Text.Megaparsec.Char.Lexer (decimal)
import Witherable (mapMaybe)

data Solution = Solution {solution1 :: String, solution2 :: String} deriving (Eq, Show)

newtype Crate = Crate {unwrapCrate :: Char} deriving (Eq, Show)

newtype Cargo a = Cargo {stacks :: M.IntMap (Stack a)} deriving (Eq, Show)

instance IsList (Cargo a) where
  type Item (Cargo a) = (Int, Stack a)
  fromList = Cargo . M.fromList
  toList = M.toList . stacks

peeks :: Cargo a -> [a]
peeks = mapMaybe peek . M.elems . stacks

data Move = Move {count :: Int, from :: Int, to :: Int} deriving (Eq, Show)

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
        s' = shuffle o (count m) s
       in
        insertOrUpdateStackAtPosition pos s' c
  | otherwise =
      let
        (s, d) = getStacksByMove m c
        (s', d') = move o (count m) s d
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
solve = fmap (Solution <$> solve1 <*> solve2) . parse

program :: T.Text -> IO ()
program = print . solve

type Parser = Parsec Void T.Text

crateParser :: Parser Crate
crateParser = undefined

moveParser :: Parser Move
moveParser = undefined

parser :: Parser Input
parser = undefined

parse :: T.Text -> Either String Input
parse = first errorBundlePretty . runParser parser "Day 1 parsing"
