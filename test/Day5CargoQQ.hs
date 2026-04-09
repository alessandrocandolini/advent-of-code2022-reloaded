{-# LANGUAGE TemplateHaskell #-}

module Day5CargoQQ (cargo) where

import Data.Char (isSpace)
import Data.List (intercalate)
import qualified Data.List as L
import qualified Data.IntMap as M
import Data.Stack (Stack (EmptyStack, NonEmptyStack))
import qualified Data.Text as T
import Day5 (Cargo (Cargo), Crate (Crate), cargoParser, parse)
import Language.Haskell.TH (ExpQ, listE)
import Language.Haskell.TH.Quote (QuasiQuoter (QuasiQuoter, quoteDec, quoteExp, quotePat, quoteType))

cargo :: QuasiQuoter
cargo =
  QuasiQuoter
    { quoteExp = quoteCargoExp
    , quotePat = unsupportedContext "patterns"
    , quoteType = unsupportedContext "types"
    , quoteDec = unsupportedContext "declarations"
    }
 where
  unsupportedContext context _ = fail ("cargo quasiquoter does not support " <> context)

quoteCargoExp :: String -> ExpQ
quoteCargoExp input =
  case parse cargoParser (T.pack (normalizeCargoLiteral input)) of
    Left err -> fail err
    Right parsedCargo -> cargoExp parsedCargo

normalizeCargoLiteral :: String -> String
normalizeCargoLiteral =
  intercalate "\n"
    . stripCargoMargin
    . trimBlankEdgeLines
    . lines
    . trimLeadingNewline

trimLeadingNewline :: String -> String
trimLeadingNewline ('\n' : rest) = rest
trimLeadingNewline input = input

stripCargoMargin :: [String] -> [String]
stripCargoMargin rows = fmap (drop margin) rows
 where
  margin =
    case lastNonBlankLine rows of
      Nothing -> 0
      Just numbersRow -> max 0 (leadingIndent numbersRow - 1)

lastNonBlankLine :: [String] -> Maybe String
lastNonBlankLine [] = Nothing
lastNonBlankLine rows = Just (last (filter (not . all isSpace) rows))

trimBlankEdgeLines :: [String] -> [String]
trimBlankEdgeLines = L.dropWhileEnd (all isSpace) . dropWhile (all isSpace)

leadingIndent :: String -> Int
leadingIndent = length . takeWhile (`elem` [' ', '\t'])

cargoExp :: Cargo Crate -> ExpQ
cargoExp (Cargo stacks') = [|Cargo (M.fromList $(stackBindingsExp (M.toList stacks')))|]

stackBindingsExp :: [(Int, Stack Crate)] -> ExpQ
stackBindingsExp = listE . fmap stackBindingExp

stackBindingExp :: (Int, Stack Crate) -> ExpQ
stackBindingExp (position, stack') = [|(position, $(stackExp stack'))|]

stackExp :: Stack Crate -> ExpQ
stackExp EmptyStack = [|EmptyStack|]
stackExp (NonEmptyStack crate stack') = [|NonEmptyStack $(crateExp crate) $(stackExp stack')|]

crateExp :: Crate -> ExpQ
crateExp (Crate name) = [|Crate name|]
