{-# LANGUAGE TypeFamilies #-}

module Data.Stack where

import Data.List (unfoldr)
import GHC.Exts (IsList (fromList, toList), Item)

data Stack a = EmptyStack | NonEmptyStack a (Stack a) deriving (Eq, Show)

isEmpty :: Stack a -> Bool
isEmpty EmptyStack = True
isEmpty (NonEmptyStack _ _) = False

size :: Stack a -> Int
size EmptyStack = 0
size (NonEmptyStack _ stack) = 1 + size stack

push :: Stack a -> a -> Stack a
push EmptyStack a = NonEmptyStack a EmptyStack
push stack@(NonEmptyStack _ _) a = NonEmptyStack a stack

pop :: Stack a -> Maybe (a, Stack a)
pop EmptyStack = Nothing
pop (NonEmptyStack a stack) = Just (a, stack)

peek :: Stack a -> Maybe a
peek = fmap fst . pop

pushN :: Stack a -> [a] -> Stack a
pushN = foldr (flip push)

pushNRev :: Stack a -> [a] -> Stack a
pushNRev = foldl' push

popN :: Int -> Stack a -> ([a], Stack a)
popN n stack = fmap snd $ unfoldAcc step (n, stack)
 where
  step (k, _) | k <= 0 = Nothing
  step (_, EmptyStack) = Nothing
  step (k, s) = do
    (a, s') <- pop s
    pure (a, (k - 1, s'))

unfoldAcc :: (b -> Maybe (a, b)) -> b -> ([a], b)
unfoldAcc step seed =
  case step seed of
    Nothing -> ([], seed)
    Just (a, seed') -> (a : as, seed'')
     where
      (as, seed'') = unfoldAcc step seed'

data StackOrder = Lifo | Fifo deriving (Eq, Show, Enum, Bounded)

move :: StackOrder -> Int -> Stack a -> Stack a -> (Stack a, Stack a)
move o n source destination =
  let
    (as, source') = popN n source
    destination' = case o of
      Lifo -> pushNRev destination as
      Fifo -> pushN destination as
   in
    (source', destination')

instance IsList (Stack a) where
  type Item (Stack a) = a
  fromList = pushN EmptyStack
  toList = unfoldr pop
