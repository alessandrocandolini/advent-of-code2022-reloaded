{-# LANGUAGE OverloadedLists #-}

module Data.StackSpec where

import Data.Maybe (isNothing)
import Data.Stack
import GHC.IsList (IsList (toList), fromList)
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

instance (Arbitrary a) => Arbitrary (Stack a) where
  arbitrary = sized $ \n -> do
    k <- chooseInt (0, n)
    build k
   where
    build 0 = pure EmptyStack
    build m = NonEmptyStack <$> arbitrary <*> build (m - 1)

instance Arbitrary StackOrder where
  arbitrary = arbitraryBoundedEnum

spec :: Spec
spec = do
  prop "pop after push gives back the same element on any stack" $ \a s ->
    pop (push s a) `shouldBe` (Just (a, s) :: Maybe (Int, Stack Int))

  prop "pop is empty if and only if the stack is empty" $ \s ->
    isNothing (pop s) `shouldBe` isEmpty (s :: Stack String)

  prop "fromList inserts from right to left; the head of the list is the head of the stack (ie, the first element to be extracted when pop)" $ \a1 a2 a3 ->
    fromList [a1, a2, a3] `shouldBe` (NonEmptyStack a1 (NonEmptyStack a2 (NonEmptyStack a3 EmptyStack)) :: Stack Int)

  prop "toList returns a list whose head is the head of the stack" $ \a1 a2 a3 ->
    toList (NonEmptyStack a1 (NonEmptyStack a2 (NonEmptyStack a3 EmptyStack))) `shouldBe` ([a1, a2, a3] :: [Int])

  prop "cyclic property of toList and fromList" $ \as ->
    toList ((fromList as) :: Stack Int) `shouldBe` as

  prop "pushN inserts from right to left; the head of the list is the head of the stack (ie, the first element to be extracted when pop)" $ \a1 a2 a3 s ->
    pushN s [a1, a2, a3] `shouldBe` (NonEmptyStack a1 (NonEmptyStack a2 (NonEmptyStack a3 s)) :: Stack Int)

  prop "popN returns a list whose head is the head of the stack" $ \a1 a2 a3 s ->
    popN 3 (NonEmptyStack a1 (NonEmptyStack a2 (NonEmptyStack a3 s))) `shouldBe` ([a1, a2, a3] :: [Int], s)

  prop "cyclic property of pushN and popN" $ \as s ->
    popN (length as) (pushN s as) `shouldBe` ((as, s) :: ([Int], Stack Int))

  prop "move and reverse move brings back the original stacks (if the source stack has enough items)" $ \n o s d ->
    (n > 0)
      ==> let
            (s', d') = move o n s d
            (d'', s'') = move o (min n (size s)) d' s'
           in
            (s, d) `shouldBe` ((s'', d'') :: (Stack Int, Stack Int))

  prop "move preserves the total number of elements" $ \o n s d ->
    let
      s', d' :: Stack Int
      (s', d') = move o n s d
     in
      size s + size d `shouldBe` size s' + size d'

  prop "move n elements in LIFO order is the same as moving one element n times" $ \n s d ->
    let
      s', d' :: Stack Int
      (s', d') = move Lifo n s d
      (s'', d'') = foldl' step (s, d) ([1 .. n] :: [Int])
      step (a, b) _ = move Lifo 1 a b
     in
      (s', d') `shouldBe` (s'', d'')

  prop "move n elements in FIFO order moves the top part of the stack from source to destination" $ \n s d ->
    (n > 0)
      ==> let
            n' = min n (size s)
            d' :: Stack Int
            (_, d') = move Fifo n s d
            (as, _) = popN n s
            (as', d'') = popN n' d'
           in
            (as, d) `shouldBe` (as', d'')

  prop "move 1 element is the same in FIFO or LIFO order" $ \s d ->
    move Fifo 1 s d `shouldBe` ((move Lifo 1 s d) :: (Stack Int, Stack Int))

  prop "move with n <= 0 leaves both stacks unchanged" $ \n o s d ->
    (n <= 0) ==> move o n s d `shouldBe` ((s, d) :: (Stack Int, Stack Int))

  prop "moving from an empty source leaves both stacks unchanged" $ \n o s ->
    move o n EmptyStack s `shouldBe` ((EmptyStack, s) :: (Stack Int, Stack Int))

  prop "moving one element does not depend on fifo/lifo" $ \s d ->
    move Fifo 1 s d `shouldBe` ((move Lifo 1 s d) :: (Stack Int, Stack Int))

  it "popN 0 returns no elements and leaves the stack unchanged" $
    popN 0 [1, 2, 3] `shouldBe` (([], [1, 2, 3]) :: ([Int], Stack Int))

  it "popN with a negative count returns no elements and leaves the stack unchanged" $
    popN (-2) [1, 2, 3] `shouldBe` (([], [1, 2, 3]) :: ([Int], Stack Int))

  it "popN larger than the stack size returns all elements and empties the stack" $
    popN 10 [1, 2, 3] `shouldBe` (([1, 2, 3], EmptyStack) :: ([Int], Stack Int))

  it "popN on an empty stack returns an empty result and empty remainder" $
    popN 3 EmptyStack `shouldBe` (([], EmptyStack) :: ([Int], Stack Int))

  it "check list syntax for stacks" $
    ['a', 'b', 'c'] `shouldBe` (NonEmptyStack 'a' (NonEmptyStack 'b' (NonEmptyStack 'c' EmptyStack)))

  it "example of a LIFO move" $
    move Lifo 3 ['d', 'n', 'z'] ['p'] `shouldBe` ([], ['z', 'n', 'd', 'p'])

  it "example of a LIFO move with a number of elements greater than the size of the source" $
    move Lifo 10 ['d', 'n', 'z'] ['p'] `shouldBe` ([], ['z', 'n', 'd', 'p'])

  it "example of a FIFO move" $
    move Fifo 3 ['d', 'n', 'z'] ['p'] `shouldBe` ([], ['d', 'n', 'z', 'p'])

  it "example of peak (nonempty case)" $
    peek ['d', 'n', 'z'] `shouldBe` Just 'd'

  it "example of peak (empty case)" $
    peek ([] :: Stack Int) `shouldBe` Nothing
