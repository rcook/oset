{-|
Module      : Data.Set.Ordered.LRSpec
Description : Tests
Copyright   : (C) Richard Cook, 2019
Licence     : MIT
Maintainer  : rcook@rcook.org
Stability   : stable
Portability : portable
-}

{-# OPTIONS_GHC -Wall -Werror #-}

{-# LANGUAGE CPP #-}

#undef SEMIGROUP_MONOID_UNIFICATION
#ifdef MIN_VERSION_GLASGOW_HASKELL
#if MIN_VERSION_GLASGOW_HASKELL(7,10,2,0)
#if MIN_VERSION_base(4,11,0)
#define SEMIGROUP_MONOID_UNIFICATION
#endif
#endif
#endif

module Data.Set.Ordered.LRSpec (spec) where

import           Data.Foldable (Foldable(..))
#ifndef SEMIGROUP_MONOID_UNIFICATION
import           Data.Semigroup (Semigroup(..))
#endif
import           Data.Set.Ordered
                    ( (|<>)
                    , (<>|)
                    , OSetL(..)
                    , OSetR(..)
                    , empty
                    , singleton
                    )
import qualified Data.Set.Ordered as OSet (fromList)
import           Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
    describe "OSetL" $
        it "preserves elements from the left-hand operand" $
            toList
                ( (mempty :: OSetL Int)
                <> singleton 5
                <> singleton 3
                <> singleton 2
                <> singleton 5
                )
                `shouldBe` [5, 3, 2]
    describe "empty" $
        it "contains no values" $
            null (empty :: OSetL Int) `shouldBe` True
    describe "mempty" $ do
        it "contains no values" $
            toList (mempty :: OSetL Int) `shouldBe` ([] :: [Int])
        it "is neutral element" $ do
            let a = OSetL $ OSet.fromList [4 :: Int, 3, 4, 1, 9]
            (mempty :: OSetL Int) <> a `shouldBe` a
            a <> (mempty :: OSetL Int) `shouldBe` a
    describe "|<>" $ do
        it "removes duplicates" $
            toList ((OSet.fromList [4, 3, 4, 1, 9] :: OSetL Int)
                |<> OSet.fromList [9, 8..0])
                `shouldBe` [4, 3, 1, 9, 8, 7, 6, 5, 2, 0]
        it "is associative" $ do
            let a :: OSetL Int
                a = OSet.fromList [4, 3, 4, 1, 9]
                b = OSet.fromList [9, 8..0]
                c = OSet.fromList [-1, 10]
                result = OSet.fromList [4, 3, 1, 9, 8, 7, 6, 5, 2, 0, -1, 10]
            a |<> b |<> c `shouldBe` result
            (a |<> b) |<> c `shouldBe` result
            a |<> (b |<> c) `shouldBe` result
    describe "<>" $ do
        it "removes duplicates" $
            toList ((OSet.fromList [4, 3, 4, 1, 9] :: OSetL Int)
                <> OSet.fromList [9, 8..0])
                `shouldBe` [4, 3, 1, 9, 8, 7, 6, 5, 2, 0]
        it "is associative" $ do
            let a :: OSetL Int
                a = OSet.fromList [4 :: Int, 3, 4, 1, 9]
                b = OSet.fromList [9, 8..0]
                c = OSet.fromList [-1, 10]
                result = OSet.fromList [4, 3, 1, 9, 8, 7, 6, 5, 2, 0, -1, 10]
            a <> b <> c `shouldBe` result
            (a <> b) <> c `shouldBe` result
            a <> (b <> c) `shouldBe` result

    describe "OSetR" $
        it "preserves elements from the right-hand operand" $
            toList
                ( (mempty :: OSetR Int)
                <> singleton 5
                <> singleton 3
                <> singleton 2
                <> singleton 5
                )
                `shouldBe` [3, 2, 5]
    describe "empty" $
        it "contains no values" $
            null (empty :: OSetR Int) `shouldBe` True
    describe "mempty" $ do
        it "contains no values" $
            toList (mempty :: OSetR Int) `shouldBe` ([] :: [Int])
        it "is neutral element" $ do
            let a = OSetR $ OSet.fromList [4 :: Int, 3, 4, 1, 9]
            (mempty :: OSetR Int) <> a `shouldBe` a
            a <> (mempty :: OSetR Int) `shouldBe` a
    describe "<>|" $ do
        it "removes duplicates" $
            toList ((OSet.fromList [4, 3, 4, 1, 9] :: OSetR Int)
                <>| OSet.fromList [9, 8..0])
                `shouldBe` [9, 8, 7, 6, 5, 4, 3, 2, 1, 0]
        it "is associative" $ do
            let a :: OSetR Int
                a = OSet.fromList [4, 3, 4, 1, 9]
                b = OSet.fromList [9, 8..0]
                c = OSet.fromList [-1, 10]
                result = OSet.fromList [9, 8, 7, 6, 5, 4, 3, 2, 1, 0, -1, 10]
            a <>| b <>| c `shouldBe` result
            (a <>| b) <>| c `shouldBe` result
            a <>| (b <>| c) `shouldBe` result
    describe "<>" $ do
        it "removes duplicates" $
            toList ((OSet.fromList [4, 3, 4, 1, 9] :: OSetR Int)
                <> OSet.fromList [9, 8..0])
                `shouldBe` [9, 8, 7, 6, 5, 4, 3, 2, 1, 0]
        it "is associative" $ do
            let a :: OSetR Int
                a = OSet.fromList [4 :: Int, 3, 4, 1, 9]
                b = OSet.fromList [9, 8..0]
                c = OSet.fromList [-1, 10]
                result = OSet.fromList [9, 8, 7, 6, 5, 4, 3, 2, 1, 0, -1, 10]
            a <> b <> c `shouldBe` result
            (a <> b) <> c `shouldBe` result
            a <> (b <> c) `shouldBe` result
