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

module Data.Set.Ordered.LRSpec (spec) where

import           Data.Foldable (Foldable(..))
import           Data.Set.Ordered
                    ( OSetL(..)
                    , OSetR(..)
                    , emptyL
                    , emptyR
                    , singletonL
                    , singletonR
                    )
import qualified Data.Set.Ordered as OSet (fromList)
import           Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
    describe "OSetL" $
        it "preserves elements from the left-hand operand" $
            toList
                ( (mempty :: OSetL Int)
                <> singletonL 5
                <> singletonL 3
                <> singletonL 2
                <> singletonL 5
                )
                `shouldBe` [5, 3, 2]
    describe "emptyL" $
        it "contains no values" $
            null emptyL `shouldBe` True
    describe "OSetL.mempty" $ do
        it "contains no values" $
            toList (mempty :: OSetL Int) `shouldBe` ([] :: [Int])
        it "is neutral element" $ do
            let a = OSetL $ OSet.fromList [4 :: Int, 3, 4, 1, 9]
            (mempty :: OSetL Int) <> a `shouldBe` a
            a <> (mempty :: OSetL Int) `shouldBe` a
    describe "OSetL.<>" $ do
        it "removes duplicates" $
            toList (OSetL (OSet.fromList [4 :: Int, 3, 4, 1, 9])
                <> OSetL (OSet.fromList [9, 8..0]))
                `shouldBe` [4, 3, 1, 9, 8, 7, 6, 5, 2, 0]
        it "is associative" $ do
            let a = OSetL $ OSet.fromList [4 :: Int, 3, 4, 1, 9]
                b = OSetL $ OSet.fromList [9, 8..0]
                c = OSetL $ OSet.fromList [-1, 10]
                result = OSetL $ OSet.fromList [4, 3, 1, 9, 8, 7, 6, 5, 2, 0, -1, 10]
            a <> b <> c `shouldBe` result
            (a <> b) <> c `shouldBe` result
            a <> (b <> c) `shouldBe` result

    describe "OSetR" $
        it "preserves elements from the right-hand operand" $
            toList
                ( (mempty :: OSetR Int)
                <> singletonR 5
                <> singletonR 3
                <> singletonR 2
                <> singletonR 5
                )
                `shouldBe` [3, 2, 5]
    describe "emptyR" $
        it "contains no values" $
            null emptyR `shouldBe` True
    describe "OSetR.mempty" $ do
        it "contains no values" $
            toList (mempty :: OSetR Int) `shouldBe` ([] :: [Int])
        it "is neutral element" $ do
            let a = OSetR $ OSet.fromList [4 :: Int, 3, 4, 1, 9]
            (mempty :: OSetR Int) <> a `shouldBe` a
            a <> (mempty :: OSetR Int) `shouldBe` a
    describe "OSetR.<>" $ do
        it "removes duplicates" $
            toList (OSetR (OSet.fromList [4 :: Int, 3, 4, 1, 9])
                <> OSetR (OSet.fromList [9, 8..0]))
                `shouldBe` [9, 8, 7, 6, 5, 4, 3, 2, 1, 0]
        it "is associative" $ do
            let a = OSetR $ OSet.fromList [4 :: Int, 3, 4, 1, 9]
                b = OSetR $ OSet.fromList [9, 8..0]
                c = OSetR $ OSet.fromList [-1, 10]
                result = OSetR $ OSet.fromList [9, 8, 7, 6, 5, 4, 3, 2, 1, 0, -1, 10]
            a <> b <> c `shouldBe` result
            (a <> b) <> c `shouldBe` result
            a <> (b <> c) `shouldBe` result
