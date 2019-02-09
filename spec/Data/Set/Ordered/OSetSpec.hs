{-|
Module      : Data.Set.Ordered.OSetSpec
Description : Tests
Copyright   : (C) Richard Cook, 2019
Licence     : MIT
Maintainer  : rcook@rcook.org
Stability   : stable
Portability : portable
-}

{-# OPTIONS_GHC -Wall -Werror #-}

{-# LANGUAGE ViewPatterns #-}

module Data.Set.Ordered.OSetSpec (spec) where

import           Data.Foldable (toList)
import           Data.Sequence (ViewL(..), ViewR(..))
import qualified Data.Sequence as Seq (fromList, viewl, viewr)
import           Data.Set.Ordered
                    ( (<|)
                    , (|<)
                    , (>|)
                    , (|>)
                    , (<>|)
                    , (|<>)
                    , (\\)
                    , OSet
                    )
import qualified Data.Set.Ordered as OSet
import           Test.Hspec

spec :: Spec
spec = do
    describe "equals" $
        it "compares as expected" $ do
            (OSet.fromListL [4, 3, 4, 1, 9] :: OSet Int)
                `shouldBe` OSet.fromListL [4, 3, 1, 9]
            (OSet.fromListL [4, 3, 4, 1, 9] :: OSet Int)
                `shouldNotBe` OSet.fromListL [3, 4, 1, 9]

    describe "compare" $
        it "compares as expected" $ do
            let a :: OSet Int
                a = OSet.fromListL [4, 3, 4, 1, 9]
                b :: OSet Int
                b = OSet.fromListL [4, 3, 1, 9]
                c :: OSet Int
                c = OSet.fromListL [4, 3, 1]
            a `compare` b `shouldBe` EQ
            a `compare` c `shouldBe` GT
            b `compare` c `shouldBe` GT
            c `compare` a `shouldBe` LT
            c `compare` b `shouldBe` LT

    describe "show" $
        it "shows content in list syntax" $
            show (OSet.fromListL [4, 3, 4, 1, 9] :: OSet Int)
                `shouldBe` "fromList [4,3,1,9]"

    describe "fromListL" $ do
        it "removes duplicates (left-biased)" $
            toList (OSet.fromListL [4, 3, 4, 1, 9] :: OSet Int)
                `shouldBe` [4, 3, 1, 9]
        it "doesn't show up in show output" $ -- The set does not remember how it was constructed
            show (OSet.fromListL [4, 3, 4, 1, 9] :: OSet Int)
                `shouldBe` "fromList [4,3,1,9]"

    describe "fromListR" $ do
        it "removes duplicates (right-biased)" $
            toList (OSet.fromListR [4, 3, 4, 1, 9] :: OSet Int)
                `shouldBe` [3, 4, 1, 9]
        it "doesn't show up in show output" $ -- The set does not remember how it was constructed
            show (OSet.fromListR [4, 3, 4, 1, 9] :: OSet Int)
                `shouldBe` "fromList [3,4,1,9]"

    describe "empty" $
        it "contains no values" $
            toList (OSet.empty :: OSet Int) `shouldBe` []

    describe "member and notMember" $ do
        it "handle element in set" $ do
            1 `OSet.member` (OSet.fromListL [1, 2, 3] :: OSet Int) `shouldBe` True
            1 `OSet.notMember` (OSet.fromListL [1, 2, 3] :: OSet Int) `shouldBe` False
        it "handle element not in set" $ do
            10 `OSet.member` (OSet.fromListL [1, 2, 3] :: OSet Int) `shouldBe` False
            10 `OSet.notMember` (OSet.fromListL [1, 2, 3] :: OSet Int) `shouldBe` True

    describe "elem" $ do
        it "handle element in set" $
            1 `elem` (OSet.fromListL [1, 2, 3] :: OSet Int) `shouldBe` True
        it "handle element not in set" $
            10 `elem` (OSet.fromListL [1, 2, 3] :: OSet Int) `shouldBe` False

    describe "insertion with <|" $ do
        it "conses new element" $
            5 <| (OSet.fromListL [4, 1, 3, 9, 1] :: OSet Int)
                `shouldBe` OSet.fromListL [5, 4, 1, 3, 9]
        it "prefers elements from right" $
            9 <| (OSet.fromListL [4, 1, 3, 9, 1] :: OSet Int)
                `shouldBe` OSet.fromListL [4, 1, 3, 9]

    describe "insertion with |<" $ do
        it "conses new element" $
            5 |< (OSet.fromListL [4, 1, 3, 9, 1] :: OSet Int)
                `shouldBe` OSet.fromListL [5, 4, 1, 3, 9]
        it "prefers elements from left" $
            9 |< (OSet.fromListL [4, 1, 3, 9, 1] :: OSet Int)
                `shouldBe` OSet.fromListL [9, 4, 1, 3]

    describe "insertion with >|" $ do
        it "appends new element" $
            (OSet.fromListL [4, 1, 3, 9, 1] :: OSet Int) >| 5
                `shouldBe` OSet.fromListL [4, 1, 3, 9, 5]
        it "prefers elements from right" $
            (OSet.fromListL [4, 1, 3, 9, 1] :: OSet Int) >| 4
                `shouldBe` OSet.fromListL [1, 3, 9, 4]

    describe "insertion with |>" $ do
        it "appends new element" $
            (OSet.fromListL [4, 1, 3, 9, 1] :: OSet Int) |> 5
                `shouldBe` OSet.fromListL [4, 1, 3, 9, 5]
        it "prefers elements from left" $
            (OSet.fromListL [4, 1, 3, 9, 1] :: OSet Int) |> 4
                `shouldBe` OSet.fromListL [4, 1, 3, 9]

    describe "append with <>|" $ do
        it "appends sets" $
            (OSet.fromListL [4, 1, 3, 9, 1] :: OSet Int)
            <>| OSet.fromListL [5, 5, 6, 6, 7, 7]
                `shouldBe` OSet.fromListL [4, 1, 3, 9, 5, 6, 7]
        it "prefers elements from right" $
            (OSet.fromListL [4, 1, 3, 9, 1] :: OSet Int)
                <>| OSet.fromListL [4, 4, 5, 5, 6, 6, 7, 7]
                `shouldBe` OSet.fromListL [1, 3, 9, 4, 5, 6, 7]

    describe "append with |<>" $ do
        it "appends sets" $
            (OSet.fromListL [4, 1, 3, 9, 1] :: OSet Int)
                |<> OSet.fromListL [5, 5, 6, 6, 7, 7]
                `shouldBe` OSet.fromListL [4, 1, 3, 9, 5, 6, 7]
        it "prefers elements from left" $
            (OSet.fromListL [4, 1, 3, 9, 1] :: OSet Int)
                |<> OSet.fromListL [4, 4, 5, 5, 6, 6, 7, 7]
                `shouldBe` OSet.fromListL [4, 1, 3, 9, 5, 6, 7]

    describe "null" $ do
        it "returns True for empty set" $ do
            null (OSet.fromListL [] :: OSet Int) `shouldBe` True
            null (OSet.empty :: OSet Int) `shouldBe` True
        it "returns False for non-empty set" $
            null (OSet.singleton 1 :: OSet Int) `shouldBe` False

    describe "size" $ do
        it "returns 0 empty set" $ do
            OSet.size (OSet.fromListL [] :: OSet Int) `shouldBe` 0
            OSet.size (OSet.empty :: OSet Int) `shouldBe` 0
        it "returns length for non-empty set" $ do
            OSet.size (OSet.singleton 1 :: OSet Int) `shouldBe` 1
            OSet.size (OSet.fromListL [1, 2, 3, 4, 5] :: OSet Int) `shouldBe` 5
            OSet.size (OSet.fromListL [1, 1, 2, 2, 3, 3] :: OSet Int) `shouldBe` 3

    describe "delete" $ do
        it "deletes element when in set" $
            1 `OSet.delete` (OSet.fromListL [1, 2, 3, 4, 5] :: OSet Int)
                `shouldBe` OSet.fromListL [2, 3, 4, 5]
        it "does not fail when element not in set" $
            6 `OSet.delete` (OSet.fromListL [1, 2, 3, 4, 5] :: OSet Int)
                `shouldBe` OSet.fromListL [1, 2, 3, 4, 5]

    describe "\\\\" $
        it "removes specified elements" $ do
            (OSet.fromListL [1, 2, 3, 4, 5] :: OSet Int)
                \\ OSet.fromListL []
                `shouldBe` OSet.fromListL [1, 2, 3, 4, 5]
            (OSet.fromListL [1, 2, 3, 4, 5] :: OSet Int)
                \\ OSet.fromListL [3, 2, 1]
                `shouldBe` OSet.fromListL [4, 5]

    describe "findIndex" $ do
        it "finds element" $
            1 `OSet.findIndex` (OSet.fromListL [5, 4, 3, 2, 1] :: OSet Int)
                `shouldBe` Just 4
        it "finds nothing" $
            10 `OSet.findIndex` (OSet.fromListL [5, 4, 3, 2, 1] :: OSet Int)
                `shouldBe` Nothing

    describe "elemAt" $ do
        it "returns element" $ do
            (OSet.fromListL [1, 2, 3, 4, 5] :: OSet Int) `OSet.elemAt` 0
                `shouldBe` Just 1
            (OSet.fromListL [1, 2, 3, 4, 5] :: OSet Int) `OSet.elemAt` 1
                `shouldBe` Just 2
            (OSet.fromListL [5, 4, 3, 2, 1] :: OSet Int) `OSet.elemAt` 0
                `shouldBe` Just 5
            (OSet.fromListL [5, 4, 3, 2, 1] :: OSet Int) `OSet.elemAt` 1
                `shouldBe` Just 4
        it "returns nothing if index out of range" $ do
            (OSet.empty :: OSet Int) `OSet.elemAt` 0
                `shouldBe` Nothing
            (OSet.empty :: OSet Int) `OSet.elemAt` (-10)
                `shouldBe` Nothing
            (OSet.empty :: OSet Int) `OSet.elemAt` 10
                `shouldBe` Nothing

    describe "toAscList" $
        it "returns elements in ascending order" $
            OSet.toAscList (OSet.fromListL [5, 4, 3, 2, 1] :: OSet Int)
                `shouldBe` [1, 2, 3, 4, 5]

    describe "singleton" $
        it "contains one element" $ do
            let o :: OSet Int
                o = OSet.singleton 5
            o `shouldBe` OSet.fromListL [5]
            5 `OSet.member` o `shouldBe` True
            5 `OSet.notMember` o `shouldBe` False
            6 `OSet.member` o `shouldBe` False
            6 `OSet.notMember` o `shouldBe` True

    describe "filter" $
        it "removes all even elements" $ do
            let a :: OSet Int
                a = OSet.fromListL [1, 2, 3, 4]
            1 `OSet.member` a `shouldBe` True
            2 `OSet.member` a `shouldBe` True
            3 `OSet.member` a `shouldBe` True
            4 `OSet.member` a `shouldBe` True
            length a `shouldBe` 4
            let b = OSet.filter odd a
            b `shouldBe` OSet.fromListL [1, 3]
            1 `OSet.member` b `shouldBe` True
            2 `OSet.member` b `shouldBe` False
            3 `OSet.member` b `shouldBe` True
            4 `OSet.member` b `shouldBe` False
            length b `shouldBe` 2

    describe "map" $ do
        it "transforms elements" $ do
            let a :: OSet Int
                a = OSet.fromListL [-1, 1, -2, 2]
            (-1) `OSet.member` a `shouldBe` True
            1 `OSet.member` a `shouldBe` True
            (-2) `OSet.member` a `shouldBe` True
            2 `OSet.member` a `shouldBe` True
            length a `shouldBe` 4
            let b = OSet.map (+ 1) a
            b `shouldBe` OSet.fromListL [0, 2, -1, 3]
            0 `OSet.member` b `shouldBe` True
            2 `OSet.member` b `shouldBe` True
            (-1) `OSet.member` b `shouldBe` True
            3 `OSet.member` b `shouldBe` True
            length b `shouldBe` 4
        it "gloriously violates the functor laws" $ do
            let a :: OSet Int
                a = OSet.fromListL [-1, 1, -2, 2]
            (-1) `OSet.member` a `shouldBe` True
            1 `OSet.member` a `shouldBe` True
            (-2) `OSet.member` a `shouldBe` True
            2 `OSet.member` a `shouldBe` True
            length a `shouldBe` 4
            let b = OSet.map (\x -> x * x) a
            b `shouldBe` OSet.fromListL [1, 4]
            1 `OSet.member` b `shouldBe` True
            4 `OSet.member` b `shouldBe` True
            length b `shouldBe` 2

    describe "toSeq" $ do
        let a :: OSet Int
            a = OSet.fromListL [4, 1, 3, 9, 9, 3, 1, 4]
        it "provides Functor instance" $
            show <$> OSet.toSeq a `shouldBe` Seq.fromList ["4", "1", "3", "9"]
        it "provides viewl" $ do
            let fromLeft o = go (OSet.toSeq o)
                    where
                        go (Seq.viewl -> EmptyL) = ""
                        go (Seq.viewl -> head_ :< tail_) = show head_ ++ go tail_
                        go _ = error "Should not happen" -- suppress warning about non-exhaustive patterns
            fromLeft (OSet.empty :: OSet Int) `shouldBe` ""
            fromLeft a `shouldBe` "4139"
        it "provides viewr" $ do
            let fromRight o = go (OSet.toSeq o)
                    where
                        go (Seq.viewr -> EmptyR) = ""
                        go (Seq.viewr -> init_ :> last_) = show last_ ++ go init_
                        go _ = error "Should not happen" -- suppress warning about non-exhaustive patterns
            fromRight (OSet.empty :: OSet Int) `shouldBe` ""
            fromRight a `shouldBe` "9314"
