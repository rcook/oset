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
            (OSet.fromList [4, 3, 4, 1, 9] :: OSet Int)
                `shouldBe` OSet.fromList [4, 3, 1, 9]
            (OSet.fromList [4, 3, 4, 1, 9] :: OSet Int)
                `shouldNotBe` OSet.fromList [3, 4, 1, 9]

    describe "compare" $
        it "compares as expected" $ do
            let a :: OSet Int
                a = OSet.fromList [4, 3, 4, 1, 9]
                b :: OSet Int
                b = OSet.fromList [4, 3, 1, 9]
                c :: OSet Int
                c = OSet.fromList [4, 3, 1]
            a `compare` b `shouldBe` EQ
            a `compare` c `shouldBe` GT
            b `compare` c `shouldBe` GT
            c `compare` a `shouldBe` LT
            c `compare` b `shouldBe` LT

    describe "show" $
        it "shows content in list syntax" $
            show (OSet.fromList [4, 3, 4, 1, 9] :: OSet Int)
                `shouldBe` "fromList [4,3,1,9]"

    describe "fromList" $
        it "removes duplicates" $
            toList (OSet.fromList [4, 3, 4, 1, 9] :: OSet Int)
                `shouldBe` [4, 3, 1, 9]

    describe "empty" $
        it "contains no values" $
            toList (OSet.empty :: OSet Int) `shouldBe` []

    describe "member and notMember" $ do
        it "handle element in set" $ do
            1 `OSet.member` (OSet.fromList [1, 2, 3] :: OSet Int) `shouldBe` True
            1 `OSet.notMember` (OSet.fromList [1, 2, 3] :: OSet Int) `shouldBe` False
        it "handle element not in set" $ do
            10 `OSet.member` (OSet.fromList [1, 2, 3] :: OSet Int) `shouldBe` False
            10 `OSet.notMember` (OSet.fromList [1, 2, 3] :: OSet Int) `shouldBe` True

    describe "elem" $ do
        it "handle element in set" $
            1 `elem` (OSet.fromList [1, 2, 3] :: OSet Int) `shouldBe` True
        it "handle element not in set" $
            10 `elem` (OSet.fromList [1, 2, 3] :: OSet Int) `shouldBe` False

    describe "insertion with <|" $ do
        it "conses new element" $
            5 <| (OSet.fromList [4, 1, 3, 9, 1] :: OSet Int)
                `shouldBe` OSet.fromList [5, 4, 1, 3, 9]
        it "prefers elements from right" $
            9 <| (OSet.fromList [4, 1, 3, 9, 1] :: OSet Int)
                `shouldBe` OSet.fromList [4, 1, 3, 9]

    describe "insertion with |<" $ do
        it "conses new element" $
            5 |< (OSet.fromList [4, 1, 3, 9, 1] :: OSet Int)
                `shouldBe` OSet.fromList [5, 4, 1, 3, 9]
        it "prefers elements from left" $
            9 |< (OSet.fromList [4, 1, 3, 9, 1] :: OSet Int)
                `shouldBe` OSet.fromList [9, 4, 1, 3]

    describe "insertion with >|" $ do
        it "appends new element" $
            (OSet.fromList [4, 1, 3, 9, 1] :: OSet Int) >| 5
                `shouldBe` OSet.fromList [4, 1, 3, 9, 5]
        it "prefers elements from right" $
            (OSet.fromList [4, 1, 3, 9, 1] :: OSet Int) >| 4
                `shouldBe` OSet.fromList [1, 3, 9, 4]

    describe "insertion with |>" $ do
        it "appends new element" $
            (OSet.fromList [4, 1, 3, 9, 1] :: OSet Int) |> 5
                `shouldBe` OSet.fromList [4, 1, 3, 9, 5]
        it "prefers elements from left" $
            (OSet.fromList [4, 1, 3, 9, 1] :: OSet Int) |> 4
                `shouldBe` OSet.fromList [4, 1, 3, 9]

    describe "append with <>|" $ do
        it "appends sets" $
            (OSet.fromList [4, 1, 3, 9, 1] :: OSet Int)
            <>| OSet.fromList [5, 5, 6, 6, 7, 7]
                `shouldBe` OSet.fromList [4, 1, 3, 9, 5, 6, 7]
        it "prefers elements from right" $
            (OSet.fromList [4, 1, 3, 9, 1] :: OSet Int)
                <>| OSet.fromList [4, 4, 5, 5, 6, 6, 7, 7]
                `shouldBe` OSet.fromList [1, 3, 9, 4, 5, 6, 7]

    describe "append with |<>" $ do
        it "appends sets" $
            (OSet.fromList [4, 1, 3, 9, 1] :: OSet Int)
                |<> OSet.fromList [5, 5, 6, 6, 7, 7]
                `shouldBe` OSet.fromList [4, 1, 3, 9, 5, 6, 7]
        it "prefers elements from left" $
            (OSet.fromList [4, 1, 3, 9, 1] :: OSet Int)
                |<> OSet.fromList [4, 4, 5, 5, 6, 6, 7, 7]
                `shouldBe` OSet.fromList [4, 1, 3, 9, 5, 6, 7]

    describe "null" $ do
        it "returns True for empty set" $ do
            null (OSet.fromList [] :: OSet Int) `shouldBe` True
            null (OSet.empty :: OSet Int) `shouldBe` True
        it "returns False for non-empty set" $
            null (OSet.singleton 1 :: OSet Int) `shouldBe` False

    describe "size" $ do
        it "returns 0 empty set" $ do
            OSet.size (OSet.fromList [] :: OSet Int) `shouldBe` 0
            OSet.size (OSet.empty :: OSet Int) `shouldBe` 0
        it "returns length for non-empty set" $ do
            OSet.size (OSet.singleton 1 :: OSet Int) `shouldBe` 1
            OSet.size (OSet.fromList [1, 2, 3, 4, 5] :: OSet Int) `shouldBe` 5
            OSet.size (OSet.fromList [1, 1, 2, 2, 3, 3] :: OSet Int) `shouldBe` 3

    describe "delete" $ do
        it "deletes element when in set" $
            1 `OSet.delete` (OSet.fromList [1, 2, 3, 4, 5] :: OSet Int)
                `shouldBe` OSet.fromList [2, 3, 4, 5]
        it "does not fail when element not in set" $
            6 `OSet.delete` (OSet.fromList [1, 2, 3, 4, 5] :: OSet Int)
                `shouldBe` OSet.fromList [1, 2, 3, 4, 5]

    describe "\\\\" $
        it "removes specified elements" $ do
            (OSet.fromList [1, 2, 3, 4, 5] :: OSet Int)
                \\ OSet.fromList []
                `shouldBe` OSet.fromList [1, 2, 3, 4, 5]
            (OSet.fromList [1, 2, 3, 4, 5] :: OSet Int)
                \\ OSet.fromList [3, 2, 1]
                `shouldBe` OSet.fromList [4, 5]

    describe "findIndex" $ do
        it "finds element" $
            (1 :: Int) `OSet.findIndex` OSet.fromList [5, 4, 3, 2, 1]
                `shouldBe` Just 4
        it "finds nothing" $
            (10 :: Int) `OSet.findIndex` OSet.fromList [5, 4, 3, 2, 1]
                `shouldBe` Nothing

    describe "elemAt" $ do
        it "returns element" $ do
            OSet.fromList [1, 2, 3, 4, 5] `OSet.elemAt` 0
                `shouldBe` (Just 1 :: Maybe Int)
            OSet.fromList [1, 2, 3, 4, 5] `OSet.elemAt` 1
                `shouldBe` (Just 2 :: Maybe Int)
            OSet.fromList [5, 4, 3, 2, 1] `OSet.elemAt` 0
                `shouldBe` (Just 5 :: Maybe Int)
            OSet.fromList [5, 4, 3, 2, 1] `OSet.elemAt` 1
                `shouldBe` (Just 4 :: Maybe Int)
        it "returns nothing if index out of range" $ do
            OSet.empty `OSet.elemAt` 0
                `shouldBe` (Nothing :: Maybe Int)
            OSet.empty `OSet.elemAt` (-10)
                `shouldBe` (Nothing :: Maybe Int)
            OSet.empty `OSet.elemAt` 10
                `shouldBe` (Nothing :: Maybe Int)

    describe "toAscList" $
        it "returns elements in ascending order" $
            OSet.toAscList (OSet.fromList [5, 4, 3, 2, 1] :: OSet Int)
                `shouldBe` [1, 2, 3, 4, 5]

    describe "singleton" $
        it "contains one element" $ do
            let o :: OSet Int
                o = OSet.singleton 5
            o `shouldBe` OSet.fromList [5]
            5 `OSet.member` o `shouldBe` True
            5 `OSet.notMember` o `shouldBe` False
            6 `OSet.member` o `shouldBe` False
            6 `OSet.notMember` o `shouldBe` True

    describe "filter" $
        it "removes all even elements" $ do
            let a :: OSet Int
                a = OSet.fromList [1, 2, 3, 4]
            1 `OSet.member` a `shouldBe` True
            2 `OSet.member` a `shouldBe` True
            3 `OSet.member` a `shouldBe` True
            4 `OSet.member` a `shouldBe` True
            length a `shouldBe` 4
            let b = OSet.filter odd a
            b `shouldBe` OSet.fromList [1, 3]
            1 `OSet.member` b `shouldBe` True
            2 `OSet.member` b `shouldBe` False
            3 `OSet.member` b `shouldBe` True
            4 `OSet.member` b `shouldBe` False
            length b `shouldBe` 2

    describe "map" $ do
        it "transforms elements" $ do
            let a :: OSet Int
                a = OSet.fromList [-1, 1, -2, 2]
            (-1) `OSet.member` a `shouldBe` True
            1 `OSet.member` a `shouldBe` True
            (-2) `OSet.member` a `shouldBe` True
            2 `OSet.member` a `shouldBe` True
            length a `shouldBe` 4
            let b = OSet.map (+ 1) a
            b `shouldBe` OSet.fromList [0, 2, -1, 3]
            0 `OSet.member` b `shouldBe` True
            2 `OSet.member` b `shouldBe` True
            (-1) `OSet.member` b `shouldBe` True
            3 `OSet.member` b `shouldBe` True
            length b `shouldBe` 4
        it "gloriously violates the functor laws" $ do
            let a :: OSet Int
                a = OSet.fromList [-1, 1, -2, 2]
            (-1) `OSet.member` a `shouldBe` True
            1 `OSet.member` a `shouldBe` True
            (-2) `OSet.member` a `shouldBe` True
            2 `OSet.member` a `shouldBe` True
            length a `shouldBe` 4
            let b = OSet.map (\x -> x * x) a
            b `shouldBe` OSet.fromList [1, 4]
            1 `OSet.member` b `shouldBe` True
            4 `OSet.member` b `shouldBe` True
            length b `shouldBe` 2

    describe "toSeq" $ do
        let a = OSet.fromList [4 :: Int, 1, 3, 9, 9, 3, 1, 4]
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
