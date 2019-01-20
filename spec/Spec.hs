module Main (main) where

import           Data.Foldable (toList)
import           Data.Set.Ordered ((|>), (|<), OSet)
import qualified Data.Set.Ordered as OSet
import           Test.Hspec

main :: IO ()
main = hspec $ do
    describe "equals" $
        it "compares as expected" $ do
            OSet.fromList [4, 3, 4, 1, 9] `shouldBe` OSet.fromList [4, 3, 1, 9]
            OSet.fromList [4, 3, 4, 1, 9] `shouldNotBe` OSet.fromList [3, 4, 1, 9]

    describe "compare" $
        it "compares as expected" $ do
            let a = OSet.fromList [4, 3, 4, 1, 9]
                b = OSet.fromList [4, 3, 1, 9]
                c = OSet.fromList [4, 3, 1]
            a `compare` b `shouldBe` EQ
            a `compare` c `shouldBe` GT
            b `compare` c `shouldBe` GT
            c `compare` a `shouldBe` LT
            c `compare` b `shouldBe` LT

    describe "show" $
        it "shows content in list syntax" $
            show (OSet.fromList [4, 3, 4, 1, 9]) `shouldBe` "fromList [4,3,1,9]"

    describe "fromList" $
        it "removes duplicates" $
            toList (OSet.fromList [4, 3, 4, 1, 9]) `shouldBe` [4, 3, 1, 9]

    describe "empty" $
        it "contains no values" $
            toList OSet.empty `shouldBe` ([] :: [Int])

    describe "mappend" $ do
        it "removes duplicates" $
            toList (OSet.fromList [4, 3, 4, 1, 9] <> OSet.fromList [9, 8..0])
                `shouldBe` [4, 3, 1, 9, 8, 7, 6, 5, 2, 0]
        it "is associative" $ do
            let a = OSet.fromList [4, 3, 4, 1, 9]
                b = OSet.fromList [9, 8..0]
                c = OSet.fromList [-1, 10]
                result = OSet.fromList [4, 3, 1, 9, 8, 7, 6, 5, 2, 0, -1, 10]
            a <> b <> c `shouldBe` result
            (a <> b) <> c `shouldBe` result
            a <> (b <> c) `shouldBe` result

    describe "mempty" $ do
        it "contains no values" $
            toList (mempty :: OSet Int) `shouldBe` ([] :: [Int])
        it "is neutral element" $ do
            let a = OSet.fromList [4, 3, 4, 1, 9]
            mempty <> a `shouldBe` a
            a <> mempty `shouldBe` a

    describe "member and notMember" $ do
        it "handle element in set" $ do
            1 `OSet.member` OSet.fromList [1, 2, 3] `shouldBe` True
            1 `OSet.notMember` OSet.fromList [1, 2, 3] `shouldBe` False
        it "handle element not in set" $ do
            10 `OSet.member` OSet.fromList [1, 2, 3] `shouldBe` False
            10 `OSet.notMember` OSet.fromList [1, 2, 3] `shouldBe` True

    describe "elem" $ do
        it "handle element in set" $
            1 `elem` OSet.fromList [1, 2, 3] `shouldBe` True
        it "handle element not in set" $
            10 `elem` OSet.fromList [1, 2, 3] `shouldBe` False

    describe "insertion with <|" $ do
        it "appends new element" $
            OSet.fromList  [4, 1, 3, 9, 1] |> 5 `shouldBe` OSet.fromList [4, 1, 3, 9, 5]
        it "prefers matching element already in set" $
            OSet.fromList  [4, 1, 3, 9, 1] |> 4 `shouldBe` OSet.fromList [4, 1, 3, 9]

    describe "insertion with |<" $ do
        it "conses new element" $
            5 |< OSet.fromList  [4, 1, 3, 9, 1] `shouldBe` OSet.fromList [5, 4, 1, 3, 9]
        it "prefers matching element already in set" $
            9 |< OSet.fromList  [4, 1, 3, 9, 1] `shouldBe` OSet.fromList [9, 4, 1, 3]

    describe "singleton" $
        it "contains one element" $ do
            let o = OSet.singleton 5
            o `shouldBe` OSet.fromList [5]
            5 `OSet.member` o `shouldBe` True
            5 `OSet.notMember` o `shouldBe` False
            6 `OSet.member` o `shouldBe` False
            6 `OSet.notMember` o `shouldBe` True

    describe "filter" $
        it "removes all even elements" $ do
            let a = OSet.fromList [1, 2, 3, 4]
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
            let a = OSet.fromList [-1, 1, -2, 2]
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
            let a = OSet.fromList [-1, 1, -2, 2]
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
