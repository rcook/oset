module Main (main) where

import           Data.Foldable (toList)
import           Data.Set.Ordered (OSet)
import qualified Data.Set.Ordered as OSet
import           Test.Hspec

main :: IO ()
main = hspec $ do
    describe "equals" $
        it "compares as expected" $ do
            OSet.fromList [4, 3, 4, 1, 9] `shouldBe` OSet.fromList [4, 3, 1, 9]
            OSet.fromList [4, 3, 4, 1, 9] `shouldNotBe` OSet.fromList [3, 4, 1, 9]

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
