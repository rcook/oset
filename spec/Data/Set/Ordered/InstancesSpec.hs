{-|
Module      : Data.Set.Ordered.InstancesSpec
Description : Tests
Copyright   : (C) Richard Cook, 2019
Licence     : MIT
Maintainer  : rcook@rcook.org
Stability   : stable
Portability : portable
-}

{-# OPTIONS_GHC -Wall -Werror #-}

module Data.Set.Ordered.InstancesSpec (spec) where

import           Data.Foldable (Foldable(..))
import           Data.Set.Ordered.Instances
                    ( OSetL
                    , OSetR
                    , singletonL
                    , singletonR
                    )
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
