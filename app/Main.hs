{-|
Module      : Main
Description : Demo app
Copyright   : (C) Richard Cook, 2019
Licence     : MIT
Maintainer  : rcook@rcook.org
Stability   : stable
Portability : portable
-}

{-# OPTIONS_GHC -Wall -Werror #-}

{-# LANGUAGE PatternSynonyms #-}

module Main (main) where

import           Data.Set.Ordered ((|>), (|<), (|<>))
import qualified Data.Set.Ordered as OSet
import           Data.Sequence (Seq(..))

main :: IO ()
main = do
    -- Create from list
    let s0 = OSet.fromList [1 :: Int, 2, 3, 4, 4, 3, 2, 1, -1, -2, -3]
    print s0 -- outputs: "fromList [1,2,3,4,-1,-2,-3]"

    -- Append
    let s1 = s0 |> 4
    print s1 -- outputs: "fromList [1,2,3,4,-1,-2,-3]"

    -- Prepend
    let s2 = 4 |< s0
    print s2 -- outputs: "fromList [4,1,2,3,-1,-2,-3]"

    -- Append
    let s3 = s0 |<> OSet.fromList [10, 10, 20, 20, 30, 30]
    print s3 -- outputs: "fromList [1,2,3,4,-1,-2,-3,10,20,30]"

    -- Map (but note that OSet is not a functor)
    let s4 = OSet.map (\x -> x * x) s3
    print s4 -- outputs: "fromList [1,4,9,16,100,400,900]"

    -- Filter
    let s5 = OSet.filter (>= 100) s4
    print s5 -- outputs: "fromList [100,400,900]"

    -- Pattern matching
    print $ foldWithPatternSynonyms (OSet.toSeq s5)

foldWithPatternSynonyms :: Show a => Seq a -> String
foldWithPatternSynonyms Empty = ""
foldWithPatternSynonyms (x :<| xs) = show x ++ foldWithPatternSynonyms xs
