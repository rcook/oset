{-|
Module      : Data.Set.Ordered
Description : An insertion-order-preserving set
Copyright   : (C) Richard Cook, 2019
Licence     : MIT
Maintainer  : rcook@rcook.org
Stability   : stable
Portability : portable

This module provides 'OSet', an insertion-order-preserving set as well
as left- and right-biased wrappers 'OSetL' and 'OSetR' respectively.
'OSet' has instances for 'Data.Foldable.Foldable' and 'Data.Data.Data'
as well as a 'map' function and other features.
'Data.Semigroup.Semigroup' and 'Data.Monoid.Monoid' type class instances
are provided for 'OSetL' and 'OSetR'.

This is intended to be API-compatible with <http://hackage.haskell.org/package/ordered-containers-0.1.1/docs/Data-Set-Ordered.html OSet>
in <http://hackage.haskell.org/package/ordered-containers-0.1.1 unordered-containers>
but with a few extra type class instances.

Here's the quick-start guide to using this package:

> module Main (main) where
>
> import           Data.Set.Ordered ((|>), (|<), (|<>))
> import qualified Data.Set.Ordered as OSet
>
> main :: IO ()
> main = do
>     -- Create from list
>     let s0 = OSet.fromListL [1 :: Int, 2, 3, 4, 4, 3, 2, 1, -1, -2, -3]
>     print s0 -- outputs: "fromListL [1,2,3,4,-1,-2,-3]"
>
>     -- Append
>     let s1 = s0 |> 4
>     print s1 -- outputs: "fromListL [1,2,3,4,-1,-2,-3]"
>
>     -- Prepend
>     let s2 = 4 |< s0
>     print s2 -- outputs: "fromListL [4,1,2,3,-1,-2,-3]"
>
>     -- Append
>     let s3 = s0 |<> OSet.fromListL [10, 10, 20, 20, 30, 30]
>     print s3 -- outputs: "fromListL [1,2,3,4,-1,-2,-3,10,20,30]"
>
>     -- Map (but note that OSet is not a functor)
>     let s4 = OSet.map (\x -> x * x) s3
>     print s4 -- outputs: "fromListL [1,4,9,16,100,400,900]"
>
>     -- Filter
>     let s5 = OSet.filter (>= 100) s4
>     print s5 -- outputs: "fromListL [100,400,900]"

There are cases where the developer's natural instinct would be to
convert the 'OSet' instance to a list using
'Data.Foldable.Foldable.toList'. While this is possible, it will often
be more efficient to use 'toSeq' and operate on the sequence that way.
You can even use view patterns to pattern-match on the resulting
sequence:

> {-# LANGUAGE ViewPatterns #-}
>
> module Main (main) where
>
> import           Data.Sequence (ViewL(..), viewl)
> import           Data.Set.Ordered (OSet)
> import qualified Data.Set.Ordered as OSet
>
> showFromLeft :: Show a => OSet a -> String
> showFromLeft o = go (OSet.toSeq o)
>     where
>         go (viewl -> EmptyL) = ""
>         go (viewl -> h :< t) = show h ++ go t
>         go _ = error "Should not happen" -- suppress warning about non-exhaustive patterns
>
> main :: IO ()
> main = do
>     let a = OSet.fromListL [4 :: Int, 1, 3, 9, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9]
>     print $ showFromLeft a -- outputs: "4139025678"
-}

{-# OPTIONS_GHC -Wall -Werror #-}

{-# LANGUAGE NoImplicitPrelude #-}

module Data.Set.Ordered
    ( OSet
    , OSetL(..)
    , OSetR(..)
    , -- * Trivial sets
      empty
    , singleton
    , -- * Insertion
      (<|)
    , (|<)
    , (>|)
    , (|>)
    , (<>|)
    , (|<>)
    , -- * Query
      member
    , notMember
    , size
    , -- * Deletion
      (\\)
    , delete
    , filter
    , -- * Indexing
      Index
    , elemAt
    , findIndex
    , -- * Conversion
      fromListL
    , fromListR
    , toAscList
    , toSeq
    , -- * Miscellaneous
      map
    ) where

import Data.Set.Ordered.LR
import Data.Set.Ordered.OSet
