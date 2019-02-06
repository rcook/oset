{-|
Module      : Data.Set.Ordered
Description : An insertion-order-preserving set
Copyright   : (C) Richard Cook, 2019
Licence     : MIT
Maintainer  : rcook@rcook.org
Stability   : stable
Portability : portable

This module provides 'OSet', an insertion-order-preserving set, with
type class instances for 'Foldable', 'Semigroup', 'Monoid' and 'Data' as
well as a 'map' function.

This is intended to be API-compatible with <http://hackage.haskell.org/package/ordered-containers-0.1.1/docs/Data-Set-Ordered.html OSet>
in <http://hackage.haskell.org/package/ordered-containers-0.1.1 unordered-containers>
but with a few extra type class instances.

Here's the quick-start guide to using this package:

> module Main (main) where
>
> import           Data.Set.Ordered ((|>), (|<))
> import qualified Data.Set.Ordered as OSet
>
> main :: IO ()
> main = do
>     -- Create from list
>     let s0 = OSet.fromList [1 :: Int, 2, 3, 4, 4, 3, 2, 1, -1, -2, -3]
>     print s0 -- outputs: "fromList [1,2,3,4,-1,-2,-3]"
>
>     -- Append
>     let s1 = s0 |> 4
>     print s1 -- outputs: "fromList [1,2,3,4,-1,-2,-3]"
>
>     -- Prepend
>     let s2 = 4 |< s0
>     print s2 -- outputs: "fromList [4,1,2,3,-1,-2,-3]"
>
>     -- Semigroup
>     let s3 = s0 <> OSet.fromList [10, 10, 20, 20, 30, 30]
>     print s3 -- outputs: "fromList [1,2,3,4,-1,-2,-3,10,20,30]"
>
>     -- Map (but note that OSet is not a functor)
>     let s4 = OSet.map (\x -> x * x) s3
>     print s4 -- outputs: "fromList [1,4,9,16,100,400,900]"
>
>     -- Filter
>     let s5 = OSet.filter (>= 100) s4
>     print s5 -- outputs: "fromList [100,400,900]"

There are cases where the developer's natural instinct would be to
convert the 'OSet' instance to a list using 'toList' from 'Foldable'.
While this is possible, it will often be more efficient to use 'toSeq'
and operate on the sequence that way. You can even use view patterns to
pattern-match on the resulting sequence:

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
>     let a = OSet.fromList [4 :: Int, 1, 3, 9, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9]
>     print $ showFromLeft a -- outputs: "4139025678"
-}

{-# OPTIONS_GHC -Wall -Werror #-}

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Set.Ordered
    ( (|>)
    , (|<)
    , OSet()
    , empty
    , filter
    , fromList
    , map
    , member
    , notMember
    , singleton
    , toSeq
    ) where

import           Data.Data (Data)
import           Data.Foldable (Foldable(..), foldl')
import           Data.Maybe (Maybe(..))
import           Data.Monoid (Monoid(..))
import           Data.Semigroup (Semigroup(..))
import           Data.Sequence (Seq)
import qualified Data.Sequence as Seq
                    ( (|>)
                    , (<|)
                    , deleteAt
                    , elemIndexL
                    , empty
                    , filter
                    , singleton
                    )
import           Data.Set (Set)
import qualified Data.Set as Set
                    ( empty
                    , filter
                    , insert
                    , member
                    , singleton
                    )
import           Prelude ((<$>), (.), Bool, Eq, Ord, Show(..), not, otherwise)

-- | An 'OSet' behaves much like a 'Set' but remembers the order in
-- which the elements were originally inserted.
data OSet a = OSet (Set a) (Seq a) deriving (Data, Eq, Ord)

instance Show a => Show (OSet a) where
    show (OSet _ xsSeq) = show xsSeq

instance Foldable OSet where
    foldMap f (OSet _ xsSeq) = foldMap f xsSeq
    x `elem` (OSet xsSet _) = x `elem` xsSet

instance Ord a => Semigroup (OSet a) where
    (<>) = foldl' (|>)

instance Ord a => Monoid (OSet a) where
    mempty = empty

-- | \(O(log(N))\). Append an element to the end of set if the set does not
-- already contain the element. The element is ignored if it is already in the
-- set.
(|>) :: Ord a
    => OSet a   -- ^ set
    -> a        -- ^ element
    -> OSet a   -- ^ set
o@(OSet xsSet xsSeq) |> x
    | x `member` o = o
    | otherwise = OSet (Set.insert x xsSet) (xsSeq Seq.|> x)
infixl 5 |>

-- | \(O(log(N))\) if the element is not in the set, \(O(N)\) if the element is
-- already in the set. Prepend an element to the head of the set if the set does
-- not already contain the element. The element is moved to the head of the
-- sequence if the element is already present in the set.
(|<) :: Ord a
    => a        -- ^ element
    -> OSet a   -- ^ set
    -> OSet a   -- ^ set
x |< (OSet xsSet xsSeq) =
    if x `Set.member` xsSet
        then
            case Seq.elemIndexL x xsSeq of
                Nothing -> OSet (Set.insert x xsSet) (x Seq.<| xsSeq)
                Just idx -> OSet xsSet (x Seq.<| Seq.deleteAt idx xsSeq)
        else OSet (Set.insert x xsSet) (x Seq.<| xsSeq)
infixr 5 |<

-- | \(O(N log(N))\). Create a set from a finite list of elements. If an element
-- occurs multiple times in the original list, only the first occurrence is
-- retained in the resulting set. The function 'toList', \(O(N)\), in 'Foldable'
-- can be used to return a list of the elements in the original insert order
-- with duplicates removed.
fromList :: Ord a
    => [a]      -- ^ elements
    -> OSet a   -- ^ set
fromList = foldl' (|>) empty

-- | \(O(1)\). The empty set.
empty ::
    OSet a      -- ^ set
empty = OSet Set.empty Seq.empty

-- | \(O(1)\). A singleton set containing the given element.
singleton ::
    a           -- ^ element
    -> OSet a   -- ^ set
singleton x = OSet (Set.singleton x) (Seq.singleton x)

-- | \(O(log(N))\). Determine if the element is in the set.
member :: Ord a
    => a        -- ^ element
    -> OSet a   -- ^ set
    -> Bool     -- ^ 'Data.Bool.True' if element is in set, 'Data.Bool.False' otherwise
member x (OSet xsSet _) = x `Set.member` xsSet

-- | \(O(log(N))\). Determine if the element is not in the set.
notMember :: Ord a
    => a        -- ^ element
    -> OSet a   -- ^ set
    -> Bool     -- ^ 'Data.Bool.True' if element is not in set, 'Data.Bool.False' otherwise
notMember = (not .) . member

-- | \(O(N)\). Filter a set by returning a set whose elements satisfy the
-- predicate.
filter :: (a -> Bool)  -- ^ predicate
    -> OSet a       -- ^ set
    -> OSet a       -- ^ set
filter p (OSet xsSet xsSeq) = OSet (Set.filter p xsSet) (Seq.filter p xsSeq)

-- | \(O(N log(N))\). Return the set obtained by applying a function to each
-- element of this set. Note that the resulting set may be smaller than the
-- original. Along with the 'Ord' constraint, this means that 'OSet' cannot
-- provide a lawful 'Data.Functor.Functor' instance.
map :: Ord b
    => (a -> b)
    -> OSet a
    -> OSet b
map f (OSet _ xsSeq) = foldl' (|>) empty (f <$> xsSeq)

-- | \(O(1)\). Return ordered sequence of elements in set. For obtaining
-- a useful 'Data.Functor.Functor' this is recommended over 'toList' due
-- to its constant-time performance. Similarly, if you want to
-- pattern-match on the 'OSet', obtain the sequence and use view
-- patterns instead of converting to a list.
toSeq :: OSet a -> Seq a
toSeq (OSet _ xsSeq) = xsSeq
