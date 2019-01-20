{-|
Module      : Data.Set.Ordered
Description : Provides an insert-order-preserving set
Copyright   : (C) Richard Cook, 2019
Licence     : MIT
Maintainer  : rcook@rcook.org
Stability   : stable
Portability : portable

This module provides @OSet@, an insert-order-preserving set, with type class
instances for @Foldable@, @Semigroup@, @Monoid@ and @Data@.

This is intended to be API-compatible with <http://hackage.haskell.org/package/ordered-containers-0.1.1/docs/Data-Set-Ordered.html
OSet> in <http://hackage.haskell.org/package/ordered-containers-0.1.1 unordered containers>
but with a few extra type class instances.
-}

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wall #-}

module Data.Set.Ordered
    ( (|>)
    , OSet()
    , empty
    , fromList
    , member
    , notMember
    ) where

import           Data.Data (Data)
import           Data.Foldable (Foldable(..), foldl')
import           Data.Monoid (Monoid(..))
import           Data.Semigroup (Semigroup(..))
import           Data.Sequence (Seq)
import qualified Data.Sequence as Seq ((|>), empty)
import           Data.Set (Set)
import qualified Data.Set as Set (empty, insert, member)
import           Prelude ((.), Bool, Eq, Ord, Show(..), not, otherwise)

-- | An @OSet@ behaves much like a @Set@ but remembers the order in which the
-- elements were originally inserted.
data OSet a = OSet (Set a) (Seq a) deriving (Data, Eq)

instance Show a => Show (OSet a) where
    show (OSet _ xsSeq) = show xsSeq

instance Foldable OSet where
    foldMap f (OSet _ xsSeq) = foldMap f xsSeq
    x `elem` (OSet xsSet _) = x `elem` xsSet

instance Ord a => Semigroup (OSet a) where
    (<>) = foldl' (|>)

instance Ord a => Monoid (OSet a) where
    mempty = empty

-- | \(O(log(N))\). Append an element to the set if the set does not already
-- contain the element.
(|>) :: Ord a
    => OSet a   -- ^ set
    -> a        -- ^ element
    -> OSet a   -- ^ set
o@(OSet xsSet xsSeq) |> x
    | x `member` o = o
    | otherwise = OSet (Set.insert x xsSet) (xsSeq Seq.|> x)
infixl 5 |>

-- | \(O(N log(N))\). Create a set from a finite list of elements. If an element
-- occurs multiple times in the original list, only the first occurrence is
-- retained in the resulting set. The function 'toList' (\(O(N)\) in 'Foldable'
-- can be used to return a list of the elements in the original insert order
-- with duplicates removed.
fromList :: Ord a
    => [a]      -- ^ elements
    -> OSet a   -- ^ set
fromList = foldl' (|>) empty

-- | \(O(1)\). The empty set.
empty :: OSet a
empty = OSet Set.empty Seq.empty

-- | \(O(log(N))\). Determine if the element is in the set.
member :: Ord a
    => a        -- ^ element
    -> OSet a   -- ^ set
    -> Bool     -- ^ @True@ if element is in set, @False@ otherwise
member x (OSet xsSet _) = x `Set.member` xsSet

-- | \(O(log(N))\). Determine if the element is not in the set.
notMember :: Ord a
    => a        -- ^ element
    -> OSet a   -- ^ set
    -> Bool     -- ^ @True@ if element is not in set, @False@ otherwise
notMember = (not .) . member
