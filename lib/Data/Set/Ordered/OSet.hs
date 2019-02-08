{-|
Module      : Data.Set.Ordered.OSet
Description : An insertion-order-preserving set
Copyright   : (C) Richard Cook, 2019
Licence     : MIT
Maintainer  : rcook@rcook.org
Stability   : stable
Portability : portable
-}

{-# OPTIONS_GHC -Wall -Werror #-}

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Set.Ordered.OSet
    ( OSet
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
      fromList
    , toAscList
    , toSeq
    , -- * Miscellaneous
      map
    ) where

import           Data.Data (Data)
import           Data.Foldable (Foldable(..), foldl')
import           Data.Maybe (Maybe(..))
import           Data.Set.Ordered.Classes
                    ( PreserveL(..)
                    , PreserveR(..)
                    , Values(..)
                    )
import           Data.Sequence (Seq)
import qualified Data.Sequence as Seq
                    ( (|>)
                    , (<|)
                    , deleteAt
                    , elemIndexL
                    , empty
                    , filter
                    , findIndexL
                    , lookup
                    , singleton
                    )
import           Data.Set (Set)
import qualified Data.Set as Set
                    ( delete
                    , empty
                    , filter
                    , insert
                    , member
                    , singleton
                    , size
                    , toAscList
                    )
import           Prelude
                    ( (<$>)
                    , (.)
                    , (==)
                    , Eq
                    , Int
                    , Ord
                    , Show(..)
                    , not
                    , otherwise
                    )

-- | A zero-based index with respect to insertion order.
type Index = Int

-- | An 'OSet' behaves much like a 'Set' but remembers the order in
-- which the elements were originally inserted.
data OSet a = OSet (Set a) (Seq a) deriving (Data, Eq, Ord)

instance Show a => Show (OSet a) where
    show (OSet _ xsSeq) = show xsSeq

instance Foldable OSet where
    foldMap f (OSet _ xsSeq) = foldMap f xsSeq
    x `elem` (OSet xsSet _) = x `elem` xsSet

instance Values a OSet where
    empty = OSet Set.empty Seq.empty
    singleton x = OSet (Set.singleton x) (Seq.singleton x)
    fromList = foldl' (|>) empty
    member x (OSet xsSet _) = x `Set.member` xsSet
    notMember = (not .) . member
    map f (OSet _ xsSeq) = foldl' (|>) empty (f <$> xsSeq)
    filter p (OSet xsSet xsSeq) = OSet (Set.filter p xsSet) (Seq.filter p xsSeq)

instance Ord a => PreserveL a OSet where
    x |< (OSet xsSet xsSeq) =
        if x `Set.member` xsSet
            then
                let Just idx = Seq.elemIndexL x xsSeq
                in OSet xsSet (x Seq.<| Seq.deleteAt idx xsSeq)
            else OSet (Set.insert x xsSet) (x Seq.<| xsSeq)
    o@(OSet xsSet xsSeq) |> x
        | x `member` o = o
        | otherwise = OSet (Set.insert x xsSet) (xsSeq Seq.|> x)
    (|<>) = foldl' (|>)

instance Ord a => PreserveR a OSet where
    x <| o@(OSet xsSet xsSeq) =
        if x `Set.member` xsSet
            then o
            else OSet (Set.insert x xsSet) (x Seq.<| xsSeq)
    (OSet xsSet xsSeq) >| x =
        if x `Set.member` xsSet
            then
                let Just idx = Seq.elemIndexL x xsSeq
                in OSet xsSet (Seq.deleteAt idx xsSeq Seq.|> x)
            else OSet (Set.insert x xsSet) (xsSeq Seq.|> x)
    (<>|) = foldl' (>|)

-- | \(O(1)\). The number of elements in the set.
size ::
    OSet a  -- ^ set
    -> Int  -- ^ size
size (OSet xsSet _ ) = Set.size xsSet

-- | \(O(1)\). Return ordered sequence of elements in set. For obtaining
-- a useful 'Data.Functor.Functor' instance this is recommended over
-- 'Data.Foldable.Foldable.toList' due to its \(O(1)\) performance.
-- Similarly, if you want to pattern-match on the 'OSet', obtain the
-- sequence and use view patterns or pattern synonyms instead of
-- converting to a list.
toSeq ::
    OSet a      -- ^ set
    -> Seq a    -- ^ sequence
toSeq (OSet _ xsSeq) = xsSeq

-- | \(O(N)\). Convert the set to an ascending list of elements.
toAscList ::
    OSet a  -- ^ set
    -> [a]  -- ^ list
toAscList (OSet xsSet _) = Set.toAscList xsSet

-- | \(O(N)\). Finds the index of the leftmost element that matches the
-- specified element or returns 'Nothing' if no matching element can be found.
findIndex :: Eq a =>
    a               -- ^ element
    -> OSet a       -- ^ set
    -> Maybe Index  -- ^ index
findIndex x (OSet _ xsSeq) = Seq.findIndexL (x ==) xsSeq

-- | \(O(log(min(i, N - i)))\). Return the element at the specified position,
-- \(i\), counting from 0. If the specified position is out of range, this
-- function returns 'Nothing'.
elemAt ::
    OSet a      -- ^ set
    -> Index    -- ^ index
    -> Maybe a  -- ^ element
elemAt (OSet _ xsSeq) idx = Seq.lookup idx xsSeq

-- | \(O(log N)\). Delete an element from the set.
delete :: Ord a =>
    a           -- ^ element
    -> OSet a   -- ^ set
    -> OSet a   -- ^ set
delete x o@(OSet xsSet xsSeq) =
    case Seq.elemIndexL x xsSeq of
        Nothing -> o
        Just idx -> OSet (Set.delete x xsSet) (Seq.deleteAt idx xsSeq)

-- | \(O(N M)\). Find the set difference: @r \\\\ s@ removes all @M@ values in
-- @s@ from @r@ with @N@ values.
(\\) :: Ord a =>
    OSet a      -- ^ set
    -> OSet a   -- ^ set
    -> OSet a   -- ^ set
o0 \\ o1 = filter (`notMember` o1) o0
