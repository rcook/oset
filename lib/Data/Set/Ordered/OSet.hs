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
      fromListL
    , fromListR
    , toAscList
    , toSeq
    , -- * Miscellaneous
      map
    ) where

import           Data.Data (Data)
import           Data.Foldable (Foldable(..), foldl')
import           Data.Maybe (Maybe(..))
import           Data.Set.Ordered.Classes
                    ( Index
                    , OrderedSet(..)
                    , PreserveL(..)
                    , PreserveR(..)
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
                    , Ord
                    , Show(..)
                    , not
                    , otherwise
                    )

-- | An 'OSet' behaves much like a 'Set' but remembers the order in
-- which the elements were originally inserted.
data OSet a = OSet (Set a) (Seq a) deriving (Data, Eq, Ord)

instance Show a => Show (OSet a) where
    show (OSet _ xsSeq) = show xsSeq

instance Foldable OSet where
    foldMap f (OSet _ xsSeq) = foldMap f xsSeq
    x `elem` (OSet xsSet _) = x `elem` xsSet

instance OrderedSet a OSet where
    empty = OSet Set.empty Seq.empty
    singleton x = OSet (Set.singleton x) (Seq.singleton x)
    fromListL = foldl' (|>) empty
    fromListR = foldl' (>|) empty
    member x (OSet xsSet _) = x `Set.member` xsSet
    notMember = (not .) . member
    map f (OSet _ xsSeq) = foldl' (|>) empty (f <$> xsSeq)
    filter p (OSet xsSet xsSeq) = OSet (Set.filter p xsSet) (Seq.filter p xsSeq)
    size (OSet xsSet _ ) = Set.size xsSet
    toSeq (OSet _ xsSeq) = xsSeq
    toAscList (OSet xsSet _) = Set.toAscList xsSet
    findIndex x (OSet _ xsSeq) = Seq.findIndexL (x ==) xsSeq
    elemAt (OSet _ xsSeq) idx = Seq.lookup idx xsSeq
    delete x o@(OSet xsSet xsSeq) =
        case Seq.elemIndexL x xsSeq of
            Nothing -> o
            Just idx -> OSet (Set.delete x xsSet) (Seq.deleteAt idx xsSeq)
    o0 \\ o1 = filter (`notMember` o1) o0

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
