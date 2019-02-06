{-|
Module      : Data.Set.Ordered
Description : An insertion-order-preserving set
Copyright   : (C) Richard Cook, 2019
Licence     : MIT
Maintainer  : rcook@rcook.org
Stability   : stable
Portability : portable

This module provides 'OSet', an insertion-order-preserving set, with type class
instances for 'Foldable' and 'Data' as well as a 'map' function and other
features. 'Semigroup' and 'Monoid' instances are provided on
'Data.Set.Ordered.Instances.OSetL' and 'Data.Set.Ordered.Instances.OSetR' which
are left- and right-biased wrappers respectively.

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
>     -- Append
>     let s3 = s0 |<> OSet.fromList [10, 10, 20, 20, 30, 30]
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
                    , Bool(..)
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

-- | \(O(log(N))\). Add an element to the left end of the sequence if the set
-- does not already contain the element. Otherwise ignore the element.
(<|) :: Ord a
    => a        -- ^ element
    -> OSet a   -- ^ set
    -> OSet a   -- ^ set
x <| o@(OSet xsSet xsSeq) =
    if x `Set.member` xsSet
        then o
        else OSet (Set.insert x xsSet) (x Seq.<| xsSeq)
infixr 5 <|

-- | \(O(log(N))\) if the element is not in the set, \(O(N)\) if the element is
-- already in the set. Add an element to the left end of the sequence if the set
-- does not already contain the element. Move the element to the left end of the
-- sequence if the element is already present in the set.
(|<) :: Ord a
    => a        -- ^ element
    -> OSet a   -- ^ set
    -> OSet a   -- ^ set
x |< (OSet xsSet xsSeq) =
    if x `Set.member` xsSet
        then
            let Just idx = Seq.elemIndexL x xsSeq
            in OSet xsSet (x Seq.<| Seq.deleteAt idx xsSeq)
        else OSet (Set.insert x xsSet) (x Seq.<| xsSeq)
infixr 5 |<

-- | \(O(log(N))\) if the element is not in the set, \(O(N)\) if the element is
-- already in the set. Add an element to the right end of the sequence if the
-- set does not already contain the element. Move the element to the right end
-- of the sequence if the element is already present in the set.
(>|) :: Ord a
    => OSet a   -- ^ set
    -> a        -- ^ element
    -> OSet a   -- ^ set
(OSet xsSet xsSeq) >| x =
    if x `Set.member` xsSet
        then
            let Just idx = Seq.elemIndexL x xsSeq
            in OSet xsSet (Seq.deleteAt idx xsSeq Seq.|> x)
        else OSet (Set.insert x xsSet) (xsSeq Seq.|> x)
infixl 5 >|

-- | \(O(log(N))\). Add an element to the right end of the sequence if the set
-- does not already contain the element. Otherwise ignore the element.
(|>) :: Ord a
    => OSet a   -- ^ set
    -> a        -- ^ element
    -> OSet a   -- ^ set
o@(OSet xsSet xsSeq) |> x
    | x `member` o = o
    | otherwise = OSet (Set.insert x xsSet) (xsSeq Seq.|> x)
infixl 5 |>

-- | \(O(N^2)\) worst case. Add elements from the right-hand set to the
-- left-hand set. If elements occur in both sets, then this operation discards
-- elements from the left-hand set and preserves those from the right.
(<>|) :: Ord a
    => OSet a   -- ^ set
    -> OSet a   -- ^ set
    -> OSet a   -- ^ set
(<>|) = foldl' (>|)
infixr 6 <>|

-- | \(O(Nlog(N))\) worst case. Add elements from the right-hand set to the
-- left-hand set. If elements occur in both sets, then this operation discards
-- elements from the right-hand set and preserves those from the left.
(|<>) :: Ord a
    => OSet a   -- ^ set
    -> OSet a   -- ^ set
    -> OSet a   -- ^ set
(|<>) = foldl' (|>)
infixr 6 |<>

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

-- | \(O(1)\). The number of elements in the set.
size ::
    OSet a  -- ^ set
    -> Int  -- ^ size
size (OSet xsSet _ ) = Set.size xsSet

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
filter ::
    (a -> Bool) -- ^ predicate
    -> OSet a   -- ^ set
    -> OSet a   -- ^ set
filter p (OSet xsSet xsSeq) = OSet (Set.filter p xsSet) (Seq.filter p xsSeq)

-- | \(O(N log(N))\). Return the set obtained by applying a function to each
-- element of this set. Note that the resulting set may be smaller than the
-- original. Along with the 'Ord' constraint, this means that 'OSet' cannot
-- provide a lawful 'Data.Functor.Functor' instance.
map :: Ord b
    => (a -> b) -- ^ function
    -> OSet a   -- ^ set
    -> OSet b   -- ^ set
map f (OSet _ xsSeq) = foldl' (|>) empty (f <$> xsSeq)

-- | \(O(1)\). Return ordered sequence of elements in set. For obtaining
-- a useful 'Data.Functor.Functor' instance this is recommended over 'toList'
-- due to its \(O(1)\) performance. Similarly, if you want to pattern-match on
-- the 'OSet', obtain the sequence and use view patterns or pattern synonyms
-- instead of converting to a list.
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
