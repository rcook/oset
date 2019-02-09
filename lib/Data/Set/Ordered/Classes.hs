{-|
Module      : Data.Set.Ordered.Classes
Description : Type classes defining standard ordered-set operations
Copyright   : (C) Richard Cook, 2019
Licence     : MIT
Maintainer  : rcook@rcook.org
Stability   : stable
Portability : portable
-}

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Set.Ordered.Classes
    ( -- * Common operations on ordered sets
      Index
    , OrderedSet(..)
    , -- * Insertion
      PreserveL(..)
    , PreserveR(..)
    ) where

import           Data.Sequence (Seq)
import           Prelude
                    ( Bool
                    , Eq
                    , Int
                    , Maybe
                    , Ord
                    )

-- | A zero-based index with respect to insertion order.
type Index = Int

-- | Common operations on ordered sets. Set type is @c@, element
-- type is @a@.
class OrderedSet a c where
    -- | \(O(1)\). The empty set.
    empty :: c a
    -- | \(O(1)\). A singleton set containing the given element.
    singleton :: a -> c a
    -- | \(O(N log(N))\). Create a set from a finite list of elements.
    -- If an element occurs multiple times in the original list, only
    -- the first occurrence is retained in the resulting set. The
    -- function 'Data.Foldable.Foldable.toList', \(O(N)\), can be used
    -- to return a list of the elements in the original insert order
    -- with duplicates removed.
    fromListL :: Ord a => [a] -> c a
    -- | \(O(N log(N))\). Create a set from a finite list of elements.
    -- If an element occurs multiple times in the original list, only
    -- the last occurrence is retained in the resulting set. The
    -- function 'Data.Foldable.Foldable.toList', \(O(N)\), can be used
    -- to return a list of the elements in the original insert order
    -- with duplicates removed.
    fromListR :: Ord a => [a] -> c a
    -- | \(O(log(N))\). Determine if the element is in the set.
    -- Evaluate to 'Data.Bool.True' if element is in set,
    -- 'Data.Bool.False' otherwise.
    member :: Ord a => a -> c a -> Bool
    -- | \(O(log(N))\). Determine if the element is not in the set.
    -- Evaluate to 'Data.Bool.True' if element is not in set,
    -- 'Data.Bool.False' otherwise.
    notMember :: Ord a => a -> c a -> Bool
    -- | \(O(N log(N))\). Return the set obtained by applying a function
    -- to each element of this set. Note that the resulting set may be
    -- smaller than the original. Along with the 'Ord' constraint, this
    -- means that 'Data.Set.Ordered.OSet.OSet' cannot provide a lawful
    -- 'Data.Functor.Functor' instance.
    map :: Ord b => (a -> b) -> c a -> c b
    -- | \(O(N)\). Filter a set by returning a set whose elements
    -- satisfy the predicate.
    filter :: (a -> Bool) -> c a -> c a
    -- | \(O(1)\). The number of elements in the set.
    size :: c a -> Int
    -- | \(O(1)\). Return ordered sequence of elements in set. For
    -- obtaining a useful 'Data.Functor.Functor' instance this is
    -- recommended over 'Data.Foldable.Foldable.toList' due to its
    -- \(O(1)\) performance. Similarly, if you want to pattern-match on
    -- the 'Data.Set.Ordered.OSet.OSet', obtain the sequence and use
    -- view patterns or pattern synonyms instead of converting to a
    -- list.
    toSeq :: c a -> Seq a
    -- | \(O(N)\). Convert the set to an ascending list of elements.
    toAscList :: c a -> [a]
    -- | \(O(N)\). Finds the index of the leftmost element that matches
    -- the specified element or returns 'GHC.Maybe.Nothing' if no
    -- matching element can be found.
    findIndex :: Eq a => a -> c a -> Maybe Index
    -- | \(O(log(min(i, N - i)))\). Return the element at the specified
    -- position, \(i\), counting from 0. If the specified position is
    -- out of range, this function returns 'GHC.Maybe.Nothing'.
    elemAt :: c a -> Index -> Maybe a
    -- | \(O(log N)\). Delete an element from the set.
    delete :: Ord a => a -> c a -> c a
    -- | \(O(N M)\). Find the set difference: @r \\\\ s@ removes all @M@
    -- values in @s@ from @r@ with @N@ values.
    (\\) :: Ord a => c a -> c a -> c a

-- | 'Data.Set.Ordered.OSet.OSet' and 'Data.Set.Ordered.OSetL'
-- operations that preserve elements from the left-hand operand in the
-- case of duplicate elements. Set type is @c@, element type is @a@.
class PreserveL a c where
    -- | \(O(log(N))\) if the element is not in the set, \(O(N)\) if the
    -- element is already in the set. Add an element to the left end of
    -- the sequence if the set does not already contain the element.
    -- Move the element to the left end of the sequence if the element
    -- is already present in the set.
    (|<) :: a -> c a -> c a
    infixr 5 |<
    -- | \(O(log(N))\). Add an element to the right end of the sequence
    -- if the set does not already contain the element. Otherwise ignore
    -- the element.
    (|>) :: c a -> a -> c a
    infixl 5 |>
    -- | \(O(Nlog(N))\) worst case. Add elements from the right-hand set
    -- to the left-hand set. If elements occur in both sets, then this
    -- operation discards elements from the right-hand set and preserves
    -- those from the left.
    (|<>) :: c a -> c a -> c a
    infixr 6 |<>

-- | 'Data.Set.Ordered.OSet.OSet' and 'Data.Set.Ordered.OSetR'
-- operations that preserve elements from the right-hand operand in the
-- case of duplicate elements. Set type is @c@, element type is @a@.
class PreserveR a c where
    -- | \(O(log(N))\). Add an element to the left end of the sequence
    -- if the set does not already contain the element. Otherwise ignore
    -- the element.
    (<|) :: a -> c a -> c a
    infixr 5 <|
    -- | \(O(log(N))\) if the element is not in the set, \(O(N)\) if the
    -- element is already in the set. Add an element to the right end of
    -- the sequence if the set does not already contain the element.
    -- Move the element to the right end of the sequence if the element
    -- is already present in the set.
    (>|) :: c a -> a -> c a
    infixl 5 >|
    -- | \(O(N^2)\) worst case. Add elements from the right-hand set to
    -- the left-hand set. If elements occur in both sets, then this
    -- operation discards elements from the left-hand set and preserves
    -- those from the right.
    (<>|) :: c a -> c a -> c a
    infixr 6 <>|
