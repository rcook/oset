{-|
Module      : Data.Set.Ordered.LR
Description : Left- and right-biased wrappers for @OSet@
Copyright   : (C) Richard Cook, 2019
Licence     : MIT
Maintainer  : rcook@rcook.org
Stability   : stable
Portability : portable
-}

{-# OPTIONS_GHC -Wall -Werror #-}

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE StandaloneDeriving #-}

module Data.Set.Ordered.LR
    ( OSetL(..)
    , OSetR(..)
    ) where

import           Data.Data (Data)
import           Data.Foldable (Foldable(..))
import           Data.Monoid (Monoid(..))
import           Data.Semigroup (Semigroup(..))
import           Data.Set.Ordered.Classes
                    ( PreserveL(..)
                    , PreserveR(..)
                    , Values(..)
                    )
import           Data.Set.Ordered.OSet ((<>|), (|<>), OSet)
import           Prelude (Eq, Ord, Show(..))

-- | A left-biased 'OSet'.
newtype OSetL a = OSetL
    { unOSetL :: OSet a -- ^ the wrapped 'OSet'
    } deriving (Data, Eq, Foldable, Ord)

deriving instance Values a OSetL
deriving instance Ord a => PreserveL a OSetL

-- | \(O(1)\). A left-biased empty set.
{-
emptyL ::
    OSetL a -- ^ set
emptyL = OSetL OSet.empty
-}

-- | \(O(1)\). A left-biased singleton set containing the given element.
{-
singletonL ::
    a           -- ^ element
    -> OSetL a  -- ^ set
singletonL = OSetL . OSet.singleton
-}

instance Show a => Show (OSetL a) where
    show (OSetL o) = show o

instance Ord a => Semigroup (OSetL a) where
    (OSetL as) <> (OSetL bs) = OSetL (as |<> bs)

instance Ord a => Monoid (OSetL a) where
    mempty = empty

-- | A right-biased 'OSet'.
newtype OSetR a = OSetR
    { unOSetR :: OSet a -- ^ the wrapped 'OSet'
    } deriving (Data, Eq, Foldable, Ord)

deriving instance Values a OSetR
deriving instance Ord a => PreserveR a OSetR

-- | \(O(1)\). A right-biased empty set.
{-
emptyR ::
    OSetR a -- ^ set
emptyR = OSetR OSet.empty
-}

-- | \(O(1)\). A right-biased singleton set containing the given element.
{-
singletonR ::
    a           -- ^ element
    -> OSetR a  -- ^ set
singletonR = OSetR . OSet.singleton
-}

instance Show a => Show (OSetR a) where
    show (OSetR o) = show o

instance Ord a => Semigroup (OSetR a) where
    (OSetR as) <> (OSetR bs) = OSetR (as <>| bs)

instance Ord a => Monoid (OSetR a) where
    mempty = empty
