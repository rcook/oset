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
                    ( OrderedSet(..)
                    , PreserveL(..)
                    , PreserveR(..)
                    )
import           Data.Set.Ordered.OSet ((<>|), (|<>), OSet)
import           Prelude (Eq, Ord, Show(..))

-- | A left-biased 'OSet'.
newtype OSetL a = OSetL
    { unOSetL :: OSet a -- ^ the wrapped 'OSet'
    } deriving (Data, Eq, Foldable, Ord)

deriving instance OrderedSet a OSetL
deriving instance Ord a => PreserveL a OSetL

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

deriving instance OrderedSet a OSetR
deriving instance Ord a => PreserveR a OSetR

instance Show a => Show (OSetR a) where
    show (OSetR o) = show o

instance Ord a => Semigroup (OSetR a) where
    (OSetR as) <> (OSetR bs) = OSetR (as <>| bs)

instance Ord a => Monoid (OSetR a) where
    mempty = empty
