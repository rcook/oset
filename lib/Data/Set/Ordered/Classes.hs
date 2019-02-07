{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Set.Ordered.Classes
    ( PreserveL(..)
    , PreserveR(..)
    , Values(..)
    ) where

class Values a c where
    empty :: c a
    singleton :: a -> c a
    fromList :: Ord a => [a] -> c a

class PreserveL a c where
    (|<) :: a -> c a -> c a
    (|>) :: c a -> a -> c a
    (|<>) :: c a -> c a -> c a
infixr 5 |<
infixl 5 |>
infixr 6 |<>

class PreserveR a c where
    (<|) :: a -> c a -> c a
    (>|) :: c a -> a -> c a
    (<>|) :: c a -> c a -> c a
infixr 5 <|
infixl 5 >|
infixr 6 <>|
