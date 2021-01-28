-------------------------------------------------------------------------------
-- |
-- Module      :  HOA.Formula
-- Maintainer  :  Philippe Heim
-- Description :  Formula representation for the HOA format
--
-- This module provides an representation for formulas used in the HOA format
-- for labels and acceptance conditions.
--
-------------------------------------------------------------------------------
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase    #-}

-------------------------------------------------------------------------------
module HOA.Formula
  ( Formula(..)
  ) where

-------------------------------------------------------------------------------
import GHC.Generics (Generic)

-------------------------------------------------------------------------------
-- | 'Formula' is the datatype for a boolean formula

data  Formula a =
    -- | Constant true
    FTrue |
    -- | Constant false
    FFalse |
    -- | Variable
    FVar a |
    -- | Disjunction
    FOr [Formula a] |
    -- | Conjunction
    FAnd [Formula a] |
    -- | Negation
    FNot (Formula a)
 deriving (Show, Eq, Ord, Generic)

-------------------------------------------------------------------------------
-- | Derive the 'Functor' class for 'Formula'
instance Functor Formula where
    fmap f = \case
        FTrue   -> FTrue
        FFalse  -> FFalse
        FVar v  -> FVar (f v)
        FOr fs  -> FOr (fmap (fmap f) fs)
        FAnd fs -> FAnd (fmap (fmap f) fs)
        FNot sf -> FNot (fmap f sf)



-------------------------------------------------------------------------------
