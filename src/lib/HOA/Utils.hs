----------------------------------------------------------------------------
-- |
-- Module      :  HOA.Utils
-- Maintainer  :  Marvin Stenger
--
-- TODO
--
-----------------------------------------------------------------------------

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ImplicitParams        #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE RecordWildCards       #-}
{-# OPTIONS_GHC -Wno-missing-fields #-}

-----------------------------------------------------------------------------

module HOA.Utils
  ( genBounds
  , numSuccessors
  , successors
  ) where

import HOA.Format (HOA(..), State)

import Data.Set (Set)
import qualified Data.Set as Set

-----------------------------------------------------------------------------

genBounds ::
  (?size :: Int) =>
  (?atomicPropositions :: Int) =>
  (?acceptanceSets :: Int) =>
  HOA

genBounds = HOA
  { size = ?size
  , atomicPropositions = ?atomicPropositions
  , acceptanceSets = ?acceptanceSets
  }

successors
  :: HOA -> State -> Set [State]

successors HOA{..} s =
  Set.map (\(s',_,_) -> s' ) $ edges s


numSuccessors
  :: HOA -> State -> Int

numSuccessors hoa s =
  Set.size $ successors hoa s
