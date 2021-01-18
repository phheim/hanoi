----------------------------------------------------------------------------
-- |
-- Module      :  HOA.Utils
-- Maintainer  :  Marvin Stenger
--
-- TODO
--
-----------------------------------------------------------------------------
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE RecordWildCards       #-}

-----------------------------------------------------------------------------

module HOA.Utils
  ( numSuccessors
  , successors
  ) where

import HOA.Format
  ( HOA(..)
  , State
  )

import Data.Set
  ( Set
  )
import qualified Data.Set as Set

-----------------------------------------------------------------------------

successors
  :: HOA -> State -> Set State

successors HOA{..} s =
  Set.map (\(s',_,_) -> s' ) $ edges s


numSuccessors
  :: HOA -> State -> Int

numSuccessors hoa s =
  Set.size $ successors hoa s
