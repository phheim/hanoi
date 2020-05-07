-----------------------------------------------------------------------------
-- |
-- Module      :  HOA.Parser
-- Maintainer  :  Gideon Geier (geier@projectjarvis.de)
--
-- Parser for Automata in HOA Format.
--
-----------------------------------------------------------------------------

{-# LANGUAGE ImplicitParams #-}

module HOA.Parser
  ( hoaParser
  ) where

-----------------------------------------------------------------------------

import Finite

import qualified Sat.Smart as Sm
  ( Formula
  )

import HOA.Format

import HOA.Parser.Util

import qualified HOA.Parser.Data as P
  ( HOAHeader(..)
  , AcceptanceType(..)
  )

import HOA.Parser.Body

import HOA.Parser.Header

import Text.Parsec.String
  ( Parser
  )

import qualified Data.Set as S
  ( Set
  , map
  )

import Data.Maybe
  ( fromMaybe
  )

import Data.Map.Strict
  ( fromList
  , (!)
  , mapKeysMonotonic
  )

-----------------------------------------------------------------------------
hoaParser :: Parser HOA
hoaParser = (~~) >> do
    header <- headerParser
    states <- bodyParser (P.atomicPropositions header) (P.aliases header)
    let ?bounds = HOA{size = length states, atomicPropositions = P.atomicPropositions header, acceptanceSets = P.acceptanceSets header}
     in do
      let names  = map (\(s, (n, _, _, _)) -> (value s, n)) states
      let labels = map (\(s, (_, l, _, _)) -> (value s, fmap smartFormulaToFinite $ fmap (fmap value) l)) states
      let accept = map (\(s, (_, _, a, _)) -> (value s, fmap (S.map value) a)) states
      let edges  = map (\(s, (_, _, _, e)) -> (value s, S.map convertEdge e)) states
      return $ HOA {
        size                  = P.size header
      , initialStates         = S.map value $ P.initialStates header
      , atomicPropositions    = P.atomicPropositions header
      , atomicPropositionName = (!) $ mapKeysMonotonic value $ P.atomicPropositionName header
      , controlableAPs        = S.map value $ P.controlableAPs header
      , acceptanceName        = fromMaybe Unknown $ P.acceptanceName header
      , acceptanceSets        = P.acceptanceSets header
      , acceptance            = smartFormulaToFinite $ fmap convertAccType $ P.acceptance header 
      , tool                  = P.tool header
      , name                  = P.name header
      , properties            = toFormatProperties $ P.properties header
      , edges                 = (!) $ fromList edges 
      , stateLabel            = (!) $ fromList labels 
      , stateAcceptance       = (!) $ fromList accept
      , stateName             = (!) $ fromList names 
      }
  where
    convertEdge :: (FiniteBounds HOA) => (Int, Maybe (Sm.Formula Int), Maybe (S.Set Int)) -> (State, Maybe Label, Maybe AcceptanceSets) 
    convertEdge (s, mFml, mAcc) = (value s, fmap smartFormulaToFinite $ fmap (fmap value) mFml, fmap (S.map value) mAcc)

    convertAccType :: (FiniteBounds HOA) => P.AcceptanceType -> AcceptanceType
    convertAccType (P.Fin b n) = Fin b $ value n
    convertAccType (P.Inf b n) = Inf b $ value n


-----------------------------------------------------------------------------
