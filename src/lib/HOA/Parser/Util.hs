-----------------------------------------------------------------------------
-- |
-- Module      :  HOA.Parser.Util
-- Maintainer  :  Gideon Geier (geier@projectjarvis.de)
--
-- Utils used in all Parts of the Parser.
--
-----------------------------------------------------------------------------

module HOA.Parser.Util where

-----------------------------------------------------------------------------

import HOA.Parser.Data
  ( globalDef
  , HOAProperty(..)
  )

import qualified HOA.Format as F
  ( HOAProperty(..)
  )

import Data.Functor.Identity
  ( Identity
  )

import Data.Set
  ( Set
  , empty
  , insert
  )

import Control.Monad
  ( void
  )

import Text.Parsec.String
  ( Parser
  )

import Text.Parsec.Token

import Sat.Smart as Sm
  ( FormulaView(..)
  , Formula
  , view
  )

import qualified Sat.Finite as Fin
  ( UnfinishedFormula
  , Formula
  , fVar
  , fTrue
  , fFalse
  , fNot
  , fAnd
  , fOr
  , fXOr
  , fEquiv
  , fImplies
  , finalize
  )

import Finite

-----------------------------------------------------------------------------

tokenparser
  :: GenTokenParser String p Identity

tokenparser =
  makeTokenParser globalDef

-----------------------------------------------------------------------------

bracketParser
  :: Parser a -> Parser a

bracketParser = brackets tokenparser

-----------------------------------------------------------------------------

parenParser
  :: Parser a -> Parser a

parenParser = parens tokenparser

-----------------------------------------------------------------------------
braceParser
  :: Parser a -> Parser a

braceParser = braces tokenparser

-----------------------------------------------------------------------------

natParser
  :: Parser Int

natParser = fmap fromInteger $ natural tokenparser

-----------------------------------------------------------------------------

stringParser
  :: Parser String

stringParser = stringLiteral tokenparser

-----------------------------------------------------------------------------

identParser
  :: Parser String

identParser = identifier tokenparser

-----------------------------------------------------------------------------

rOp
  :: String -> Parser ()

rOp = reservedOp tokenparser

-----------------------------------------------------------------------------

keyword
  :: String -> Parser ()

keyword = void . reserved tokenparser

-----------------------------------------------------------------------------

(~~)
  :: Parser ()

(~~) = whiteSpace tokenparser

-----------------------------------------------------------------------------
-- | Translates Parsing HOAProperties to the HOAProperties of the Format
--  looses the implicit/explicit label properties,
--  because after parsing everything is made explicit
toFormatProperties :: Set HOAProperty -> Set F.HOAProperty
toFormatProperties = foldl translateProperty empty

translateProperty :: Set F.HOAProperty -> HOAProperty -> Set F.HOAProperty
translateProperty s ONLY_STATE_LABELS       = insert F.ONLY_STATE_LABELS     s
translateProperty s ONLY_TRANS_LABELS       = insert F.ONLY_TRANS_LABELS     s
translateProperty s PURE_STATE_ACCEPTANCE   = insert F.PURE_STATE_ACCEPTANCE s
translateProperty s PURE_TRANS_ACCEPTRACE   = insert F.PURE_TRANS_ACCEPTRACE s
translateProperty s UNIV_BRANCHING          = insert F.UNIV_BRANCHING        s
translateProperty s NO_UNIV_BRANCHING       = insert F.NO_UNIV_BRANCHING     s
translateProperty s DETERMINISTIC           = insert F.DETERMINISTIC         s
translateProperty s COMPLETE                = insert F.COMPLETE              s
translateProperty s UNAMBIGOUS              = insert F.UNAMBIGOUS            s
translateProperty s STUTTER_INVARIANT       = insert F.STUTTER_INVARIANT     s
translateProperty s WEAK                    = insert F.WEAK                  s
translateProperty s VERY_WEAK               = insert F.VERY_WEAK             s
translateProperty s INHERENTLY_WEAK         = insert F.INHERENTLY_WEAK       s
translateProperty s TERMINAL                = insert F.TERMINAL              s
translateProperty s TIGHT                   = insert F.TIGHT                 s
translateProperty s COLORED                 = insert F.COLORED               s
translateProperty s IMPLICIT_LABELS         = s
translateProperty s EXPLICIT_LABELS         = s

-----------------------------------------------------------------------------
-- | Translates a Formula from Sat.Smart to Sat.Finite
smartFormulaToFinite :: (Finite b a, FiniteBounds b)
  => Sm.Formula a -> Fin.Formula a
smartFormulaToFinite = (Fin.finalize . smartFormulaToFinite') . Sm.view
smartFormulaToFinite' :: (Finite b a, FiniteBounds b)
  => FormulaView Sm.Formula a -> Fin.UnfinishedFormula a
smartFormulaToFinite' TTrue             = Fin.fTrue 
smartFormulaToFinite' FFalse            = Fin.fFalse 
smartFormulaToFinite' (Var v)           = Fin.fVar v
smartFormulaToFinite' (Not fml)         = Fin.fNot $ (smartFormulaToFinite' . view) fml 
smartFormulaToFinite' (And xs)          = Fin.fAnd $ map (smartFormulaToFinite' . view) xs
smartFormulaToFinite' (Or xs)           = Fin.fOr  $ map (smartFormulaToFinite' . view) xs
smartFormulaToFinite' (Equiv fmlA fmlB) = Fin.fEquiv    ((smartFormulaToFinite' . view) fmlA) ((smartFormulaToFinite' . view) fmlB)
smartFormulaToFinite' (Impl fmlA fmlB)  = Fin.fImplies  ((smartFormulaToFinite' . view) fmlA) ((smartFormulaToFinite' . view) fmlB)
smartFormulaToFinite' (XOr fmlA fmlB)   = Fin.fXOr      ((smartFormulaToFinite' . view) fmlA) ((smartFormulaToFinite' . view) fmlB)
