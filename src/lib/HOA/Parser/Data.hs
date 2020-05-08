-----------------------------------------------------------------------------
-- |
-- Module      :  HOA.Parser.Data
-- Maintainer  :  Gideon Geier (geier@projectjarvis.de)
--
-- Common data used by the parser module.
--
-----------------------------------------------------------------------------

module HOA.Parser.Data
  ( globalDef
  , HOAHeader(..)
  , HOAProperty(..)
  , AcceptanceType(..)
  ) where

-----------------------------------------------------------------------------

import Text.Parsec
  ( (<|>)
  , char
  , letter
  , alphaNum
  )

import Text.Parsec.Token
  ( LanguageDef
  , GenLanguageDef(..)
  )

import Text.Parsec.Language
  ( emptyDef
  )

import Data.Set
  ( Set
  )

import HOA.Format
  ( HOAAcceptanceName
  )

import Sat.Smart
  ( Formula
  )

import Data.Map.Strict as Map

-----------------------------------------------------------------------------

-- | The language definition which is shared among all parsers.

globalDef
  :: LanguageDef a

globalDef =
  emptyDef
  { commentStart   = "/*"
  , commentEnd     = "*/"
  , commentLine    = ""
  , nestedComments = True
  , identStart     = letter <|> char '_'
  , identLetter    = alphaNum <|> char '_' <|> char '-'
  , caseSensitive  = True
  , reservedNames  = ["f", "t"]
  }

---------------------------------------------------------------------------------
-- | The parsing representation of a HOA Header

data HOAHeader =
   HOAHeader
    { -- | Number of states (set can be computed via the type)
      size :: Int
    , -- | Set of initial states
      initialStates :: Set Int
    , -- | Number of atomic propositions (set can be computed via the type)
      atomicPropositions :: Int
    , -- | Name of the atomic proposition
      atomicPropositionName :: Map Int String
    , -- | Controlable APs, typcally outputs (Syntcomp Extension)
      controlableAPs :: Set Int
    , -- | Acceptance name
      acceptanceName :: Maybe HOAAcceptanceName
    , -- | Number of acceptance sets (the sets can be computed via the type)
      acceptanceSets :: Int
    , -- | Acceptance condition
      acceptance :: Formula AcceptanceType
    , -- | Tool name (might be empty)
      tool :: (String, Maybe String)
    , -- | Automaton name
      name :: String
    , -- | Properties
      properties :: Set HOAProperty
    , -- | Aliases
      aliases :: Map String (Formula Int)
    }

-------------------------------------------------------------------------

data HOAProperty
  = ONLY_STATE_LABELS
  | ONLY_TRANS_LABELS
  | PURE_STATE_ACCEPTANCE
  | PURE_TRANS_ACCEPTRACE
  | UNIV_BRANCHING
  | NO_UNIV_BRANCHING
  | DETERMINISTIC
  | COMPLETE
  | UNAMBIGOUS
  | STUTTER_INVARIANT
  | WEAK
  | VERY_WEAK
  | INHERENTLY_WEAK
  | TERMINAL
  | TIGHT
  | COLORED
  | IMPLICIT_LABELS
  | EXPLICIT_LABELS
  deriving (Eq, Ord, Show)

-------------------------------------------------------------------------

data AcceptanceType
  = Fin Bool Int
  | Inf Bool Int
  deriving (Eq, Ord, Show)

-------------------------------------------------------------------------
