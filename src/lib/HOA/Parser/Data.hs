-----------------------------------------------------------------------------
-- |
-- Module      :  HOA.Parser.Data
-- Maintainer  :  Gideon Geier (geier@projectjarvis.de)
--
-- Common data used by the parser module.
--
-----------------------------------------------------------------------------

module HOA.Parser.Data
  ( AcceptanceType(..)
  , HOAHeader(..)
  , HOAProperty(..)
  , globalDef
  ) where

-----------------------------------------------------------------------------

import Text.Parsec (alphaNum, char, letter, oneOf, (<|>))

import Text.Parsec.Token (GenLanguageDef(..), LanguageDef)

import Text.Parsec.Language (emptyDef)

import Data.Set (Set)

import HOA.Format (HOAAcceptanceName)

import HOA.Formula (Formula)

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
  , opStart        = oneOf ""
  , opLetter       = oneOf ""
  , reservedOpNames = ["!", "&", "|"]
  }

---------------------------------------------------------------------------------
-- | The parsing representation of a HOA Header

data HOAHeader =
   HOAHeader
    { -- | Number of states
      size :: Int
    , -- | Set of initial states
      initialStates :: Set [Int]
    , -- | Number of atomic propositions
      atomicPropositions :: Int
    , -- | Name of the atomic proposition
      atomicPropositionName :: Map Int String
    , -- | Controllable APs, typically outputs (Syntcomp Extension)
      controllableAPs :: Set Int
    , -- | Acceptance name
      acceptanceName :: Maybe HOAAcceptanceName
    , -- | Number of acceptance sets
      acceptanceSets :: Int
    , -- | Acceptance condition
      acceptance :: Formula AcceptanceType
    , -- | Tool name, parameters
      tool :: Maybe (String, Maybe String)
    , -- | Automaton name
      name :: Maybe String
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
