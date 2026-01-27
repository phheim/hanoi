-----------------------------------------------------------------------------
-----------------------------------------------------------------------------
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ImplicitParams #-}

-----------------------------------------------------------------------------

-- |
-- Module      :  HOA.Format
-- Maintainer  :  Philippe Heim
--
-- The internal representation of an HOA
module HOA.Format where

-----------------------------------------------------------------------------
import Data.Set as Set (Set)
import Data.List as List
import Finite
import Finite.TH (baseInstance, newInstance)
import GHC.Generics (Generic)

-----------------------------------------------------------------------------

-- | The type of a state, generated using the Finite library
newInstance "State"

-----------------------------------------------------------------------------

-- | The type of an atomic proposition, generated using the Finite library
newInstance "AP"

-----------------------------------------------------------------------------

-- | The type of an acceptance set, generated using the Finite library
newInstance "AcceptanceSet"

type AcceptanceSets = Set AcceptanceSet

-----------------------------------------------------------------------------

-- | The different properties of a HOA
-- Remark: The properties do not contain implicit-labels, explicit-labels,
-- as in the internal format all labels are explicit
-- (implicit ones have to be parsed before)
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
  deriving (Eq, Ord, Show)

-----------------------------------------------------------------------------

-- | All possible HOA acceptance names with the respective parameters
data HOAAcceptanceName
  = Buchi
  | CoBuchi
  | GeneralizedBuchi Int
  | GeneralizedCoBuchi Int
  | Streett Int
  | Rabin Int
  | GeneralizedRabin Int Int Int
  | ParityMinOdd Int
  | ParityMaxOdd Int
  | ParityMinEven Int
  | ParityMaxEven Int
  | All
  | None
  deriving (Show)

-----------------------------------------------------------------------------

-- | The definition of an acceptance condition, which is a propositional formula
-- over acceptance sets that are visited finitely of infinitely often
data AcceptanceType
  = Fin Bool AcceptanceSet
  | Inf Bool AcceptanceSet
  deriving (Eq, Ord, Show, Generic)

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
 deriving (Show, Eq, Ord)

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

type AcceptanceCondition = Formula AcceptanceType

instance Finite HOA Bool

-----------------------------------------------------------------------------

-- | The definition of a label, which is a propositional formula over
-- atomic propositions
type Label = Formula AP

-----------------------------------------------------------------------------

-- | The internal presentation of an HOA, note that alias and implicit labels
-- are not represented anymore
data HOA = HOA
  { -- | Number of states (set can be computed via the type)
    size :: Int,
    -- | Set of initial states (singletons) or conjuncts of initial states
    -- for alternating automata, each list forms a conjunct
    initialStates :: Set [State],
    -- | Number of atomic propositions (set can be computed via the type)
    atomicPropositions :: Int,
    -- | Name of the atomic proposition
    atomicPropositionName :: AP -> String,
    -- | Controllable APs, typically outputs (Syntcomp Extension)
    controllableAPs :: Set AP,
    -- | Acceptance name
    acceptanceName :: Maybe HOAAcceptanceName,
    -- | Number of acceptance sets (the sets can be computed via the type)
    acceptanceSets :: Int,
    -- | Acceptance condition
    acceptance :: AcceptanceCondition,
    -- | Tool name, parameters
    tool :: Maybe (String, Maybe String),
    -- | Automaton name
    name :: Maybe String,
    -- | Properties
    properties :: Set HOAProperty,
    -- | Set of edges for each state, an edge consists of target state
    -- (-conjunct) a optional label and an optional set of acceptance sets
    edges :: State -> Set ([State], Maybe Label, Maybe AcceptanceSets),
    -- | For each state a possible label
    stateLabel :: State -> Maybe Label,
    -- | For each state a possible set of acceptance sets
    stateAcceptance :: State -> Maybe AcceptanceSets,
    -- | Name of a state
    stateName :: State -> Maybe String
  }

-----------------------------------------------------------------------------

-- | The instantiation of the State type
baseInstance [t|HOA|] [|size|] "State"

-----------------------------------------------------------------------------

-- | The instantiation of the atomic proposition type
baseInstance [t|HOA|] [|atomicPropositions|] "AP"

-----------------------------------------------------------------------------

-- | The instantiation of the acceptance set type
baseInstance [t|HOA|] [|acceptanceSets|] "AcceptanceSet"

instance Finite HOA AcceptanceType

-- | 'states' returns all states of a 'HOA'
states :: HOA -> [State]
states hoa =
  let ?bounds = hoa
   in List.sortOn index values

-- | 'atomicProps' returns all atomic propositions of a 'HOA'
atomicProps :: HOA -> [AP]
atomicProps hoa =
  let ?bounds = hoa
   in List.sortOn index values


