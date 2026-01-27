-----------------------------------------------------------------------------
-- |
-- Module      :  HOA.Format
-- Maintainer  :  Philippe Heim
--
-- The internal representation of an HOA
--
-----------------------------------------------------------------------------
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

-----------------------------------------------------------------------------
module HOA.Format where

-----------------------------------------------------------------------------
import Data.Set(Set)

-----------------------------------------------------------------------------

-- | The type of a state
newtype State = State Int
    deriving (Eq, Ord, Show)

-- | The type of an atomic proposition
newtype AP = AP Int
    deriving (Eq, Ord, Show)

-- | The type of an acceptance set
newtype AcceptanceSet = AcceptanceSet Int
    deriving (Eq, Ord, Show)

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
  deriving (Eq, Ord, Show)

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

-----------------------------------------------------------------------------

-- | The definition of a label, which is a propositional formula over
-- atomic propositions
type Label = Formula AP

-----------------------------------------------------------------------------

-- | The internal presentation of an HOA, note that alias and implicit labels
-- are not represented anymore
data HOA = HOA
  { -- | Number of states
    size :: Int,
    -- | Set of initial states (singletons) or conjuncts of initial states
    -- for alternating automata, each list forms a conjunct
    initialStates :: Set [State],
    -- | Number of atomic propositions
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

-- | 'states' returns all states of a 'HOA'
-- in order of index number
states :: HOA -> [State]
states hoa = map State [0 .. size hoa - 1]

-- | 'atomicProps' returns all atomic propositions of a 'HOA'
-- in order of index number
atomicProps :: HOA -> [AP]
atomicProps hoa = map AP [0 .. atomicPropositions hoa - 1]

-----------------------------------------------------------------------------
