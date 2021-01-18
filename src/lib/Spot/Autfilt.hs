----------------------------------------------------------------------------
-- |
-- Module      :  Spot.Autfilt
-- Maintainer  :  Marvin Stenger
--
-- TODO
--
-----------------------------------------------------------------------------
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE RecordWildCards       #-}

-----------------------------------------------------------------------------
module Spot.Autfilt 
  ( AutfiltResult(..)
  , StateBasedAcceptance(..)
  , Parity(..)
  , Acceptance(..)
  , SimplificationGoal(..)
  , SimplificationLevel(..)
  , AutfiltInput(..)
  , defaultAutfiltInput
  , autfilt
  , hasExistentialBranching
  , hasUniversialBranching
  , isAlternating
  , isColored
  , isComplete
  , isDeterministic
  , isEmpty
  , isInherentlyWeak
  , isSemiDeterministic
  , isStutterInvariant
  , isTerminal
  , isUnambiguous
  , isVeryWeak
  , isWeak
  , areIsomorphic
  , areIsomorphic2
  , areEquivalent
  , areEquivalent2
  , doIntersect
  , transformCleanupAcceptance
  , transformCNFAcceptance
  , transformComplement
  , transformComplementAcceptance
  , transformDestutter
  , transformDNFAcceptance
  , transformDualize
  , transformExclusiveAPs
  , transformMergeTransitions
  , transformRemoveAPs
  , transformRemoveDeadStates
  , transformRemoveFin
  , transformRemoveUnreachableStates
  , transformRemoveUnusedAPs
  , transformSATMinimize
  , transformSeparateSets
  , transformSimplifyAcceptance
  , transformSplitEdges
  , transformStreettLike
  , transformStripAcceptance
  , combineAnd
  , combineOr
  ) where

-----------------------------------------------------------------------------

import System.Directory
  ( findExecutable
  )
import System.Exit
  ( ExitCode(..)
  )
import System.IO.Temp
  ( writeSystemTempFile
  )
import System.Process
  ( readProcessWithExitCode
  )

import Data.List
  ( intercalate
  )

-----------------------------------------------------------------------------
data AutfiltResult =
    AutfiltSuccess String
  | AutfiltNoMatch
  | AutfiltFailure String
  | AutfiltException String

-----------------------------------------------------------------------------
-- | autfilt (spot) plain wrapper
autfiltCMD :: String -> [String] -> IO AutfiltResult
autfiltCMD stdin args = do
  let executable = "autfilt"
  e <- findExecutable executable
  case e of
    Nothing -> return $ AutfiltException (executable ++ " not found")
    Just autfilt -> do
      (ec,out,err) <- readProcessWithExitCode autfilt args stdin
      case ec of
        ExitSuccess   -> return $ AutfiltSuccess out
        ExitFailure 1 -> return AutfiltNoMatch
        ExitFailure _ -> return $ AutfiltFailure err


-----------------------------------------------------------------------------
-- | TODO
data StateBasedAcceptance =
    StateBasedAcceptance       -- ^ state-based Automaton
  | TransitionBasedAcceptance  -- ^ transition-based Automaton (default)
  deriving (Show)

-----------------------------------------------------------------------------
-- | TODO
data Parity =
    ParityAny
  | ParityMin
  | ParityMax
  | ParityOdd
  | ParityEven
  | ParityMinOdd
  | ParityMinEven
  | ParityMaxOdd
  | ParityMaxEven
  deriving (Show)

-----------------------------------------------------------------------------
-- | TODO
data Acceptance =
    AcceptanceDefault
  | AcceptanceGeneric                                                                 -- ^ any acceptance condition is allowed
  | AcceptanceBuchi                                                                   -- ^ Büchi Automaton (implies stateBasedAcceptance)
  | AcceptanceCoBuchi { acceptance :: StateBasedAcceptance }                          -- ^ automaton with co-Büchi acceptance (will recognize a superset of the input language if not co-Büchi realizable)
  | AcceptanceMonitor { acceptance :: StateBasedAcceptance }                          -- ^ Monitor (accepts all finite prefixes of the given property)
  | AcceptanceParity { parity :: Parity, acceptance :: StateBasedAcceptance }         -- ^ automaton with parity acceptance
  | AcceptanceColoredParity { parity :: Parity, acceptance :: StateBasedAcceptance }  -- ^ colored automaton with parity acceptance
  | AcceptanceTGBA                                                                    -- ^ transition-based Generalized Büchi Automaton
  deriving (Show)

-----------------------------------------------------------------------------
-- | TODO
data SimplificationGoal =
    SimplificationGoalDefault
  | SimplificationGoalAny            -- ^ no preference, do not bother making it small or deterministic
  | SimplificationGoalDeterministic  -- ^ prefer deterministic automata (combine with generic to be sure to obtain a deterministic automaton)
  | SimplificationGoalSmall          -- ^ prefer small automata
  deriving (Show)

-----------------------------------------------------------------------------
-- | TODO
data SimplificationLevel =
    SimplificationLevelDefault
  | SimplificationLevelHigh    -- ^ all available optimizations
  | SimplificationLevelMedium  -- ^ moderate optimizations
  | SimplificationLevelLow     -- ^ minimal optimizations
  deriving (Show)

-----------------------------------------------------------------------------
-- | TODO
data AutfiltInput =
  AutfiltInput
    { -- | process the automaton
      automaton :: String

    , -- |  If false, properties listed in HOA files are ignored, unless they can be easily verified.  If true (the default) any supported property is trusted.
      trustHOA :: Bool

    , -- | output automaton acceptance
      acceptance :: Acceptance

    , -- | output a complete automaton
      complete :: Bool
    --, -- | output unambiguous automata
    --  unambiguous :: Bool

    , -- | simplicication goal
      simplificationGoal :: SimplificationGoal

    , -- | simplicication level
      simplificationLevel :: SimplificationLevel

    , -- | extra arguments
      extraArgs :: [String]
    }

-----------------------------------------------------------------------------
-- | Default AutfiltInput
defaultAutfiltInput :: AutfiltInput
defaultAutfiltInput =
  AutfiltInput
    { automaton           = ""
    , trustHOA            = True
    , acceptance          = AcceptanceDefault
    , complete            = False
    , simplificationGoal  = SimplificationGoalDefault
    , simplificationLevel = SimplificationLevelDefault
    , extraArgs           = []
    }


-----------------------------------------------------------------------------
class AutfiltArgument a where
  toString :: a -> String
  toString _ = ""
  toArgs :: a -> [String]
  toArgs _ = []

instance AutfiltArgument Parity where
  toString p = case p of
    ParityAny     -> "any"
    ParityMin     -> "min"
    ParityMax     -> "max"
    ParityOdd     -> "odd"
    ParityEven    -> "even"
    ParityMinOdd  -> "min odd"
    ParityMinEven -> "min even"
    ParityMaxOdd  -> "max odd"
    ParityMaxEven -> "max even"

instance AutfiltArgument Acceptance where
  toArgs a = case a of
    AcceptanceDefault -> []
    AcceptanceGeneric -> ["--generic"]
    AcceptanceBuchi -> ["--ba"]
    AcceptanceCoBuchi{..} ->
      let args = ["--cobuchi"] in
      case acceptance of
        StateBasedAcceptance -> args ++ ["--state-based-acceptance"]
        _                    -> args
    AcceptanceMonitor{..} ->
      let args = ["--monitor"] in
      case acceptance of
        StateBasedAcceptance -> args ++ ["--state-based-acceptance"]
        _                    -> args
    AcceptanceParity{..} ->
      let args = ["--parity=" ++ (toString parity)] in
      case acceptance of
        StateBasedAcceptance -> args ++ ["--state-based-acceptance"]
        _                    -> args
    AcceptanceColoredParity{..} ->
      let args = ["--colored-parity=" ++ (toString parity)] in
      case acceptance of
        StateBasedAcceptance -> args ++ ["--state-based-acceptance"]
        _                    -> args
    AcceptanceTGBA -> ["--tgba"]

instance AutfiltArgument SimplificationGoal where
  toArgs s = case s of
    SimplificationGoalDefault       -> []
    SimplificationGoalAny           -> ["--any"]
    SimplificationGoalDeterministic -> ["--deterministic"]
    SimplificationGoalSmall         -> ["--small"]

instance AutfiltArgument SimplificationLevel where
  toArgs s = case s of
    SimplificationLevelDefault -> []
    SimplificationLevelHigh    -> ["--high"]
    SimplificationLevelMedium  -> ["--medium"]
    SimplificationLevelLow     -> ["--low"]

instance AutfiltArgument AutfiltInput where
  toArgs AutfiltInput{..} =
    ["--trust-hoa=false"| not trustHOA]
    ++
    (toArgs acceptance)
    ++
    ["--complete" | complete]
    ++
    (toArgs simplificationGoal)
    ++
    (toArgs simplificationLevel)
    ++
    extraArgs

-----------------------------------------------------------------------------
-- | autfilt (spot) wrapper
autfilt :: AutfiltInput -> IO AutfiltResult
autfilt input@AutfiltInput{automaton} =
  autfiltCMD automaton (toArgs input)


check :: [String] -> String -> IO (Either String Bool)
check args hoa = do
  res <- autfiltCMD hoa args
  case res of
    AutfiltSuccess _     -> return $ Right True
    AutfiltNoMatch       -> return $ Right False
    AutfiltFailure err   -> return $ Left err
    AutfiltException err -> return $ Left err

checkAutomaton :: String -> String -> IO (Either String Bool)
checkAutomaton arg = check [arg]

hasExistentialBranching :: String -> IO (Either String Bool)
hasExistentialBranching = checkAutomaton "--has-exist-branching"

hasUniversialBranching :: String -> IO (Either String Bool)
hasUniversialBranching = checkAutomaton "--has-univ-branching"

isAlternating :: String -> IO (Either String Bool)
isAlternating = checkAutomaton "--is-alternating"

isColored :: String -> IO (Either String Bool)
isColored = checkAutomaton "--is-colored"

isComplete :: String -> IO (Either String Bool)
isComplete = checkAutomaton "--is-complete"

isDeterministic :: String -> IO (Either String Bool)
isDeterministic = checkAutomaton "--is-deterministic"

isEmpty :: String -> IO (Either String Bool)
isEmpty = checkAutomaton "--is-empty"

isInherentlyWeak :: String -> IO (Either String Bool)
isInherentlyWeak = checkAutomaton "--is-inherently-weak"

isSemiDeterministic :: String -> IO (Either String Bool)
isSemiDeterministic = checkAutomaton "--is-semi-deterministic"

isStutterInvariant :: String -> IO (Either String Bool)
isStutterInvariant = checkAutomaton "--is-stutter-invariant"

isTerminal :: String -> IO (Either String Bool)
isTerminal = checkAutomaton "--is-terminal"

isUnambiguous :: String -> IO (Either String Bool)
isUnambiguous = checkAutomaton "--is-unambiguous"

isVeryWeak :: String -> IO (Either String Bool)
isVeryWeak = checkAutomaton "--is-very-weak"

isWeak :: String -> IO (Either String Bool)
isWeak = checkAutomaton "--is-weak"

checkAutomata :: String -> [String] -> IO (Either String Bool)
checkAutomata arg = \case
  [] -> return $ Left "nothing given to check"
  [_] -> return $ Right True
  head:hoas -> do
    headFile <- writeSystemTempFile "head.hoa" head
    let args = [arg ++ "=" ++ headFile]
    results <- mapM (check args) hoas
    return $
      foldl1
        (\eAcc e ->
          case eAcc of
            Left err -> Left err
            Right bAcc ->
              case e of
                Left err -> Left err
                Right b  -> Right $ bAcc && b
        )
        results

checkAutomata2 :: String -> String -> String -> IO (Either String Bool)
checkAutomata2 arg h1 h2 = checkAutomata arg [h1,h2]

areIsomorphic :: [String] -> IO (Either String Bool)
areIsomorphic = checkAutomata "--are-isomorphic"

areIsomorphic2 :: String -> String -> IO (Either String Bool)
areIsomorphic2 = checkAutomata2 "--are-isomorphic"

areEquivalent :: [String] -> IO (Either String Bool)
areEquivalent = checkAutomata "--equivalent-to"

areEquivalent2 :: String -> String -> IO (Either String Bool)
areEquivalent2 = checkAutomata2 "--equivalent-to"

doIntersect :: String -> String -> IO (Either String Bool)
doIntersect = checkAutomata2 "--intersect"

transform :: [String] -> String -> IO (Either String String)
transform args hoa = do
  res <- autfiltCMD hoa args
  case res of
    AutfiltSuccess out -> return $ Right out
    AutfiltNoMatch -> return $ Left "transformation lead to no match, should never happen"
    AutfiltFailure err -> return $ Left err
    AutfiltException err -> return $ Left err

transformAutomaton :: String -> String -> IO (Either String String)
transformAutomaton arg = transform [arg]

transformCleanupAcceptance :: String -> IO (Either String String)
transformCleanupAcceptance = transformAutomaton "--cleanup-acceptance"

transformCNFAcceptance :: String -> IO (Either String String)
transformCNFAcceptance = transformAutomaton "--cnf-acceptance"

transformComplement :: String -> IO (Either String String)
transformComplement = transformAutomaton "--complement"

transformComplementAcceptance :: String -> IO (Either String String)
transformComplementAcceptance = transformAutomaton "--complement-acceptance"

transformDestutter :: String -> IO (Either String String)
transformDestutter = transformAutomaton "--destut"

transformDNFAcceptance :: String -> IO (Either String String)
transformDNFAcceptance = transformAutomaton "--dnf-acceptance"

transformDualize :: String -> IO (Either String String)
transformDualize = transformAutomaton "--dualize"

transformMergeTransitions :: String -> IO (Either String String)
transformMergeTransitions = transformAutomaton "--merge-transitions"

transformRemoveDeadStates :: String -> IO (Either String String)
transformRemoveDeadStates = transformAutomaton "--remove-dead-states"

transformRemoveFin :: String -> IO (Either String String)
transformRemoveFin = transformAutomaton "--remove-fin"

transformRemoveUnreachableStates :: String -> IO (Either String String)
transformRemoveUnreachableStates = transformAutomaton "--remove-unreachable-states"

transformRemoveUnusedAPs :: String -> IO (Either String String)
transformRemoveUnusedAPs = transformAutomaton "--remove-unused-ap"

transformSeparateSets :: String -> IO (Either String String)
transformSeparateSets = transformAutomaton "--separate-sets"

transformSimplifyAcceptance :: String -> IO (Either String String)
transformSimplifyAcceptance = transformAutomaton "--simplify-acceptance"

transformSplitEdges :: String -> IO (Either String String)
transformSplitEdges = transformAutomaton "--split-edges"

transformStreettLike :: String -> IO (Either String String)
transformStreettLike = transformAutomaton "--streett-like"

transformStripAcceptance :: String -> IO (Either String String)
transformStripAcceptance = transformAutomaton "--strip-acceptance"


combineAutomata :: String -> [String] -> IO (Either String String)
combineAutomata param = \case
  [] -> return $ Left "nothing given to combine"
  [hoa] -> return $ Right hoa
  hoa:hoas -> do
    hoaFiles <- mapM (writeSystemTempFile ".hoa") hoas
    let args = map (\file -> param ++ "=" ++ file) hoaFiles
    transform args hoa

combineAnd :: [String] -> IO (Either String String)
combineAnd = combineAutomata "--product"

combineOr :: [String] -> IO (Either String String)
combineOr = combineAutomata "--sum"


transformExclusiveAPs :: String -> [[String]] -> Bool -> IO (Either String String)
transformExclusiveAPs hoa apss simplify =
  let args =
        (map (\aps -> "--exclusive-ap=" ++ (intercalate "," aps)) apss)
        ++
        ["--simplify-exclusive-ap" | simplify]
  in
  transform args hoa


-- remove atomic propositions either by existential
-- quantification, or by assigning them 0 or 1
transformRemoveAPs :: String -> [(String,Maybe Bool)] -> IO (Either String String)
transformRemoveAPs hoa assignments =
  let param =
        "--remove-ap="
        ++
        (
          intercalate "," (map
              (\(ap,v) ->
                ap
                ++
                (
                  case v of
                    Nothing    -> ""
                    Just False -> "=0"
                    Just True  -> "=1"
                )
              )
            assignments
            )
        )
  in
  transformAutomaton param hoa

-- minimize the automaton using a SAT solver
-- (only works for deterministic automata). Supported
-- options are acc=STRING, states=N, max-states=N,
-- sat-incr=N, sat-incr-steps=N, sat-langmap,
-- sat-naive, colored, preproc=N. Spot uses by
-- default its PicoSAT distribution but an external
-- SATsolver can be set thanks to the SPOT_SATSOLVER
-- environment variable(see spot-x).
transformSATMinimize :: String -> Maybe String -> IO (Either String String)
transformSATMinimize hoa options =
  let param =
        "--sat-minimize"
        ++
        (
          case options of
            Nothing      -> ""
            Just options -> "=" ++ options
        )
  in
  transformAutomaton param hoa
