-----------------------------------------------------------------------------
-- |
-- Module      :  HOA.Printer
-- Maintainer  :  Philippe Heim
-- Description :  Prints an HOA
--
-- This module prints an 'HOA' as a string.
--
-----------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE ImplicitParams        #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}

-----------------------------------------------------------------------------
module HOA.Printer
  ( printHOA
  , printHOALines
  ) where

-----------------------------------------------------------------------------

import Data.List as List (intercalate, sortOn)
import Data.Maybe (maybeToList)
import Data.Set as Set (Set, elems, toList)
import Finite (Finite, FiniteBounds, index, offset, v2t, values)
import HOA.Format
  ( AcceptanceSet
  , AcceptanceSets
  , AcceptanceType(..)
  , HOA(..)
  , HOAAcceptanceName(..)
  , HOAProperty(..)
  , Label
  , State
  )
import HOA.Formula (Formula(..))

-----------------------------------------------------------------------------
-- | 'printHOA' prints a 'HOA' to as a 'String'

printHOA :: HOA -> String
printHOA = unlines . printHOALines

-----------------------------------------------------------------------------
-- | 'printHOA' prints a 'HOA' as a list of 'String's representing
-- different lines

printHOALines :: HOA -> [String]
printHOALines hoa@HOA {..} =
  let ?bounds = hoa in
  let
    apNamesSorted = map (quote . atomicPropositionName) $ sortOn index values
    nameAcceptanceCond =
        printFormula
          (\case
              Fin True s  -> "Fin(" ++ strInd s ++ ")"
              Fin False s -> "Fin(!" ++ strInd s ++ ")"
              Inf True s  -> "Inf(" ++ strInd s ++ ")"
              Inf False s -> "Inf(!" ++ strInd s ++ ")")
          acceptance
  in
  [ "HOA: v1" ]
  ++
  -- name
  (case name of
    Just name -> ["name: " ++ quote name]
    Nothing   -> []
  )
  ++
  [ "States: " ++ show size ]
  ++
  -- Start
  (("Start: " ++ ) . printStateConj <$> elems initialStates)
  ++
  -- acc-name
  (case acceptanceName of
    Just acceptanceName -> ["acc-name: " ++ printAcceptanceName acceptanceName]
    Nothing             -> []
  )
  ++
  -- Acceptance
  ["Acceptance: " ++ show acceptanceSets ++ " " ++ nameAcceptanceCond]
  ++
  -- AP
  ["AP: " ++ unwords (show atomicPropositions : apNamesSorted)]
  ++
  -- controllable-AP
  ["controllable-AP: " ++ unwords (map strInd $ toList controllableAPs) | not (null controllableAPs)]
  ++
  -- properties
  [ "properties: " ++ unwords ("explicit-labels" : map printProperty (toList properties)) ]
  ++
  -- tool
  (case tool of
    Just (tool, Nothing)    -> ["tool: " ++ tool]
    Just (tool, Just param) -> ["tool: " ++ tool ++ " " ++ param]
    Nothing                 -> []
  )
  ++
  ["--BODY--"]
  ++
  concatMap printState values
  ++
  ["--END--"]

  where
    printState :: FiniteBounds HOA => State -> [String]
    printState s =
      unwords (
        "State:"
        :
        maybeToList (printLabel <$> stateLabel s)
        ++
        [strInd s]
        ++
        maybeToList (quote <$> stateName s)
        ++
        (case stateAcceptance s of
          Just aSets -> [brCurly $ unwords (map strInd $ elems aSets)]
          Nothing    -> []
        )
      )
      :
      map (("  " ++) . printEdge) (toList $ edges s)

    printEdge ::
          FiniteBounds HOA
      => ([State], Maybe Label, Maybe (Set AcceptanceSet))
      -> String
    printEdge edge =
      let (target, label, aSets) = edge
      in
      unwords . concat $
        [ maybeToList $ printLabel <$> label
        , [printStateConj target]
        , maybeToList $ printAcceptanceSets <$> aSets
        ]

    printAcceptanceSets :: FiniteBounds HOA => AcceptanceSets -> String
    printAcceptanceSets aSets =
      brCurly $ unwords $ map strInd $ toList aSets

    printLabel :: FiniteBounds HOA => Label -> String
    printLabel label = brBox $ printFormula strInd label

    printStateConj :: FiniteBounds HOA => [State] -> String
    printStateConj = intercalate " & " . map strInd


-----------------------------------------------------------------------------
-- | Different library related printing methods

strInd :: (Finite HOA a, FiniteBounds HOA) => a -> String
strInd a = show (index a - offset (v2t a))

wrap :: String -> String -> String -> String
wrap prefix suffix s = prefix ++ s ++ suffix

quote :: String -> String
quote = wrap "\"" "\""

brRound :: String -> String
brRound = wrap "(" ")"

brBox :: String -> String
brBox = wrap "[" "]"

brCurly :: String -> String
brCurly = wrap "{" "}"



-----------------------------------------------------------------------------
-- | 'printProperty' prints a 'HOAProperty' as a 'String'

printProperty :: HOAProperty -> String
printProperty =
  \case
    ONLY_STATE_LABELS     -> "state-labels"
    ONLY_TRANS_LABELS     -> "trans-labels"
    PURE_STATE_ACCEPTANCE -> "state-acc"
    PURE_TRANS_ACCEPTRACE -> "trans-acc"
    UNIV_BRANCHING        -> "univ-branc"
    NO_UNIV_BRANCHING     -> "no-univ-branch"
    DETERMINISTIC         -> "deterministic"
    COMPLETE              -> "complete"
    UNAMBIGOUS            -> "unambiguous"
    STUTTER_INVARIANT     -> "stutter-invariant"
    WEAK                  -> "weak"
    VERY_WEAK             -> "very-weak"
    INHERENTLY_WEAK       -> "inherently-weak"
    TERMINAL              -> "terminal"
    TIGHT                 -> "tight"
    COLORED               -> "colored"

-----------------------------------------------------------------------------
-- | 'printAcceptanceName' prints an 'HOAAcceptanceName' as a 'String'

printAcceptanceName :: HOAAcceptanceName -> String
printAcceptanceName =
  \case
    Buchi -> "Buchi"
    CoBuchi -> "co-Buchi"
    GeneralizedBuchi n -> "generalized-Buchi " ++ show n
    GeneralizedCoBuchi n -> "generalized-co-Buchi " ++ show n
    Streett n -> "Streett " ++ show n
    Rabin n -> "Rabin " ++ show n
    GeneralizedRabin a b c ->
      "generalized-Rabin " ++ show a ++ " " ++ show b ++ " " ++ show c
    ParityMinOdd n  -> "parity min odd " ++ show n
    ParityMaxOdd n  -> "parity max odd " ++ show n
    ParityMinEven n -> "parity min even " ++ show n
    ParityMaxEven n -> "parity max even " ++ show n
    All             -> "all"
    None            -> "none"


-----------------------------------------------------------------------------
-- | 'printFormula' prints a 'Formula' as a 'String'. Note that the way
-- of printing these formulas is HOA specific and therefore not part of
-- the HOA.Formula module.

printFormula :: (a -> String) -> Formula a -> String
printFormula showVar = \case
  FTrue -> "t"
  FFalse -> "f"
  FVar a -> showVar a
  FNot f -> "!" ++ printSubFormula f
  FAnd fs ->
    intercalate " & " $ fmap printSubFormula fs
  FOr fs ->
    intercalate " | " $ fmap printSubFormula fs

  where
    printSubFormula = (brRound . printFormula showVar)

