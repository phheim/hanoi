-----------------------------------------------------------------------------
-- |
-- Module      :  HOA.Printer
-- Maintainer  :  Philippe Heim
--
-- Prints the internal HOA format
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
-- | Converts a HOA to a string
printHOA :: HOA -> String
printHOA = unlines . printHOALines

-----------------------------------------------------------------------------
-- | Converts a HOA to a list of strings (different potential lines)
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
    Just acceptanceName -> ["acc-name: " ++ (printAcceptanceName acceptanceName)]
    Nothing -> []
  )
  ++
  -- Acceptance
  ["Acceptance: " ++ (show acceptanceSets) ++ " " ++ nameAcceptanceCond]
  ++
  -- AP
  ["AP: " ++ unwords ((show atomicPropositions) : apNamesSorted)]
  ++
  -- controllable-AP
  (if not $ null controllableAPs
    then ["controllable-AP: " ++ unwords (map strInd $ toList controllableAPs)]
    else []
  )
  ++
  -- properties
  [ "properties: " ++ unwords ("explicit-labels" : (map printProperty $ toList properties)) ]
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
      (unwords $
        ["State:"]
        ++
        (maybeToList $ printLabel <$> stateLabel s)
        ++
        [strInd s]
        ++
        (maybeToList $ quote <$> stateName s)
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
        , maybeToList $ printAcceptingSets <$> aSets
        ]

    printAcceptingSets :: FiniteBounds HOA => AcceptanceSets -> String
    printAcceptingSets aSets =
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
-- | Converts a HOA property to a string
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
-- | Converts a HOA acceptance name to a string
printAcceptanceName :: HOAAcceptanceName -> String
printAcceptanceName =
  \case
    Buchi -> "Buchi"
    CoBuchi -> "co-Buchi"
    GeneralizedBuchi n -> "generalized-Buchi " ++ (show n)
    GeneralizedCoBuchi n -> "generalized-co-Buchi " ++ (show n)
    Streett n -> "Streett " ++ (show n)
    Rabin n -> "Rabin " ++ (show n)
    GeneralizedRabin a b c ->
      "generalized-Rabin " ++ (show a) ++ " " ++ (show b) ++ " " ++ (show c)
    ParityMinOdd n  -> "parity min odd " ++ (show n)
    ParityMaxOdd n  -> "parity max odd " ++ (show n)
    ParityMinEven n -> "parity min even " ++ (show n)
    ParityMaxEven n -> "parity max even " ++ (show n)
    All             -> "all"
    None            -> "none"


printFormula :: (a -> String) -> Formula a -> String
printFormula showVar = \case
        FTrue -> "t"
        FFalse -> "f"
        FVar a -> showVar a
        FNot f -> "!" ++ (brRound . printFormula showVar) f
        FAnd fs ->
          intercalate " & " $ fmap (brRound . printFormula showVar) fs
        FOr fs ->
          intercalate " | " $ fmap (brRound . printFormula showVar) fs

