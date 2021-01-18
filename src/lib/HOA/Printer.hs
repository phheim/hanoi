-----------------------------------------------------------------------------
-- |
-- Module      :  HOA.Printer
-- Maintainer  :  Philippe Heim (Heim@ProjectJARVIS.de)
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

import Data.List as List (intercalate, sort, sortOn)
import Data.Maybe (maybeToList)
import Data.Set as Set (Set, toAscList, toList)
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
import Sat.Finite (Formula, FormulaView(..), view)

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
  -- Start
  ((("Start: " ++ ) . strInd) <$> (toAscList initialStates))
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
          Just aSets -> [brCurly $ unwords (map strInd $ toAscList aSets)]
          Nothing    -> []
        )
      )
      :
      map (("  " ++) . printEdge) (toList $ edges s)

    printEdge ::
          FiniteBounds HOA
      => (State, Maybe Label, Maybe (Set AcceptanceSet))
      -> String
    printEdge edge =
      let (target, label, aSets) = edge
      in
      unwords . concat $
        [ maybeToList $ printLabel <$> label
        , [strInd target]
        , maybeToList $ printAcceptingSets <$> aSets
        ]

    printAcceptingSets :: FiniteBounds HOA => AcceptanceSets -> String
    printAcceptingSets aSets =
      brCurly $ unwords $ map strInd $ toList aSets

    printLabel :: FiniteBounds HOA => Label -> String
    printLabel label = brBox $ printFormula strInd label

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
printFormula showVar = printFormula'
  where
    printFormula' form =
      case view form of
        TTrue -> "t"
        FFalse -> "f"
        Var a -> showVar a
        Not f -> "!" ++ printFormula' f
        And fs ->
          intercalate " & " $ sort $ fmap (brRound . printFormula') fs
        Or fs ->
          intercalate " | " $ sort $ fmap (brRound . printFormula') fs
        Impl f1 f2 ->
          let s1 = printFormula' f1
              s2 = printFormula' f2
           in "(!(" ++ s1 ++ ")) | " ++ s2
        Equiv f1 f2 ->
          let s1 = printFormula' f1
              s2 = printFormula' f2
           in "(" ++
              s1 ++ "&" ++ s2 ++ ") | ((!(" ++ s1 ++ ")) & (!(" ++ s2 ++ ")))"
        XOr f1 f2 ->
          let s1 = printFormula' f1
              s2 = printFormula' f2
           in "((!(" ++
              s1 ++ ")) & " ++ s2 ++ ") | ((!(" ++ s2 ++ ")) & " ++ s1 ++ ")"
