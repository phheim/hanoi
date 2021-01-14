-----------------------------------------------------------------------------
-- |
-- Module      :  HOA.Printer
-- Maintainer  :  Philippe Heim (Heim@ProjectJARVIS.de)
--
-- Prints the internal HOA format
--
-----------------------------------------------------------------------------
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE ImplicitParams        #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TemplateHaskell       #-}

-----------------------------------------------------------------------------
module HOA.Printer
  ( printHOA
  , printHOALines
  ) where

-----------------------------------------------------------------------------

import Data.List as List
  ( intercalate
  , sortOn
  )
import Data.Maybe
  ( maybeToList
  )
import Data.Set as Set
  ( Set
  , toList
  )
import Finite
  ( Finite
  , FiniteBounds
  , index
  , offset
  , v2t
  , values
  )
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
import Sat.Finite
  ( FormulaView(..)
  )
import Sat.Finite
  ( Formula
  , view
  )

-----------------------------------------------------------------------------
-- | Converts a HOA to a string
printHOA :: Bool -> HOA -> String
printHOA singelLine hoa =
  let lines = printHOALines hoa
   in if singelLine
        then unwords lines
        else unlines lines

-----------------------------------------------------------------------------
-- | Converts a HOA to a list of strings (different potential lines)
printHOALines :: HOA -> [String]
printHOALines hoa@HOA {..} =
  let ?bounds = hoa in
  let
    startConj = intercalate "&" $ map strInd $ toList initialStates
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
  [ "HOA: v1"
  , "name: " ++ (quote name)
  , "States: " ++ show size
  , "Start: " ++ startConj
  , "AP: "
    ++ unwords ((show atomicPropositions) : apNamesSorted)
  , "Acceptance: " ++ (show acceptanceSets) ++ " " ++ nameAcceptanceCond
  , "acc-name: " ++ (printAcceptanceName acceptanceName)
  , "properties: "
    ++ unwords ("explicit-labels" : (map printProperty $ toList properties))
  , "controllable-AP: "
    ++ unwords (map strInd $ toList controllableAPs)
  , "tool: "
    ++ case tool of
        (name, Nothing)        -> quote name
        (name, Just parameter) -> (quote name) ++ " " ++ (quote parameter)
  ]
  ++
  ["--BODY--"]
  ++
  concatMap printState values
  ++
  ["--END--"]

  where
    printState :: FiniteBounds HOA => State -> [String]
    printState s =
      ("State: "
        ++ strInd s
        ++ " "
        ++ quote (stateName s)
        ++ case stateAcceptance s of
            Nothing -> ""
            Just aSets ->
              brCurly $ unwords (map strInd $ toList aSets)
      )
      :
      map printEdge (toList $ edges s)

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
        , maybeToList $ printAccetingSets <$> aSets
        ]

    printAccetingSets :: FiniteBounds HOA => AcceptanceSets -> String
    printAccetingSets aSets =
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
    Unknown         -> ""


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
          intercalate " & " $ fmap (brRound . printFormula') fs
        Or fs ->
          intercalate " | " $ fmap (brRound . printFormula') fs
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
