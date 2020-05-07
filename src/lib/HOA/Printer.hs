-----------------------------------------------------------------------------
-- |
-- Module      :  HOA.Printer
-- Maintainer  :  Philippe Heim (Heim@ProjectJARVIS.de)
--
-- Prints the internal HOA format
--
-----------------------------------------------------------------------------
{-# LANGUAGE LambdaCase, FlexibleInstances, MultiParamTypeClasses,
  DeriveGeneric, TemplateHaskell, RecordWildCards, FlexibleContexts,
  ImplicitParams #-}

-----------------------------------------------------------------------------
module HOA.Printer
  ( printHOA
  , printHOALines
  ) where

-----------------------------------------------------------------------------
import Control.Exception (assert)
import Data.List as List (sortOn, intercalate)
import Data.Set as Set (Set, toList)
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
import Sat.Finite (FormulaView(..))
import Sat.Finite (Formula, view)

-----------------------------------------------------------------------------
-- | Converts a HOA to a string
printHOA :: Bool -> HOA -> String
printHOA singelLine hoa =
  let lines = printHOALines hoa
   in if singelLine
        then concatMap (++ " ") lines
        else concatMap (++ "\n") lines

-----------------------------------------------------------------------------
-- | Converts a HOA to a list of strings (different potential lines)
printHOALines :: HOA -> [String]
printHOALines hoa@HOA {..} =
  let ?bounds = hoa
   in let startConj =
            case toList initialStates of
              [] -> assert False undefined
              s:sr -> foldl (\a e -> a ++ "&" ++ strInd e) (strInd s) sr
          apNamesSorted' =
            intercalate "\" \"" $ map atomicPropositionName $ sortOn index values
          apNamesSorted = "\"" ++ apNamesSorted' ++ "\""
          nameAcceptanceCond =
            printFormula
              (\case
                 Fin True s -> "Fin(" ++ strInd s ++ ")"
                 Fin False s -> "Fin(!" ++ strInd s ++ ")"
                 Inf True s -> "Inf(" ++ strInd s ++ ")"
                 Inf False s -> "Inf(!" ++ strInd s ++ ")")
              acceptance
       in [ "HOA: v1"
          , "name: " ++ name
          , "States: " ++ show size
          , "Start: " ++ startConj
          , "AP: " ++ (show atomicPropositions) ++ " " ++ apNamesSorted
          , "Acceptance: " ++ (show acceptanceSets) ++ " " ++ nameAcceptanceCond
          , "acc-name: " ++ (printAcceptanceName acceptanceName)
          , "properties: " ++
            (concatMap (\e -> printProperty e ++ " ") $ toList properties) ++
            "explicit-labels"
          , "controllable-AP: " ++
            concatMap (\e -> (strInd e) ++ " ") (toList controlableAPs)
          , "tool: " ++
            case tool of
              (name, Nothing) -> name
              (name, Just parameter) -> name ++ " " ++ parameter
          , "--BODY--"
          ] ++
          (concat (map printState values)) ++ ["--END--"]
  where
    printState :: FiniteBounds HOA => State -> [String]
    printState s =
      ("State: " ++
       strInd s ++
       " " ++
       stateName s ++
       case stateAcceptance s of
         Nothing -> ""
         Just aSets ->
           "{" ++ (concatMap (\s -> strInd s ++ " ") (toList aSets)) ++ "}") :
      map printEdge (toList $ edges s)
    --
    printEdge ::
         FiniteBounds HOA
      => (State, Maybe Label, Maybe (Set AcceptanceSet))
      -> String
    printEdge =
      \case
        (target, Nothing, Nothing) -> strInd target
        (target, Just label, Nothing) ->
          printLabel label ++ " " ++ (strInd target)
        (target, Nothing, Just aSets) ->
          (strInd target) ++ " " ++ printAccetingSets aSets
        (target, Just label, Just aSets) ->
          printLabel label ++
          " " ++ (strInd target) ++ " " ++ printAccetingSets aSets
    --
    printAccetingSets :: FiniteBounds HOA => AcceptanceSets -> String
    printAccetingSets aSets =
      "{" ++ (concatMap (\s -> strInd s ++ " ") (toList aSets)) ++ "}"
    --
    printLabel :: FiniteBounds HOA => Label -> String
    printLabel label = "[" ++ printFormula strInd label ++ "] "

-----------------------------------------------------------------------------
-- | Converts a HOA property to a string
printProperty :: HOAProperty -> String
printProperty =
  \case
    ONLY_STATE_LABELS -> "state-labels"
    ONLY_TRANS_LABELS -> "trans-labels"
    PURE_STATE_ACCEPTANCE -> "state-acc"
    PURE_TRANS_ACCEPTRACE -> "trans-acc"
    UNIV_BRANCHING -> "univ-branc"
    NO_UNIV_BRANCHING -> "no-univ-branch"
    DETERMINISTIC -> "deterministic"
    COMPLETE -> "complete"
    UNAMBIGOUS -> "unambiguous"
    STUTTER_INVARIANT -> "stutter-invariant"
    WEAK -> "weak"
    VERY_WEAK -> "very-weak"
    INHERENTLY_WEAK -> "inherently-weak"
    TERMINAL -> "terminal"
    TIGHT -> "tight"
    COLORED -> "colored"

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
    ParityMinOdd n -> "parity min odd " ++ (show n)
    ParityMaxOdd n -> "parity max odd " ++ (show n)
    ParityMinEven n -> "parity min even " ++ (show n)
    ParityMaxEven n -> "parity max even " ++ (show n)
    All -> "all"
    None -> "none"
    Unknown -> ""

-----------------------------------------------------------------------------
-- | Different library related printing methods
strInd :: (Finite HOA a, FiniteBounds HOA) => a -> String
strInd a = show (index a - offset (v2t a))

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
          concat $
          ["("] ++ (addInBetween ") & (" $ fmap printFormula' fs) ++ [")"]
        Or fs ->
          concat $
          ["("] ++ (addInBetween ") | (" $ fmap printFormula' fs) ++ [")"]
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
    --
    addInBetween _ [] = []
    addInBetween _ [x] = [x]
    addInBetween a (x:xr) = x : a : addInBetween a xr
