-----------------------------------------------------------------------------
-- |
-- Module      :  HOA.Sanitizer
-- Maintainer  :  Philippe Heim (Heim@ProjectJARVIS.de)
--
-- A sanitizer for the HOA format
--
-----------------------------------------------------------------------------
{-# LANGUAGE LambdaCase, FlexibleInstances, MultiParamTypeClasses,
  DeriveGeneric, TemplateHaskell, RecordWildCards, ImplicitParams #-}

-----------------------------------------------------------------------------
module HOA.Sanitizer
  ( sanitize
  , complete
  ) where

-----------------------------------------------------------------------------
import Data.Maybe (isNothing)
import Data.Set (insert)
import Finite
import HOA.Format

-----------------------------------------------------------------------------
-- | Error type, TODO: Use another error type
type Error = String

genError :: String -> Error
genError s = s

-----------------------------------------------------------------------------
-- | Checks whether a HOA has only right properties and is consitent
sanitize :: HOA -> Maybe Error
sanitize hoa =
  let ?bounds = hoa
   in test hoa sanitizer
  where
    test ::
         FiniteBounds HOA
      => HOA
      -> [(HOA -> Maybe Error, Maybe HOAProperty)]
      -> Maybe Error
    test _ [] = Nothing
    test aut ((f, Nothing):tr) =
      case f aut of
        Nothing -> test aut tr
        Just err -> Just err
    test aut ((f, Just prop):tr) =
      if prop `elem` properties aut
        then case f aut of
               Nothing -> test aut tr
               Just err -> Just err
        else test aut tr

-----------------------------------------------------------------------------
-- | Adds missing properties to an HOA
complete :: HOA -> HOA
complete hoa =
  let ?bounds = hoa
   in add hoa sanitizer
  where
    add ::
         FiniteBounds HOA
      => HOA
      -> [(HOA -> Maybe Error, Maybe HOAProperty)]
      -> HOA
    add aut [] = aut
    add aut ((_, Nothing):xr) = add aut xr
    add aut ((f, Just prop):xr) =
      case f aut of
        Just _ -> add aut xr -- Note: In this case, the automaton does not fulfill the propoerty
        Nothing -> add aut {properties = insert prop (properties aut)} xr

-----------------------------------------------------------------------------
-- | Checking methods an potential properties to add
sanitizer :: FiniteBounds HOA => [(HOA -> Maybe Error, Maybe HOAProperty)]
sanitizer =
  [ (checkOnlyStateLabels, Just ONLY_STATE_LABELS)
  , (checkOnlyTransLabels, Just ONLY_TRANS_LABELS)
  , (checkPureStateAcc, Just PURE_STATE_ACCEPTANCE)
  , (checkPureTransAcc, Just PURE_TRANS_ACCEPTRACE)
  , error "NOT FULLY IMPLEMENTED"
  ]

-----------------------------------------------------------------------------
-- | General checking method
check :: String -> Bool -> Maybe Error
check msg v =
  if v
    then Nothing
    else Just $ genError msg

-----------------------------------------------------------------------------
-- | Checks whether some HOA has only states lables
checkOnlyStateLabels :: FiniteBounds HOA => HOA -> Maybe Error
checkOnlyStateLabels hoa =
  check "Unexpected transition label found" $
  all
    (\v -> all (\(_, potLabel, _) -> isNothing potLabel) (edges hoa v))
    (values :: [State])

-----------------------------------------------------------------------------
-- | Checks whether some HOA has only transition labels
checkOnlyTransLabels :: FiniteBounds HOA => HOA -> Maybe Error
checkOnlyTransLabels hoa =
  check "Unexpected state label found" $
  all (\s -> isNothing $ stateLabel hoa s) (values :: [State])

-----------------------------------------------------------------------------
-- | Checks whether some HOA has only state based acceptance
checkPureStateAcc :: FiniteBounds HOA => HOA -> Maybe Error
checkPureStateAcc hoa =
  check "Unexpected transition acceptance found" $
  all
    (\v -> all (\(_, _, accSet) -> isNothing accSet) (edges hoa v))
    (values :: [State])

-----------------------------------------------------------------------------
-- | Checks whether some HOA has only transition labels
checkPureTransAcc :: FiniteBounds HOA => HOA -> Maybe Error
checkPureTransAcc hoa =
  check "Unexpected state acceptance found" $
  all (\s -> isNothing $ stateAcceptance hoa s) (values :: [State])
