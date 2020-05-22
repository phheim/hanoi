-----------------------------------------------------------------------------
-- |
-- Module      :  Hanoi
-- Maintainer  :  Philippe Heim (Heim@ProjectJARVIS.de)
--                Gideon Geier (geier@projectjarvis.de)
--
-- This module exports all stuff of the hanoi library
--
-----------------------------------------------------------------------------
module Hanoi
  ( AP
  , AcceptanceCondition
  , AcceptanceSet
  , AcceptanceType
  , HOA(..)
  , HOAAcceptanceName(..)
  , HOAProperty(..)
  , Label
  , Formula
  , State
  -- Parsing
  , hoaParser
  , parse
  -- Printing
  , printHOA
  , printHOALines
  -- Sanitizing
  , complete
  , sanitize
  -- Checking
  , checkValidHOA
  ) where

import HOA.Format
  ( AP
  , AcceptanceCondition
  , AcceptanceSet
  , AcceptanceType
  , HOA(..)
  , HOAAcceptanceName(..)
  , HOAProperty(..)
  , Label
  , State
  )

import Sat.Finite (Formula)

import HOA.Printer (printHOA, printHOALines)

import HOA.Parser (hoaParser, parse)

import HOA.Sanitizer (complete, sanitize)

import Spot.Autfilt
  ( autfilt
  , AutfiltInput(automaton)
  , defaultAutfiltInput
  , AutfiltResult(..)
  )

import System.IO (readFile)
import Control.Exception
  ( IOException
  , try)

-- | Check if a hoa is a valid one according to spot
checkValidHOA :: String -> IO (Either String Bool)
checkValidHOA hoa = do
  let input = defaultAutfiltInput {automaton = hoa}
  res <- autfilt input
  case res of
    AutfiltSuccess _ -> return $ Right True
    AutfiltNoMatch -> return $ Right False
    AutfiltFailure _ -> return $ Right False
    AutfiltException err -> return $ Left err


wrap :: (String -> Either String b) -> HOA -> Either String b
wrap f hoa = f (printHOA False hoa)

wrapTransform :: (String -> Either String String) -> HOA -> Either String HOA
wrapTransform f hoa = (wrap f hoa) >>= parse

wrapFile :: (String -> Either String b) -> String -> IO (Either String b)
wrapFile f path = do
  content <- try $ readFile path
  case content of
    Left ex -> do
      let err = show (ex :: IOException)
      return $ Left err
    Right content ->
      return $ (parse content) >>= (wrap f)

wrapFileTransform :: (String -> Either String String) -> String -> IO (Either String HOA)
wrapFileTransform f path = do
  res <- wrapFile f path
  return $ res >>= parse