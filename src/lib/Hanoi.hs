-----------------------------------------------------------------------------
-- |
-- Module      :  Hanoi
-- Maintainer  :  Philippe Heim
--                Gideon Geier
--
-- This module exports all stuff of the hanoi library
--
-----------------------------------------------------------------------------

module Hanoi
  ( AP
  , AcceptanceCondition
  , AcceptanceSet
  , AcceptanceType
  , Formula(..)
  , HOA(..)
  , HOAAcceptanceName(..)
  , HOAProperty(..)
  , Label
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
    -- Utils
  , numSuccessors
  , successors
  ) where

-----------------------------------------------------------------------------

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

import HOA.Formula (Formula(..))

import HOA.Printer (printHOA, printHOALines)

import HOA.Parser (hoaParser, parse)

import HOA.Sanitizer (complete, sanitize)

import HOA.Utils (numSuccessors, successors)

import Control.Exception (IOException, try)
import System.IO (readFile)

-----------------------------------------------------------------------------

wrap :: (String -> Either String b) -> HOA -> Either String b
wrap f hoa = f (printHOA hoa)

wrapTransform :: (String -> Either String String) -> HOA -> Either String HOA
wrapTransform f hoa =
  wrap f hoa
  >>= parse

wrapFile :: (String -> Either String b) -> String -> IO (Either String b)
wrapFile f path = do
  content <- try $ readFile path
  case content of
    Left ex -> do
      let err = show (ex :: IOException)
      return $ Left err
    Right content ->
      return $ parse content >>= wrap f

wrapFileTransform :: (String -> Either String String) -> String -> IO (Either String HOA)
wrapFileTransform f path = do
  res <- wrapFile f path
  return $ res >>= parse
