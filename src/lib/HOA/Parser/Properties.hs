-----------------------------------------------------------------------------
-- |
-- Module      :  HOA.Parser.Properties
-- Maintainer  :  Gideon Geier (geier@projectjarvis.de)
--
-- Parser for the properties header-item.
--
-----------------------------------------------------------------------------

module HOA.Parser.Properties
  ( propertiesParser
  ) where

-----------------------------------------------------------------------------

import HOA.Parser.Data
  ( HOAProperty(..)
  )

import HOA.Parser.Util

import Data.Set
  ( Set
  , fromList
  )

import Text.Parsec
  ( (<|>)
  , sepBy1
  )

import Text.Parsec.String
  ( Parser
  )

-----------------------------------------------------------------------------

propertiesParser :: Parser (Set HOAProperty) 
propertiesParser = (fmap fromList $ sepBy1 properties (~~))
  where
    properties =(keyword "state-labels"     >> return ONLY_STATE_LABELS)  
            <|> (keyword "trans-labels"     >> return ONLY_TRANS_LABELS) 
            <|> (keyword "state-acc"        >> return PURE_STATE_ACCEPTANCE) 
            <|> (keyword "trans-acc"        >> return PURE_TRANS_ACCEPTRACE) 
            <|> (keyword "univ-branch"      >> return UNIV_BRANCHING) 
            <|> (keyword "no-univ-branch"   >> return NO_UNIV_BRANCHING) 
            <|> (keyword "deterministic"    >> return DETERMINISTIC) 
            <|> (keyword "complete"         >> return COMPLETE) 
            <|> (keyword "unambigous"       >> return UNAMBIGOUS) 
            <|> (keyword "stutter-invariant">> return STUTTER_INVARIANT) 
            <|> (keyword "weak"             >> return WEAK) 
            <|> (keyword "very-weak"        >> return VERY_WEAK) 
            <|> (keyword "inherently-weak"  >> return INHERENTLY_WEAK) 
            <|> (keyword "terminal"         >> return TERMINAL) 
            <|> (keyword "tight"            >> return TIGHT) 
            <|> (keyword "colored"          >> return COLORED) 
            <|> (keyword "implicit-labels"  >> return IMPLICIT_LABELS) 
            <|> (keyword "explicit-labels"  >> return EXPLICIT_LABELS) 

