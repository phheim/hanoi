-----------------------------------------------------------------------------
-- |
-- Module      :  HOA.Parser.Header
-- Maintainer  :  Gideon Geier (geier@projectjarvis.de)
--
-- Parser for the Header section.
--
-----------------------------------------------------------------------------

module HOA.Parser.Header
  ( headerParser
  , versionParser
  ) where

-----------------------------------------------------------------------------

import HOA.Parser.Data
  ( HOAHeader(..)
  )

import HOA.Parser.Util

import HOA.Parser.LabelExpr

import HOA.Parser.AccCond

import HOA.Parser.AccName

import HOA.Parser.Properties

import Sat.Smart
  ( fTrue
  )

import Data.Maybe
  ( isJust
  , Maybe(..) 
  )

import Data.Set as S
  ( union
  , fromList
  , empty 
  )

import Control.Monad
  ( when
  )

import Text.Parsec
  ( (<|>)
  , sepBy1
  , many
  , unexpected
  , optionMaybe
  )

import Text.Parsec.String
  ( Parser
  )

import Text.ParserCombinators.Parsec.Char
  ( char
  )

import Data.Map.Strict as M
  ( empty
  , insert
  , member
  , fromAscList
  )

-----------------------------------------------------------------------------

headerParser
  :: Parser HOAHeader 

headerParser = do
    version <- versionParser
    when (version /= "v1") $ unexpected "Unsupported Format: only v1 of the HOA format supported"
    headerItemParser HOAHeader{size = 0,
                            initialStates =  S.empty,
                            atomicPropositions = -1,
                            atomicPropositionName = M.empty,
                            controllableAPs = S.empty,
                            acceptanceName = Nothing,
                            acceptanceSets = -1,
                            acceptance = fTrue,
                            tool = ("", Nothing),
                            name = "",
                            properties = S.empty,
                            aliases = M.empty}
  
  where
    -- recursive parser: all parsers called, except endParser, call headerItemParser again
    -- (after parsing their section and adding it to hoa) 
    -- this is necessary because there is no fixed order for the header-items
    headerItemParser hoa = 
          do {keyword "States:"; statesParser hoa}
      <|> do {keyword "Start:"; startParser hoa}
      <|> do {keyword "AP:"; apParser hoa}
      <|> do {keyword "Alias:"; aliasParser hoa}
      <|> do {keyword "Acceptance:"; acceptParser hoa}
      <|> do {keyword "acc-name:"; acceptNameParser hoa}
      <|> do {keyword "tool:"; toolParser hoa}
      <|> do {keyword "name:"; nameParser hoa}
      <|> do {keyword "properties:"; propParser hoa}
      <|> do {keyword "controllable-AP:"; capParser hoa}
      <|> do {keyword "--BODY--"; endParser hoa}

    statesParser hoa = if size hoa /= 0 then errDoubleDef "States"
      else do 
        num <- natParser
        headerItemParser hoa{size = num}

    startParser hoa = do 
        states <- sepBy1 natParser (do {_ <- char '&'; (~~)})
        headerItemParser hoa{initialStates = union (initialStates hoa) $ S.fromList states}

    apParser hoa = if atomicPropositions hoa /= -1 then errDoubleDef "AP" 
      else do
        num <- natParser;
        aps <- many stringParser
        if num /= length aps then unexpected "number of APs did not match actual APs"
        else headerItemParser hoa{atomicPropositions = num, atomicPropositionName = M.fromAscList $ zip [0..] aps} 

    aliasParser hoa = do 
            _ <- char '@' 
            id <- identParser
            expr <- labelExprParser (aliases hoa)
            if M.member id $ aliases hoa then errDoubleDef $ "alias " ++ id
            else headerItemParser hoa{aliases = M.insert id expr (aliases hoa) }

    acceptParser hoa = if acceptanceSets hoa /= -1 then errDoubleDef "Acceptance"   
      else do
            nat <- natParser
            acc <- accCondParser
            headerItemParser hoa{acceptanceSets = nat, acceptance = acc}

    acceptNameParser hoa = if isJust $ acceptanceName hoa then errDoubleDef "AcceptanceName"
      else do
            accName <- accNameParser
            headerItemParser hoa {acceptanceName = Just accName}

    toolParser hoa = if fst (tool hoa) /= "" then errDoubleDef "tool"
      else do
            str <- stringParser;
            mStr <- optionMaybe stringParser;
            headerItemParser hoa {tool = (str, mStr)} 

    nameParser hoa = if name hoa /= "" then errDoubleDef "name"
      else do
            str <- stringParser
            headerItemParser hoa {name = str}

    propParser hoa = do
            prop <- propertiesParser 
            headerItemParser hoa {properties = union (properties hoa) prop}

    capParser hoa = if not $ null $ controllableAPs hoa then errDoubleDef "controllable-AP"
      else do
            caps <- many natParser
            headerItemParser hoa {controllableAPs = S.fromList caps}

    endParser hoa = if acceptanceSets hoa == -1 then unexpected "Acceptance missing" 
      else
            if atomicPropositions hoa == -1 then return hoa {atomicPropositions = 0}
            else return hoa

    errDoubleDef str = 
      unexpected $
      str ++ " (already defined)" 

-----------------------------------------------------------------------------

versionParser
  :: Parser String 

versionParser =
  keyword "HOA:" >> identParser

-----------------------------------------------------------------------------
