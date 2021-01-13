-----------------------------------------------------------------------------
-- |
-- Module      :  HOA.Parser.Body
-- Maintainer  :  Gideon Geier (geier@projectjarvis.de)
--
-- Parser for the Body section.
--
-----------------------------------------------------------------------------

module HOA.Parser.Body
  ( bodyParser
  ) where

-----------------------------------------------------------------------------

import HOA.Parser.Util

import HOA.Parser.LabelExpr

import Sat.Smart
  ( Formula
  , fAnd
  , fVar
  , fNot
  )

import Data.Bits
  ( testBit
  , shiftR
  )

import Data.Maybe
  ( isJust
  , isNothing
  , Maybe(..) 
  )

import Data.Set as S
  ( fromList
  , Set
  )

import Text.Parsec
  ( sepBy1
  , many
  , many1
  , unexpected
  , option
  , optionMaybe
  )

import Text.Parsec.String
  ( Parser
  )

import Text.ParserCombinators.Parsec.Char
  ( char
  )

import qualified Data.Map.Strict as M
  ( Map
  )

-----------------------------------------------------------------------------
-- | The body parser takes the number of atomic propositions to parse implicitly labeled edges.
-- | It takes a the map of Aliases as an environment to parse explicit labels. 

bodyParser
  :: Int -> M.Map String (Formula Int) -> Parser [(Int, (String, Maybe (Formula Int), Maybe (Set Int), Set (Int, Maybe (Formula Int), Maybe (Set Int))))]
bodyParser numAPs env = do
    states <- many1 stateParser 
    keyword "--END--"
    return states 
  where
    stateParser :: Parser (Int, (String, Maybe (Formula Int), Maybe (Set Int), Set (Int, Maybe (Formula Int), Maybe (Set Int)))) 
    stateParser = do
        (state, mLabel, name, mAccSet) <- stateNameParser
        edges <- many edgeParser
        if isNothing mLabel then do
            let representEdges = map head edges
            let implicitLabels = foldr (\(_, l, _) -> (&&) (isNothing l)) True representEdges 
            if implicitLabels then do
                let expectedEdges = 2^numAPs
                let actualEdges = length representEdges 
                if  expectedEdges == actualEdges then do
                    let labeledEdges = map (\(n, xs) -> map (\(s, _, mA) -> (s, Just $ implicitLabel numAPs n, mA)) xs) $ zip [0..] edges
                    return (state, (name, mLabel, mAccSet, S.fromList $ concat labeledEdges))
                else unexpected $ "Implicit labeled edges should be: " ++ show expectedEdges ++ " but were: " ++ show actualEdges
            else do            
                let explicitLabels = foldr (\(_, l, _) -> (&&) (isJust l)) True representEdges
                if not explicitLabels then unexpected "Edge Labels inconsitent (some labeled, some not labeled)"
                else return (state, (name, mLabel, mAccSet, S.fromList $ concat edges))
        else return (state, (name, mLabel, mAccSet, S.fromList $ concat edges))

    stateNameParser :: Parser (Int, Maybe (Formula Int), String, Maybe (Set Int))
    stateNameParser = do
        keyword "State:"
        (~~)
        label <- optionMaybe (bracketParser labelParser)
        nat <- natParser
        name <- option "" stringParser
        acc <- accSigParser
        return (nat, label, name, acc)

    edgeParser :: Parser [(Int, Maybe (Formula Int), Maybe (Set Int))]
    edgeParser = do
        (~~)
        label <- optionMaybe (bracketParser labelParser)
        natStates <- sepBy1 natParser (do {_ <- char '&'; (~~)})
        accSet <- accSigParser
        return (foldl (\xs state -> (state, label, accSet):xs) [] natStates)

    accSigParser :: Parser (Maybe (Set Int))
    accSigParser =  optionMaybe $ braceParser $ do
        nats <- many1 natParser
        return $ S.fromList nats

    labelParser = labelExprParser env
  
    -- creates a label with numAP many atomic propositions and binary valuation according to n 
    implicitLabel :: Int -> Int -> Formula Int 
    implicitLabel numAP n = fAnd $ map props $ take numAP $ zip [0..] $ bits n 
   
    props :: (Int, Bool) -> Formula Int
    props (n, True)  = fVar n
    props (n, False) = fNot $ fVar n

    bits n = (testBit n 0) : (bits (shiftR n 1))
-----------------------------------------------------------------------------
