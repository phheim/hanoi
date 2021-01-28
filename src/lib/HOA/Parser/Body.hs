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

import HOA.Formula (Formula(FAnd, FNot, FVar))

import Data.Bits (shiftR, testBit)

import Data.Maybe (Maybe(..), isJust, isNothing)

import Data.Set as S (Set, fromList)

import Text.Parsec (many, many1, optionMaybe, sepBy1, unexpected)

import Text.Parsec.String (Parser)

import Text.ParserCombinators.Parsec.Char (char)

import qualified Data.Map.Strict as M (Map)

-----------------------------------------------------------------------------
-- | The body parser takes the number of atomic propositions to parse implicitly labeled edges.
-- | It takes a the map of Aliases as an environment to parse explicit labels.

bodyParser
  :: Int -> M.Map String (Formula Int) -> Parser [(Int, (Maybe String, Maybe (Formula Int), Maybe (Set Int), Set ([Int], Maybe (Formula Int), Maybe (Set Int))))]
bodyParser numAPs env = do
    states <- many1 stateParser
    keyword "--END--"
    return states
  where
    stateParser :: Parser (Int, (Maybe String, Maybe (Formula Int), Maybe (Set Int), Set ([Int], Maybe (Formula Int), Maybe (Set Int))))
    stateParser = do
        (state, mLabel, name, mAccSet) <- stateNameParser
        edges <- many edgeParser
        if isNothing mLabel then do
            let implicitLabels = foldr (\(_, l, _) -> (&&) (isNothing l)) True edges
            if implicitLabels then do
                let expectedEdges = 2^numAPs
                let actualEdges = length edges
                if expectedEdges == actualEdges then do
                    let labeledEdges = zipWith (\n (s, _, mA) -> (s, Just $ implicitLabel numAPs n, mA)) [0..] edges
                    return (state, (name, mLabel, mAccSet, S.fromList labeledEdges))
                else unexpected $ "Implicit labeled edges should be: " ++ show expectedEdges ++ " but were: " ++ show actualEdges
            else do
                let explicitLabels = foldr (\(_, l, _) -> (&&) (isJust l)) True edges
                if not explicitLabels then unexpected "Edge Labels inconsitent (some labeled, some not labeled)"
                else return (state, (name, mLabel, mAccSet, S.fromList edges))
        else return (state, (name, mLabel, mAccSet, S.fromList edges))

    stateNameParser :: Parser (Int, Maybe (Formula Int), Maybe String, Maybe (Set Int))
    stateNameParser = do
        keyword "State:"
        (~~)
        label <- optionMaybe (bracketParser labelParser)
        nat <- natParser
        name <- optionMaybe stringParser
        acc <- accSigParser
        return (nat, label, name, acc)

    edgeParser :: Parser ([Int], Maybe (Formula Int), Maybe (Set Int))
    edgeParser = do
        (~~)
        label <- optionMaybe (bracketParser labelParser)
        natStates <- sepBy1 natParser (do {_ <- char '&'; (~~)})
        accSet <- accSigParser
        return (natStates, label, accSet)

    accSigParser :: Parser (Maybe (Set Int))
    accSigParser =  optionMaybe $ braceParser $ do
        nats <- many1 natParser
        return $ S.fromList nats

    labelParser = labelExprParser env

    -- creates a label with numAP many atomic propositions and binary valuation according to n
    implicitLabel :: Int -> Int -> Formula Int
    implicitLabel numAP n = FAnd $ map props $ take numAP $ zip [0..] $ bits n

    props :: (Int, Bool) -> Formula Int
    props (n, True)  = FVar n
    props (n, False) = FNot $ FVar n

    bits n = (testBit n 0) : (bits (shiftR n 1))
-----------------------------------------------------------------------------
