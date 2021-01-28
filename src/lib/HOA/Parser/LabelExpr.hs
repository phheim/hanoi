-----------------------------------------------------------------------------
-- |
-- Module      :  HOA.Parser.LabelExpr
-- Maintainer  :  Gideon Geier
--
-- Parser for LabelExpressions in Aliases and State/Edge Labels.
--
-----------------------------------------------------------------------------

module HOA.Parser.LabelExpr
  ( labelExprParser
  ) where

-----------------------------------------------------------------------------

import HOA.Parser.Util

import Sat.Smart (Formula, fAnd, fFalse, fNot, fOr, fTrue, fVar)

import Data.Map.Strict as M (Map, lookup)

import Text.Parsec (unexpected, (<|>))

import Text.Parsec.Expr (Assoc(..), Operator(..), buildExpressionParser)

import Text.Parsec.String (Parser)

import Text.ParserCombinators.Parsec.Char (char)

-----------------------------------------------------------------------------

labelExprParser :: Map String (Formula Int) -> Parser (Formula Int)
labelExprParser env = expr
  where
    expr = buildExpressionParser table term
    table = [   [prefix "!" fNot],
                [binary "&" (\x y -> fAnd [x,y]) AssocLeft],
                [binary "|" (\x y -> fOr  [x,y]) AssocLeft]
            ]
    binary  name fun = Infix  (do{ rOp name; return fun })
    prefix  name fun = Prefix (do{ rOp name; return fun })
    term = parenParser expr
        <|> boolLabel
        <|> intLabel
        <|> stringLabel
    boolLabel = falseLabel <|> trueLabel
    falseLabel = do
            _ <- char 'f'
            (~~)
            return fFalse
    trueLabel = do
            _ <- char 't'
            (~~)
            return fTrue
            
    intLabel = fVar <$> natParser
            
    stringLabel = do
            _ <- char '@'
            id <- identParser
            case M.lookup id env of
                Just expr -> return expr
                Nothing   -> unexpected $ "@"++id++ " undefined"
