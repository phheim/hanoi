-----------------------------------------------------------------------------
-- |
-- Module      :  HOA.Parser.AccCond
-- Maintainer  :  Gideon Geier (geier@projectjarvis.de)
--
-- Parser for the acceptance-cond part of the Acceptance header-item.
--
-----------------------------------------------------------------------------

module HOA.Parser.AccCond
  ( accCondParser
  ) where

-----------------------------------------------------------------------------

import HOA.Parser.Util

import HOA.Parser.Data
  ( AcceptanceType(..)
  )

import Sat.Smart
  ( Formula
  , fVar
  , fAnd
  , fOr
  , fTrue
  , fFalse
  )

import Text.Parsec
  ( option
  , (<|>)
  )

import Text.Parsec.Expr

import Text.Parsec.String
  ( Parser
  )

import Text.ParserCombinators.Parsec.Char
  ( char
  )

-----------------------------------------------------------------------------

accCondParser :: Parser (Formula AcceptanceType)
accCondParser = expr
  where
    expr = buildExpressionParser table term
    table = [   [binary "&" (\x y -> fAnd [x,y]) AssocLeft],
                [binary "|" (\x y -> fOr [x,y]) AssocLeft]
            ]
    binary  name fun = Infix (do{ rOp name; return fun })
    term = parenParser expr
        <|> boolExrp
        <|> setExpr "Fin" Fin
        <|> setExpr "Inf" Inf

    boolExrp = falseExpr <|> trueExpr
    falseExpr = do
            _ <- char 'f'
            (~~)
            return fFalse
    trueExpr = do
            _ <- char 't'
            (~~)
            return fTrue
    setExpr str op = do
            keyword str
            (b, set) <- parenParser condParser
            return $ fVar $ op b set
    condParser = do
            b <- option True (char '!'>> return False)
            set <- natParser
            return (b, set)
