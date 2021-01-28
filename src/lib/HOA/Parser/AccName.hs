-----------------------------------------------------------------------------
-- |
-- Module      :  HOA.Parser.AccName
-- Maintainer  :  Gideon Geier
--
-- Parser for the acc-name header-item.
--
-----------------------------------------------------------------------------
module HOA.Parser.AccName
  ( accNameParser
  ) where

-----------------------------------------------------------------------------
import HOA.Parser.Util

import HOA.Format (HOAAcceptanceName(..))

import Text.Parsec ((<|>))

import Text.Parsec.String (Parser)

-----------------------------------------------------------------------------
accNameParser :: Parser HOAAcceptanceName
accNameParser = accName
  where
    accName =
      (keyword "Buchi" >> return Buchi) <|>
      (keyword "co-Buchi" >> return CoBuchi) <|>
      (keyword "all" >> return All) <|>
      (keyword "none" >> return None) <|>
      (natName "generalized-Buchi" GeneralizedBuchi) <|>
      (natName "generalized-co-Buchi" GeneralizedCoBuchi) <|>
      (natName "Streett" Streett) <|>
      (natName "Rabin" Rabin) <|>
      (parityName "parity" "min" "odd" ParityMinOdd) <|>
      (parityName "parity" "max" "odd" ParityMaxOdd) <|>
      (parityName "parity" "min" "even" ParityMinEven) <|>
      (parityName "parity" "max" "even" ParityMaxEven) <|>
      genRabin

    genRabin = do
      keyword "generalized-Rabin"
      nat1 <- natParser
      nat2 <- natParser
      GeneralizedRabin nat1 nat2 <$> natParser

    parityName str1 str2 str3 op = do
      keyword str1
      keyword str2
      keyword str3
      op <$> natParser

    natName str op = do
      keyword str
      op <$> natParser

