-----------------------------------------------------------------------------
-- |
-- Module      :  HOA.Parser
-- Maintainer  :  Gideon Geier
--
-- Parser for Automata in HOA Format.
--
-----------------------------------------------------------------------------
module HOA.Parser
  ( parse
  ) where

-----------------------------------------------------------------------------
import HOA.Format

import HOA.Parser.Util

import qualified HOA.Parser.Data as P (AcceptanceType(..), HOAHeader(..))

import HOA.Parser.Body

import HOA.Parser.Header

import Text.Parsec.String (Parser)

import qualified Text.Parsec as P (parse, unexpected)

import qualified Data.Set as S (Set, map)

import Data.Map.Strict (fromList, mapKeysMonotonic, (!))

-----------------------------------------------------------------------------
hoaParser :: Parser HOA
hoaParser =
  (~~) >> do
    header <- headerParser
    states <- bodyParser (P.atomicPropositions header) (P.aliases header)
    if P.size header /= 0 && P.size header /= length states
    then P.unexpected "Number of States does not match number given in \"States:\""
    else
      -- process raw parsed states to internal format
      -- using the finite library
      let
        names = map (\(s, (n, _, _, _)) -> (State s, n)) states
        labels =
            map
              (\(s, (_, l, _, _)) ->
                  (State s, fmap (fmap AP) l))
              states
        accept =
            map
              (\(s, (_, _, a, _)) -> (State s, fmap (S.map AcceptanceSet) a))
              states
        edges =
            map
              (\(s, (_, _, _, e)) -> (State s, S.map convertEdge e))
              states
      in
      return
        HOA
        { size = length states
        , initialStates = S.map (map State) $ P.initialStates header
        , atomicPropositions = P.atomicPropositions header
        , atomicPropositionName =
            (!) $ mapKeysMonotonic AP $ P.atomicPropositionName header
        , controllableAPs = S.map AP $ P.controllableAPs header
        , acceptanceName = P.acceptanceName header
        , acceptanceSets = P.acceptanceSets header
        , acceptance =
            convertAccType <$> P.acceptance header
        , tool = P.tool header
        , name = P.name header
        , properties = toFormatProperties $ P.properties header
        , edges = (!) $ fromList edges
        , stateLabel = (!) $ fromList labels
        , stateAcceptance = (!) $ fromList accept
        , stateName = (!) $ fromList names
        }

  where
    convertEdge ::
         ([Int], Maybe (Formula Int), Maybe (S.Set Int))
      -> ([State], Maybe Label, Maybe AcceptanceSets)
    convertEdge (s, mFml, mAcc) =
      ( map State s
      , fmap (fmap AP) mFml
      , fmap (S.map AcceptanceSet) mAcc)
    convertAccType :: P.AcceptanceType -> AcceptanceType
    convertAccType (P.Fin b n) = Fin b $ AcceptanceSet n
    convertAccType (P.Inf b n) = Inf b $ AcceptanceSet n

-----------------------------------------------------------------------------
type Error = String

parse :: String -> Either Error HOA
parse str =
  case P.parse hoaParser "Parser Error" str of
    Left err -> Left $ show err
    Right x  -> return x
-----------------------------------------------------------------------------
