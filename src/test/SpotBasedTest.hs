----------------------------------------------------------------------------
-- |
-- Module      :  SpotBasedTest
-- Maintainer  :  Philippe Heim
--
-- Tests done using spots hoa tools
--
-----------------------------------------------------------------------------

{-# LANGUAGE LambdaCase #-}

-----------------------------------------------------------------------------

module SpotBasedTest
  ( generateTest
  , tests
  ) where

-----------------------------------------------------------------------------
import Distribution.TestSuite

import Hanoi (parse, printHOA)
import Spot.Autfilt
import Spot.Randaut (RandautResult(..), randautCMD)

import System.Directory (findExecutable)
import System.Process (readProcessWithExitCode)

-----------------------------------------------------------------------------
tests :: IO [TestInstance]
tests = do
  let seeds = [1 .. 100]
  let apCounts = [1 .. 10]
  spotTests <- createTests [(s,a) | s <- seeds, a <- apCounts]
  case spotTests of
    Left err -> do
      putStrLn err
      error err
    Right tests -> return tests

-----------------------------------------------------------------------------
type Error = String

-----------------------------------------------------------------------------
-- | Generate a random HOA using spots randaut into given a seed and
-- a number of APs
randHOA :: Int -> Int -> IO (Either Error String)
randHOA seed apCnt =
  let aps = map (\n -> "ap" ++ (show n)) [0 .. (apCnt - 1)]
  in
  randautCMD "" (aps ++ ["--seed=" ++ (show seed)])
  >>= \case
    RandautSuccess hoa   -> return $ Right hoa
    RandautFailure err   -> return $ Left err
    RandautException err -> return $ Left err

-----------------------------------------------------------------------------
-- | Check if a hoa is a valid one according to spot
checkValidHOA :: String -> IO (Either Error (Maybe Error))
checkValidHOA hoa = do
  let input = defaultAutfiltInput {automaton = hoa}
  res <- autfilt input
  case res of
    AutfiltSuccess _     -> return $ Right Nothing
    AutfiltNoMatch       -> return $ Right $ Just "no match"
    AutfiltFailure err   -> return $ Right $ Just err
    AutfiltException err -> return $ Left err

-----------------------------------------------------------------------------
-- | Generates a parser/printer test given an HOA and an name index
generateTest :: String -> Int -> TestInstance
generateTest hoa ind =
  let inst =
        TestInstance
          { run =
              case parse hoa of
                Left err -> do
                  putStrLn $ "Parser returned with error: " ++ err ++ " on:"
                  putStrLn hoa
                  return $ Finished $ Fail "PARSERBUG"
                Right parsedHoa -> do
                  let printed = printHOA parsedHoa
                  valid <- checkValidHOA printed
                  case valid of
                    Left err ->
                      return $ Finished $ Fail $ "TESTING FAILURE: " ++ err
                    Right (Just err) -> do
                      putStrLn "Printed HOA is not valid. Error:"
                      putStrLn err
                      putStrLn "Original:"
                      putStrLn hoa
                      putStrLn "Printed:"
                      putStrLn printed
                      return $ Finished $ Fail "PRINTERBUG"
                    Right Nothing -> do
                      isomorphic <- areIsomorphic2 hoa printed
                      case isomorphic of
                        Left err ->
                          return $ Finished $ Fail $ "TESTING FAILURE: " ++ err
                        Right False -> do
                          putStrLn
                            "Printed HOA is not isomorphic to original one"
                          putStrLn "Original:"
                          putStrLn hoa
                          putStrLn "Printed:"
                          putStrLn printed
                          return $ Finished $ Fail "PARSER- or PRINTERBUG"
                        Right True -> return $ Finished Pass
          , name = "Spot based parser/printer test " ++ (show ind)
          , tags = ["spot", "parser", "printer"]
          , options = []
          , setOption = \_ _ -> Right inst
          }
   in inst

-----------------------------------------------------------------------------
-- | Creates a list of test given a seed, atomic proposition list
createTests :: [(Int, Int)] -> IO (Either Error [TestInstance])
createTests seeds = do
  potHOAs <- mapM (\(s, ap) -> randHOA s ap) seeds
  let potHOAList = help potHOAs
  case potHOAList of
    Left err -> return $ Left err
    Right hoas ->
      return $ Right $ map (\(n, h) -> generateTest h n) $ zip [1 ..] hoas
  where
    help :: [Either Error String] -> Either Error [String]
    help [] = Right []
    help (Left err:_) = Left err
    help (Right h:xr) =
      case help xr of
        Left err -> Left err
        Right hr -> Right (h : hr)
