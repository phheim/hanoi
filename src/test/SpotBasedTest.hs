----------------------------------------------------------------------------
-- |
-- Module      :  SpotBasedTest
-- Maintainer  :  Philippe Heim 
--
-- Tests done using spots hoa tools
--
-----------------------------------------------------------------------------
{-# LANGUAGE LambdaCase, RecordWildCards #-}

-----------------------------------------------------------------------------
module SpotBasedTest
  ( tests
  ) where

-----------------------------------------------------------------------------
import Distribution.TestSuite

import Hanoi (parse, printHOA)

import System.Directory (findExecutable)
import System.IO.Temp (writeSystemTempFile)
import System.Process (readProcessWithExitCode)

-----------------------------------------------------------------------------
tests :: IO [TestInstance]
tests = do
  let seeds = [100 .. 200]
  let apCounts = [1 .. 10]
  spotTests <- createTests $ concatMap (\s -> map (\a -> (s, a)) apCounts) seeds
  case spotTests of
    Left err -> do
      putStrLn err
      error err
    Right tests -> return tests

-----------------------------------------------------------------------------
type Error = String

-----------------------------------------------------------------------------
-- | Generate a random HOA using spots randAut into given a seed and
-- a number of APs
randHOA :: Int -> Int -> IO (Either Error String)
randHOA seed apCnt = do
  let aps = map (\n -> " ap" ++ (show n)) [0 .. (apCnt - 1)]
  potRandAut <- findExecutable "randaut"
  case potRandAut of
    Nothing -> return $ Left "randaut not found"
    Just randAut -> do
      (_, hoa, _) <-
        readProcessWithExitCode randAut (aps ++ ["--seed=" ++ (show seed)]) ""
      return $ Right hoa

-----------------------------------------------------------------------------
-- | Check if a hoa is a valid one according to spot
checkValidHOA :: String -> IO (Either Error Bool)
checkValidHOA hoa = do
  potAutFilt <- findExecutable "autfilt"
  case potAutFilt of
    Nothing -> return $ Left "autfilt not found"
    Just autfilt -> do
      hoaFile <- writeSystemTempFile "test.hoa" hoa
      (_, _, err) <- readProcessWithExitCode autfilt [hoaFile] ""
      return $ Right $ err == ""

-----------------------------------------------------------------------------
-- | Check if two automatons are isomorphic usings spots autfill
checkIsomorphic :: String -> String -> IO (Either Error Bool)
checkIsomorphic h1 h2 = do
  potAutFilt <- findExecutable "autfilt"
  case potAutFilt of
    Nothing -> return $ Left "autfilt not found"
    Just autfilt -> do
      h1File <- writeSystemTempFile "a.hoa" h1
      h2File <- writeSystemTempFile "b.hoa" h2
      (_, stdOut, _) <-
        readProcessWithExitCode
          autfilt
          ["--are-isomorphic=" ++ h1File, h2File]
          ""
      return $ Right $ stdOut /= ""

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
                  return $ Finished $ Fail $ "PARSERBUG"
                Right parsedHoa -> do
                  let printed = printHOA False parsedHoa
                  valid <- checkValidHOA printed
                  case valid of
                    Left err ->
                      return $ Finished $ Fail $ "TESTING FAILURE: " ++ err
                    Right False -> do
                      putStrLn "Printed HOA is not valid and looks like:"
                      putStrLn printed
                      return $ Finished $ Fail $ "PRINTERBUG"
                    Right True -> do
                      isomorphic <- checkIsomorphic hoa printed
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
                          return $ Finished $ Fail $ "PARSER- or PRINTERBUG"
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
  potHOAs <- sequence $ map (\(s, ap) -> randHOA s ap) seeds
  let potHOAList = help potHOAs
  case potHOAList of
    Left err -> return $ Left err
    Right hoas ->
      return $ Right $ map (\(n, h) -> generateTest h n) $ zip [0 ..] hoas
  where
    help :: [Either Error String] -> Either Error [String]
    help [] = Right []
    help (Left err:_) = Left err
    help (Right h:xr) =
      case help xr of
        Left err -> Left err
        Right hr -> Right (h : hr)
