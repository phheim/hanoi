----------------------------------------------------------------------------
-- |
-- Module      :  Spot.Randaut
-- Maintainer  :  Marvin Stenger
--
-- TODO
--
-----------------------------------------------------------------------------
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NamedFieldPuns        #-}

-----------------------------------------------------------------------------
module Spot.Randaut
  ( RandautResult(..)
  , randautCMD
  ) where

-----------------------------------------------------------------------------

import System.Directory (findExecutable)
import System.Exit (ExitCode(..))
import System.Process (readProcessWithExitCode)

-----------------------------------------------------------------------------
data RandautResult =
    RandautSuccess String
  | RandautFailure String
  | RandautException String

-----------------------------------------------------------------------------
-- | randaut (spot) plain wrapper
randautCMD :: String -> [String] -> IO RandautResult
randautCMD stdin args =
  let executable = "randaut"
  in
  findExecutable executable
  >>= \case
    Nothing -> return $ RandautException (executable ++ " not found")
    Just randaut -> do
      (ec,out,err) <- readProcessWithExitCode randaut args stdin
      case ec of
        ExitSuccess   -> return $ RandautSuccess out
        ExitFailure _ -> return $ RandautFailure err
