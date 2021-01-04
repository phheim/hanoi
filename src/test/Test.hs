----------------------------------------------------------------------------
-- |
-- Module      :  Test
-- Maintainer  :  Philippe Heim
--
-- Standard TestSuite.
--
-----------------------------------------------------------------------------
{-# LANGUAGE LambdaCase, RecordWildCards #-}

-----------------------------------------------------------------------------
module Test
  ( Test.tests
  ) where

-----------------------------------------------------------------------------
import Distribution.TestSuite

import SpotBasedTest (tests)
import HOAExamplesTest (exampleTests)

-----------------------------------------------------------------------------
-- | The Tests
tests :: IO [Test]
tests = do
  dummyTest <- dummy
  spotTests <- SpotBasedTest.tests
  return $ map Test $ [dummyTest] ++ spotTests ++ exampleTests

-----------------------------------------------------------------------------
-- | This is the dummy test
dummy :: IO TestInstance
dummy =
  let dum =
        TestInstance
          { run = return $ Finished Pass
          , name = "dummy"
          , tags = []
          , options = []
          , setOption = \_ _ -> Right dum
          }
   in return dum
