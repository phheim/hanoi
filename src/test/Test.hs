----------------------------------------------------------------------------
-- |
-- Module      :  Test
-- Maintainer  :  Philippe Heim
--
-- Standard TestSuite.
--
-----------------------------------------------------------------------------
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}

-----------------------------------------------------------------------------
module Test
  ( Test.tests
  ) where

-----------------------------------------------------------------------------
import Distribution.TestSuite

import HOAExamplesTest
  ( exampleTests
  )
import SpotBasedTest
  ( tests
  )

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
