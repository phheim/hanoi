----------------------------------------------------------------------------
-- |
-- Module      :  Test
-- Maintainer  :  Philippe Heim
--
-- Standard TestSuite.
--
-----------------------------------------------------------------------------

module Test
  ( Test.tests
  ) where

-----------------------------------------------------------------------------
import Distribution.TestSuite

import HOAExamplesTest (exampleParsePrintIdempotenceTest, exampleTests)
import SpotBasedTest (tests)

-----------------------------------------------------------------------------
-- | The Tests
tests :: IO [Test]
tests = do
  dummyTest <- dummy
  spotTests <- SpotBasedTest.tests
  return $ map Test $
    [dummyTest] ++ spotTests ++ exampleTests ++ exampleParsePrintIdempotenceTest

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
