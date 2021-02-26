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

import HOAExamplesTest (exampleIdempotenceTests, exampleIsomorphicTests)
import SpotBasedTest (idempotenceTests, isomorphicTests)

-----------------------------------------------------------------------------
-- | The Tests
tests :: IO [Test]
tests = do
  dummyTest <- dummy
  spotIsomorphicTests <- SpotBasedTest.isomorphicTests
  spotIdempotenceTests <- SpotBasedTest.idempotenceTests
  return $ map Test $
    [dummyTest] ++ spotIsomorphicTests ++ exampleIsomorphicTests ++ spotIdempotenceTests ++ exampleIdempotenceTests

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
