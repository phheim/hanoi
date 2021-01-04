----------------------------------------------------------------------------
-- |
-- Module      :  HOAExamplesTest
-- Maintainer  :  Gideon Geier
--
-- Tests done using spots hoa tools
--
-----------------------------------------------------------------------------
module HOAExamplesTest
  ( exampleTests
  , hoaExamples
  ) where

-----------------------------------------------------------------------------
import Distribution.TestSuite

import SpotBasedTest (generateTest)

-----------------------------------------------------------------------------
exampleTests :: [TestInstance]
exampleTests = map (uncurry generateTest) $ zip hoaExamples [2001, 2002, 2003, 2032, 2004, 2005, 2006, 2007, 2008, 2011]


hoaExamples :: [String]
hoaExamples =
  [ unlines -- aut1.hoa 
    [ "HOA: v1"
    , "States: 2"
    , "Start: 0"
    , "acc-name: Rabin 1"
    , "Acceptance: 2 (Fin(0) & Inf(1))"
    , "AP: 2 \"a\" \"b\""
    , "--BODY--"
    , "State: 0 \"a U b\"   /* An example of named state */"
    , "  [0 & !1] 0 {0}"
    , "  [1] 1 {0}"
    , "State: 1"
    , "  [t] 1 {1}"
    , "--END--"
    ]
   
  , unlines -- aut2.hoa
    [ "HOA: v1"
    , "States: 3"
    , "Start: 0"
    , "acc-name: Rabin 1"
    , "Acceptance: 2 (Fin(0) & Inf(1))"
    , "AP: 2 \"a\" \"b\""
    , "--BODY--"
    , "State: 0 \"a U b\" { 0 }"
    , "  2  /* !a  & !b */"
    , "  0  /*  a  & !b */"
    , "  1  /* !a  &  b */"
    , "  1  /*  a  &  b */"
    , "State: 1 { 1 }"
    , "  1 1 1 1       /* four transitions on one line */"
    , "State: 2 \"sink state\" { 0 }"
    , "  2 2 2 2"
    , "--END--"
    ]

  , unlines -- aut3.hoa
    [ "HOA: v1"
    , "name: \"GFa & GFb\""
    , "States: 1"
    , "Start: 0"
    , "acc-name: generalized-Buchi 2"
    , "Acceptance: 2 (Inf(0) & Inf(1))"
    , "AP: 2 \"a\" \"b\""
    , "--BODY--"
    , "State: 0"
    , "  0       /* !a  & !b */"
    , "  0 {0}   /*  a  & !b */"
    , "  0 {1}   /* !a  &  b */"
    , "  0 {0 1} /*  a  &  b */"
    , "--END--"
    ]

  , unlines -- aut3.2.hoa
    [ "HOA: v1"
    , "name: \"GFa & GFb\""
    , "States: 1"
    , "Start: 0"
    , "acc-name: generalized-Buchi 2"
    , "Acceptance: 2 (Inf(0) & Inf(1))"
    , "AP: 2 \"a\" \"b\""
    , "--BODY--"
    , "State: 0"
    , "[!0 & !1] 0"
    , "[0 & !1]  0 {0}"
    , "[!0 & 1]  0 {1}"
    , "[0 & 1]   0 {0 1}"
    , "--END--"
    ]

  , unlines -- aut4.hoa
    [ "HOA: v1"
    , "name: \"GFa & GF(b & c)\""
    , "States: 1"
    , "Start: 0"
    , "acc-name: generalized-Buchi 2"
    , "Acceptance: 2 (Inf(0) & Inf(1))"
    , "AP: 3 \"a\" \"b\" \"c\""
    , "Alias: @a 0"
    , "Alias: @bc 1 & 2"
    , "--BODY--"
    , "State: 0"
    , "[!@a & !@bc] 0"
    , "[@a & !@bc]  0 {0}"
    , "[!@a & @bc]  0 {1}"
    , "[@a & @bc]   0 {0 1}"
    , "--END--"
    ]

  , unlines -- aut5.hoa
    [ "HOA: v1"
    , "name: \"GFa\""
    , "States: 2"
    , "Start: 0"
    , "Start: 1"
    , "acc-name: Buchi"
    , "Acceptance: 1 Inf(0)"
    , "AP: 1 \"a\""
    , "--BODY--"
    , "State: [0] 0 {0}"
    , "  0 1"
    , "State: [!0] 1"
    , "  0 1"
    , "--END--"
    ]

  , unlines -- aut6.hoa
    [ "HOA: v1"
    , "States: 3"
    , "Start: 0"
    , "acc-name: Buchi"
    , "Acceptance: 1 Inf(0)"
    , "AP: 1 \"a\""
    , "--BODY--"
    , "State: 0"
    , " [0] 1"
    , " [!0]  2"
    , "State: 1  /* former state 0 */"
    , " [0] 1 {0}"
    , " [!0] 2 {0}"
    , "State: 2  /* former state 1 */"
    , " [0] 1"
    , " [!0] 2"
    , "--END--"
    ]

  , unlines -- aut7.hoa
    [ "HOA: v1"
    , "name: \"GFa | G(b <-> Xa)\""
    , "Start: 0"
    , "acc-name: Buchi"
    , "Acceptance: 1 Inf(0)"
    , "AP: 2 \"a\" \"b\""
    , "properties: explicit-labels trans-labels"
    , "--BODY--"
    , "State: 0"
    , " [t] 1"
    , " [1] 2"
    , " [!1] 3"
    , "State: 1 \"GFa\""
    , " [0] 1 {0}"
    , " [!0] 1"
    , "State: 2 \"a & G(b <-> Xa)\" {0}"
    , " [0&1] 2"
    , " [0&!1] 3"
    , "State: 3 \"!a & G(b <-> Xa)\" {0}"
    , " [!0&1] 2"
    , " [!0&!1] 3"
    , "--END--"
    ]

  , unlines -- aut8.hoa
    [ "HOA: v1"
    , "name: \"GFa | G(b <-> Xa)\""
    , "Start: 0"
    , "acc-name: Buchi"
    , "Acceptance: 1 Inf(0)"
    , "AP: 2 \"a\" \"b\""
    , "properties: explicit-labels trans-labels trans-acc"
    , "--BODY--"
    , "State: 0"
    , " [t] 1"
    , " [1] 2"
    , " [!1] 3"
    , "State: 1 \"GFa\""
    , " [0] 1 {0}"
    , " [!0] 1"
    , "State: 2 \"a & G(b <-> Xa)\""
    , " [0&1] 2 {0}"
    , " [0&!1] 3 {0}"
    , "State: 3 \"!a & G(b <-> Xa)\""
    , " [!0&1] 2 {0}"
    , " [!0&!1] 3 {0}"
    , "--END--"
    ]

  , unlines -- aut11.hoa
    [ "HOA: v1"
    , "name: \"(Fa & G(b&Xc)) | c\""
    , "States: 4"
    , "Start: 0&2"
    , "Start: 3"
    , "acc-name: co-Buchi"
    , "Acceptance: 1 Fin(0)"
    , "AP: 3 \"a\" \"b\" \"c\""
    , "--BODY--"
    , "State: 0 \"Fa\""
    , "[t] 0 {0}"
    , "[0] 1"
    , "State: 1 \"true\""
    , "[t] 1"
    , "State: 2 \"G(b&Xc)\""
    , "[1] 2&3"
    , "State: 3 \"c\""
    , "[2] 1"
    , "--END--"
    ]
  ]
