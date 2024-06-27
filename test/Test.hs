--------------------------------------------------------------------------------
-- Copyright 2024 Michael P Williams. All rights reserved.
--------------------------------------------------------------------------------

module Main
  ( main
  ) where

import Test.Framework
import qualified Gen0.TestANF
import qualified Gen0.TestCESK
import qualified Gen0.TestGC
import qualified Gen1.TestNormalize
import qualified Gen1.TestScheme

main :: IO ()
main = defaultMain
  [ Gen0.TestANF.tests
  , Gen0.TestCESK.tests
  , Gen0.TestGC.tests
  , Gen1.TestScheme.tests
  , Gen1.TestNormalize.tests
  ]
