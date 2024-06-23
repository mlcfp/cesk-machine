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

main :: IO ()
main = defaultMain
  [ Gen0.TestANF.tests
  , Gen0.TestCESK.tests
  , Gen0.TestGC.tests
  ]
