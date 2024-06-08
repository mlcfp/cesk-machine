--------------------------------------------------------------------------------
-- Copyright 2024 Michael P Williams. All rights reserved.
--------------------------------------------------------------------------------

module Main
  ( main
  ) where

import Test.Framework
import qualified TestANF

main :: IO ()
main = defaultMain
  [ TestANF.tests
  ]
