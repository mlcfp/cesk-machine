--------------------------------------------------------------------------------
-- Copyright 2024 Michael P Williams. All rights reserved.
--------------------------------------------------------------------------------

module Main
  ( main
  ) where

-- import qualified Data.Text as T
import Data.Time.Clock (getCurrentTime)
import Text.RawString.QQ
import Gen1.CESK
-- import Gen1.Normalize
-- import Gen1.Scheme

main :: IO ()
main = do
  t0 <- getCurrentTime
  putStrLn $ show t0

  a <- ceskRun "(sin 0.0)"
  putStrLn $ show a
