--------------------------------------------------------------------------------
-- Copyright 2024 Michael P Williams. All rights reserved.
--------------------------------------------------------------------------------

module Main
  ( main
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (getCurrentTime)
import Text.RawString.QQ
import Gen1.CESK
import Gen1.Normalize
import Gen1.Scheme

main :: IO ()
main = do
  t0 <- getCurrentTime
  putStrLn $ show t0

  -- a <- ceskRun "(+ 1 -2e3)"
  -- putStrLn $ show a
  -- a <- ceskRun "(pi)"
  -- putStrLn $ show a
  -- a <- ceskRun "(sin (@pi))"
  -- putStrLn $ show a
  -- a <- ceskRun "(let ((x (pi))) (sin x))"
  -- putStrLn $ show a
  -- a <- ceskRun "(let ((x 0.0)) (sin x))"
  -- putStrLn $ show a
  -- a <- ceskRun "(null)"
  -- putStrLn $ show a
  -- a <- ceskRun "(cons 1 (@null))"
  -- putStrLn $ show a
  -- a <- normalizePretty "(sin (pi))" >>= ceskRun
  -- putStrLn $ show a
  -- a <- normalizePretty [r|(cons "x" (cons 2 (cons 1 (null))))|] >>= ceskRun
  -- putStrLn $ show a
  -- a <- normalizePretty [r|(list "x" 2 1 "aa")|]
  -- putStrLn $ show a
  -- a <- ceskRun a
  -- putStrLn $ show a
  -- a <- ceskRun "(string-make (@cons #\\a (@null)))"
  a <- normalizePretty [r|
    (string-part (string-make (cons #\x (cons #\b (cons #\a (@null))))) 0 2) |]
  putStrLn $ show a
  a <- ceskRun a
  putStrLn $ show a


normalizePretty :: Text -> IO Text
normalizePretty p = do
  -- let x = schemeParse p
  -- putStrLn $ show x
  let Right ast = schemeParse p
  normalizeProg ast >>= schemeRender schemeRenderOptions
    { schemeRenderOptionStyle = SchemeRenderNormal }
