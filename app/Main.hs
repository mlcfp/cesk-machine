--------------------------------------------------------------------------------
-- Copyright 2024 Michael P Williams. All rights reserved.
--------------------------------------------------------------------------------

module Main
  ( main
  ) where

import qualified Gen0.CESK as CESK
import qualified Data.Text as T
import Data.Time.Clock (getCurrentTime)
import Text.RawString.QQ

import Gen1.Normalize
import Gen1.Scheme

main :: IO ()
main = do
  t0 <- getCurrentTime
  putStrLn $ show t0
  -- let e = CESK.run [r|
  --   (define y (λ (n) (* 2 n)))
  --   (define g (λ (x) (+ x 1)))
  --   (define sqr (λ (a) (* a a)))
  --   (sqr 2)
  --   ;(sqr 2,)
  -- |]
  -- case e of
  --   Left err -> putStrLn $ T.unpack err
  --   Right v -> putStrLn $ show v

  e <- pure (SchemeApp
    [ (SchemeApp
        [ SchemeExpVar (SchemeVar "x")
        , SchemeExpInt 1
        ])
    , (SchemeApp
        [ SchemeExpVar (SchemeVar "y")
        , SchemeExpInt 2
        ])
    ])
  e' <- schemeNormalize e
  putStrLn $ show e
  putStrLn $ show e'

  putStrLn "exp:"
  t <- schemeRender e
  putStrLn $ T.unpack t

  putStrLn "anf exp:"
  t' <- schemeRender e'
  putStrLn $ T.unpack t'
