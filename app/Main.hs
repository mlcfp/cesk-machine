--------------------------------------------------------------------------------
-- Copyright 2024 Michael P Williams. All rights reserved.
--------------------------------------------------------------------------------

module Main
  ( main
  ) where

import ANF
import qualified CESK as CESK
import Data.Either (fromRight)
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import Text.RawString.QQ

main :: IO ()
main = do
  t0 <- getCurrentTime
  putStrLn $ show t0
  -- let !e = CESK.run pFactoral
  -- t1 <- getCurrentTime
  -- putStrLn $ show e
  -- putStrLn $ show t0
  -- putStrLn $ show t1
  -- putStrLn $ show $ diffUTCTime t1 t0

  -- let a = parseANF "(let ((x 1)) x)"
  -- let a = parseANF "(λ (x y) #f)"
  -- let a = parseANF "(let (( y  1 )) x )"
  -- let a = parseANF "(let (( y  1 )) (λ (x y) #f) )"
  -- let a = parseANF "(let (( y  1 )) (let (( x  2 )) (* 2 3) ))"
  -- let a = parseANF "(let (( y  1 )) (call/cc 2))"
  -- let a = parseANF "(letrec (( y  1 )) x )"
  -- let a = parseANF s1
  -- putStrLn $ show a
  -- runTestProg "(define (y 1)) y"
  -- runTestProg "(define y (λ (n) (* 2 n))) (y 4)"
  let x = anfProg [r|
    (define y (λ (n) (* 2 n)))
    (define g (λ (x) (+ x 1)))
    (define sqr (λ (a) (* a a)))
    ;(sqr 2)
    (sqr 2)
  |]
  let e = CESK.run $ fromRight (error "") x
  putStrLn $ show e
