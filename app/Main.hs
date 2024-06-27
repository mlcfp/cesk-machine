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

  -- e <- pure (SchemeExpApp
  --   [ (SchemeExpApp
  --       [ SchemeExpVar (SchemeVar "x")
  --       , SchemeExpInt 1
  --       ])
  --   , (SchemeExpApp
  --       [ SchemeExpVar (SchemeVar "y")
  --       , SchemeExpInt 2
  --       ])
  --   ])
  -- e' <- schemeNormalize e
  -- putStrLn $ show e
  -- putStrLn $ show e'

  -- putStrLn "exp:"
  -- t <- schemeRender e
  -- putStrLn $ T.unpack t

  -- putStrLn "anf exp:"
  -- t' <- schemeRender e'
  -- putStrLn $ T.unpack t'

  let Right prog = schemeParse testProg
  putStrLn $ show prog
  p <- schemeRender renderOptions prog
  putStrLn $ T.unpack p

  prog' <- normalizeProg prog
  putStrLn $ show prog'
  p' <- schemeRender renderOptions { renderOptionStyle = RenderPretty } prog'
  putStrLn $ T.unpack p'


-- testProg = "((f g) (h x) 3)"
-- testProg = "(+ 2 (+ 3 (+ 4 5)))"
-- testProg = "(f 1 2)"
testProg = factorialProgram

-- testProg = [r|
-- (define (f x) (h x))
-- (define (h x) (+ 1 x))
-- (f 20)
-- |]

factorialProgram = [r|
  (define (f n)
    (if (= n 0)
        1
        (* n (f (- n 1)))))
  (f 20)
  |]

f' = [r|
(define f
  (λ (n)
    (let ((g1 (= n 0)))
      (if g1
        1
        (let ((g2 (- n 1)))
          (let ((g3 (f g2)))
            (* n g3)))))))
(f 20)|]

fact = [r|
      #|
      factorial
      |#
      (define f
        (λ (n)
          (let ((g1 (= n 0)))
            (if g1
              1
              (let ((g2 (- n 1)))
                (let ((g3 (f g2)))
                  (* n g3)))))))
      ; factorial for 20
      (f 20)|]
