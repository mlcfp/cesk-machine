--------------------------------------------------------------------------------
-- Copyright 2024 Michael P Williams. All rights reserved.
--------------------------------------------------------------------------------

module Gen1.TestCESK
  ( tests
  ) where

import Gen1.CESK
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (assertEqual)
import Text.RawString.QQ

tests :: Test
tests = testGroup "CESK"
  [ testVal
  , testVar
  , testMath
  , testDefine
  , testProgFactorial
  ]

testVal :: Test
testVal = testCase "val" $ do
  ceskRun "0" >>=
    assertEqual "int" (Right $ CESKValInt 0)
  ceskRun "#t" >>=
    assertEqual "true" (Right $ CESKValBool True)
  ceskRun "#f" >>=
    assertEqual "false" (Right $ CESKValBool False)

testVar :: Test
testVar = testCase "var" $ do
  ceskRun "(letrec (( y  1 )) y )" >>=
    assertEqual "var 1" (Right $ CESKValInt 1)
  ceskRun "(letrec((y #t))y)" >>=
    assertEqual "var 2" (Right $ CESKValBool True)
  ceskRun "(let ((y 1)) y)" >>=
    assertEqual "var 3" (Right $ CESKValInt 1)
  ceskRun "(let ((y 8)) (* 2 y))" >>=
    assertEqual "var 4" (Right $ CESKValInt 16)

testMath :: Test
testMath = testCase "math" $ do
  ceskRun "(+ 66 788)" >>=
    assertEqual "add" (Right $ CESKValInt 854)
  ceskRun "(- 66 788)" >>=
    assertEqual "sub" (Right $ CESKValInt (-722))
  ceskRun "(* 6 6)" >>=
    assertEqual "mul" (Right $ CESKValInt 36)
  ceskRun "(/ 6 3)" >>=
    assertEqual "div" (Right $ CESKValInt 2)
  ceskRun "(/ 6 7)" >>=
    assertEqual "div" (Right $ CESKValInt 0)
  ceskRun "(* (+ 1 (- 3 2)) 3)" >>=
    assertEqual "complex 1" (Right $ CESKValInt 6)

testDefine :: Test
testDefine = testCase "define" $ do
  ceskRun "(define y 1) y" >>=
    assertEqual "define 1" (Right $ CESKValInt 1)
  ceskRun "(define y (λ (n) (* 2 n))) (y 4)" >>=
    assertEqual "define 2" (Right $ CESKValInt 8)
  ceskRun [r|
      (define y (λ (n) (* 2 n)))
      (define g (λ (x) (+ x 1)))
      (define sqr (λ (a) (* a a)))
      (let ((r0 (g 1)))
        (let ((r1 (y r0)))
          (sqr r1)))
    |] >>=
      assertEqual "define 3" (Right $ CESKValInt 16)

testProgFactorial :: Test
testProgFactorial = testCase "factorial" $ do
  ceskRun [r|
    #| factorial version 1 |#
    (letrec ((f
      (λ (n)
        (let ((g1 (= n 0)))
          (if g1
            1
            (let ((g2 (- n 1)))
              (let ((g3 (f g2)))
                (* n g3))))))))
    ; factorial for 20
    (f 20))|] >>=
      assertEqual "ver 1" (Right $ CESKValInt 2432902008176640000)
  ceskRun [r|
    (define factorial (λ (x)
      (letrec ((f
        (λ (n)
          (let ((g1 (= n 0)))
            (if g1
              1
              (let ((g2 (- n 1)))
                (let ((g3 (f g2)))
                  (* n g3))))))))
        (f x))))
    (let ((r0 (factorial 3)))
      (factorial r0))
  |] >>=
    assertEqual "ver 2" (Right $ CESKValInt 720)
