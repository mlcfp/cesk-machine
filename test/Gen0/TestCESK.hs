--------------------------------------------------------------------------------
-- Copyright 2024 Michael P Williams. All rights reserved.
--------------------------------------------------------------------------------

module Gen0.TestCESK
  ( tests
  ) where

import Gen0.CESK
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
  assertEqual "int" (Right $ ValInt 0) (run "0")
  assertEqual "true" (Right $ ValBool True) (run "#t")
  assertEqual "false" (Right $ ValBool False) (run "#f")

testVar :: Test
testVar = testCase "var" $ do
  assertEqual "var 1"
    (Right $ ValInt 1)
    (run "(letrec (( y  1 )) y )")
  assertEqual "var 2"
    (Right $ ValBool True)
    (run "(letrec((y #t))y)")
  assertEqual "var 3"
    (Right $ ValInt 1)
    (run "(let ((y 1)) y)")
  assertEqual "var 4"
    (Right $ ValInt 16)
    (run "(let ((y 8)) (* 2 y))")

testMath :: Test
testMath = testCase "math" $ do
  assertEqual "add"
    (Right $ ValInt 854)
    (run "(+ 66 788)")
  assertEqual "sub"
    (Right $ ValInt (-722))
    (run "(- 66 788)")
  assertEqual "mul"
    (Right $ ValInt 36)
    (run "(* 6 6)")
  assertEqual "div"
    (Right $ ValInt 2)
    (run "(/ 6 3)")
  assertEqual "div"
    (Right $ ValInt 0)
    (run "(/ 6 7)")
  assertEqual "complex 1"
    (Right $ ValInt 6)
    (run "(* (+ 1 (- 3 2)) 3)")

testDefine :: Test
testDefine = testCase "define" $ do
  assertEqual "define 1"
    (Right $ ValInt 1)
    (run "(define y 1) y")
  assertEqual "define 2"
    (Right $ ValInt 8)
    (run "(define y (λ (n) (* 2 n))) (y 4)")
  assertEqual "define 3"
    (Right $ ValInt 16)
    (run [r|
      (define y (λ (n) (* 2 n)))
      (define g (λ (x) (+ x 1)))
      (define sqr (λ (a) (* a a)))
      (let ((r0 (g 1)))
        (let ((r1 (y r0)))
          (sqr r1)))
    |])

testProgFactorial :: Test
testProgFactorial = testCase "factorial" $ do
  assertEqual "ver 1"
    (Right $ ValInt 2432902008176640000)
    (run [r|
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
      (f 20))|])
  assertEqual "ver 2"
    (Right $ ValInt 720)
    (run [r|
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
    |])