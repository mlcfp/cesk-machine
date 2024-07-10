--------------------------------------------------------------------------------
-- Copyright 2024 Michael P Williams. All rights reserved.
--------------------------------------------------------------------------------

module Gen1.TestCESK
  ( tests
  ) where

import qualified Data.Text as T
import Gen1.CESK
import Gen1.Normalize
import Gen1.Scheme
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (assertEqual)
import Text.RawString.QQ

tests :: Test
tests = testGroup "CESK"
  [ testVal
  , testVar
  , testMath
  , testIntrinsic
  , testLogical
  , testChar
  , testDefine
  , testANFFactorial
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
  ceskRun "(+ 6.6 78.8)" >>=
    assertEqual "add float" (Right $ CESKValFloat $ 6.6 + 78.8)
  ceskRun "(* 6 7.2)" >>=
    assertEqual "mul float" (Right $ CESKValFloat $ 6 * 7.2)
  ceskRun "(/ 6 7.0)" >>=
    assertEqual "div float" (Right $ CESKValFloat $ 6 / 7.0)
  ceskRun "(* (+ 1 (- 3.0 2)) 3)" >>=
    assertEqual "complex 2" (Right $ CESKValFloat $ ((3 - 2) + 1) * 3)

testIntrinsic :: Test
testIntrinsic = testCase "intrinsic" $ do
  ceskRun "(@sin 0.0)" >>=
    assertEqual "sin" (Right $ CESKValFloat 0.0)
  ceskRun "(@cos 0.0)" >>=
    assertEqual "cos" (Right $ CESKValFloat 1.0)
  ceskRun "(@tan 0.0)" >>=
    assertEqual "tan" (Right $ CESKValFloat 0.0)
  ceskRun "(@pi)" >>=
    assertEqual "pi" (Right $ CESKValFloat pi)
  ceskRun "(@string-length \"abc123\")" >>=
    assertEqual "strlen" (Right $ CESKValInt 6)
  ceskRun "(@string-char \"xyz\" 2)" >>=
    assertEqual "strchar" (Right $ CESKValChar 'z')
  ceskRun "(void? #void)" >>=
    assertEqual "void 1" (Right $ CESKValBool True)
  ceskRun "(void? #\\a)" >>=
    assertEqual "void 2" (Right $ CESKValBool False)
  ceskRun [r|(char? #\a)|] >>=
    assertEqual "char" (Right $ CESKValBool True)

testLogical :: Test
testLogical = testCase "logical" $ do
  ceskRun "(> 1 2)" >>=
    assertEqual "gt" (Right $ CESKValBool False)
  ceskRun "(< 1 2)" >>=
    assertEqual "lt" (Right $ CESKValBool True)
  ceskRun "(>= 1 2)" >>=
    assertEqual "ge" (Right $ CESKValBool False)
  ceskRun "(<= 1 2)" >>=
    assertEqual "le" (Right $ CESKValBool True)
  ceskRun "(= 1 2)" >>=
    assertEqual "le" (Right $ CESKValBool False)
  ceskRun "(/= 1 2)" >>=
    assertEqual "le" (Right $ CESKValBool True)

testChar :: Test
testChar = testCase "char" $ do
  ceskRun "(= #\\a #\\b)" >>=
    assertEqual "eq" (Right $ CESKValBool False)
  ceskRun "(= #\\@ #\\@)" >>=
    assertEqual "eq" (Right $ CESKValBool True)

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

testANFFactorial :: Test
testANFFactorial = testCase "anf factorial" $ do
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
    (f 20))
  |] >>=
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
  ceskRun [r|
    (define f (λ (n)
      (let ((g0 (= n 0)))
        (if g0
          1
          (let ((g1 (- n 1)))
            (let ((g2 (f g1)))
              (* n g2)))))))
    (f 11)
  |] >>=
    assertEqual "ver 2" (Right $ CESKValInt 39916800)
