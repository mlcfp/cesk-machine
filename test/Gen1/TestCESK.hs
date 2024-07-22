--------------------------------------------------------------------------------
-- Copyright 2024 Michael P Williams. All rights reserved.
--------------------------------------------------------------------------------

module Gen1.TestCESK
  ( tests
  ) where

import Data.Text (Text)
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

run :: Text -> IO (Either CESKError CESKVal)
run = ceskRun'

runBind :: Text -> IO (Either CESKError CESKVal)
runBind = ceskRun ceskDefaultOptions { ceskOptionIntrinsicBindings = True }

testVal :: Test
testVal = testCase "val" $ do
  run "0" >>=
    assertEqual "int" (Right $ CESKValInt 0)
  run "#t" >>=
    assertEqual "true" (Right $ CESKValBool True)
  run "#f" >>=
    assertEqual "false" (Right $ CESKValBool False)

testVar :: Test
testVar = testCase "var" $ do
  run "(letrec (( y  1 )) y )" >>=
    assertEqual "var 1" (Right $ CESKValInt 1)
  run "(letrec((y #t))y)" >>=
    assertEqual "var 2" (Right $ CESKValBool True)
  run "(let ((y 1)) y)" >>=
    assertEqual "var 3" (Right $ CESKValInt 1)
  run "(let ((y 8)) (* 2 y))" >>=
    assertEqual "var 4" (Right $ CESKValInt 16)

testMath :: Test
testMath = testCase "math" $ do
  run "(+ 66 788)" >>=
    assertEqual "add" (Right $ CESKValInt 854)
  run "(- 66 788)" >>=
    assertEqual "sub" (Right $ CESKValInt (-722))
  run "(* 6 6)" >>=
    assertEqual "mul" (Right $ CESKValInt 36)
  run "(/ 6 3)" >>=
    assertEqual "div" (Right $ CESKValInt 2)
  run "(/ 6 7)" >>=
    assertEqual "div" (Right $ CESKValInt 0)
  run "(* (+ 1 (- 3 2)) 3)" >>=
    assertEqual "complex 1" (Right $ CESKValInt 6)
  run "(+ 6.6 78.8)" >>=
    assertEqual "add float" (Right $ CESKValFloat $ 6.6 + 78.8)
  run "(* 6 7.2)" >>=
    assertEqual "mul float" (Right $ CESKValFloat $ 6 * 7.2)
  run "(/ 6 7.0)" >>=
    assertEqual "div float" (Right $ CESKValFloat $ 6 / 7.0)
  run "(* (+ 1 (- 3.0 2)) 3)" >>=
    assertEqual "complex 2" (Right $ CESKValFloat $ ((3 - 2) + 1) * 3)

testIntrinsic :: Test
testIntrinsic = testCase "intrinsic" $ do
  run "(@sin 0.0)" >>=
    assertEqual "sin" (Right $ CESKValFloat 0.0)
  run "(@cos 0.0)" >>=
    assertEqual "cos" (Right $ CESKValFloat 1.0)
  run "(@tan 0.0)" >>=
    assertEqual "tan" (Right $ CESKValFloat 0.0)
  run "(@pi)" >>=
    assertEqual "pi" (Right $ CESKValFloat pi)
  run "(@string-length \"abc123\")" >>=
    assertEqual "strlen" (Right $ CESKValInt 6)
  run "(@string-char \"xyz\" 2)" >>=
    assertEqual "strchar" (Right $ CESKValChar 'z')
  runBind "(void? #void)" >>=
    assertEqual "void 1" (Right $ CESKValBool True)
  runBind "(void? #\\a)" >>=
    assertEqual "void 2" (Right $ CESKValBool False)
  runBind [r|(char? #\a)|] >>=
    assertEqual "char" (Right $ CESKValBool True)

testLogical :: Test
testLogical = testCase "logical" $ do
  run "(> 1 2)" >>=
    assertEqual "gt" (Right $ CESKValBool False)
  run "(< 1 2)" >>=
    assertEqual "lt" (Right $ CESKValBool True)
  run "(>= 1 2)" >>=
    assertEqual "ge" (Right $ CESKValBool False)
  run "(<= 1 2)" >>=
    assertEqual "le" (Right $ CESKValBool True)
  run "(= 1 2)" >>=
    assertEqual "le" (Right $ CESKValBool False)
  run "(/= 1 2)" >>=
    assertEqual "le" (Right $ CESKValBool True)

testChar :: Test
testChar = testCase "char" $ do
  run "(= #\\a #\\b)" >>=
    assertEqual "eq" (Right $ CESKValBool False)
  run "(= #\\@ #\\@)" >>=
    assertEqual "eq" (Right $ CESKValBool True)

testDefine :: Test
testDefine = testCase "define" $ do
  run "(define y 1) y" >>=
    assertEqual "define 1" (Right $ CESKValInt 1)
  run "(define y (λ (n) (* 2 n))) (y 4)" >>=
    assertEqual "define 2" (Right $ CESKValInt 8)
  run [r|
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
  run [r|
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
  run [r|
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
  run [r|
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
