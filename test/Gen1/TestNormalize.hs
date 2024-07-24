--------------------------------------------------------------------------------
-- Copyright 2024 Michael P Williams. All rights reserved.
--------------------------------------------------------------------------------

module Gen1.TestNormalize
  ( tests
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Gen1.Normalize
import Gen1.Scheme
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (assertEqual)
import Text.RawString.QQ
import Text.Show.Pretty (pPrint)

tests :: Test
tests = testGroup "Gen1.Normalize"
  [ testExp1
  , testExp2
  , testExp3
  , testExp4
  , testExp5
  , testProg1
  , testProg2
  , testFactorial
  ]

testExp1 :: Test
testExp1 = testCase "exp1" $ do
  normalizePretty exp1 >>= assertEqual "exp1" exp1'

exp1 = [r|
(λ (z)
    (λ (x)
      (set! z (f x))))
|]

exp1' = [r|(λ (z)
  (λ (x)
    (let ((g0 (f x)))
      (let ((_1 (set! z g0)))
        (void)))))|]

testExp2 :: Test
testExp2 = testCase "exp2" $ do
  normalizePretty exp2 >>= assertEqual "exp2" exp2'

exp2 = [r|(+ (if (f x) 0 1) 2)|]

exp2' = [r|(let ((g0 (f x)))
  (let ((g1 (if g0
    0
    1)))
    (+ g1 2)))|]

testExp3 :: Test
testExp3 = testCase "exp3" $ do
  normalizePretty exp3 >>= assertEqual "exp3" exp3'

exp3 = [r|
(let ((id (λ (x) x)))
  (let ((apply (λ (f x) (f x))))
    ((id apply) (id 3))))
|]

exp3' = [r|(let ((id (λ (x)
  x)))
  (let ((apply (λ (f x)
    (f x))))
    (let ((g0 (id apply)))
      (let ((g1 (id 3)))
        (g0 g1)))))|]

testExp4 :: Test
testExp4 = testCase "exp4" $ do
  normalizePretty exp4 >>= assertEqual "exp4" exp4'

exp4 = [r|
(let ((id (λ (x) x))
      (apply (λ (f x) (f x))))
  ((id apply) (id 3)))
|]

exp4' = [r|(let ((id (λ (x)
  x)))
  (let ((apply (λ (f x)
    (f x))))
    (let ((g0 (id apply)))
      (let ((g1 (id 3)))
        (g0 g1)))))|]

testExp5 :: Test
testExp5 = testCase "exp5" $ do
  normalizePretty exp5 >>= assertEqual "exp5" exp5'

exp5 = [r|((f g) (h 3) #f)|]

exp5' = [r|(let ((g0 (f g)))
  (let ((g1 (h 3)))
    (g0 g1 #f)))|]

testProg1 :: Test
testProg1 = testCase "prog1" $ do
  normalizePretty prog1 >>= assertEqual "prog1" prog1'

prog1 = [r|
(define (f x) (h x))
(define (h x) (+ 1 x))
(f 20)
|]

prog1' = [r|(define f (λ (x)
  (h x)))
(define h (λ (x)
  (+ 1 x)))
(f 20)|]

testProg2 :: Test
testProg2 = testCase "prog2" $ do
  normalizePretty prog2 >>= assertEqual "prog2" prog2'

prog2 = [r|(define v ((f g) (h 3) #f))|]

prog2' = [r|(begin
  (define g0 (f g))
  (define g1 (h 3))
  (define v (g0 g1 #f)))|]

testFactorial :: Test
testFactorial = testCase "normalize factorial" $ do
  p <- normalizePretty factorialProgram
  assertEqual "factorial" factorialNormal p

factorialProgram = [r|
  (define (f n)
    (if (= n 0)
        1
        (* n (f (- n 1)))))
  (f 20)
  |]

factorialNormal = [r|(define f (λ (n)
  (let ((g0 (= n 0)))
    (if g0
      1
      (let ((g1 (- n 1)))
        (let ((g2 (f g1)))
          (* n g2)))))))
(f 20)|]

normalizePretty :: Text -> IO Text
normalizePretty p = do
  let Right ast = schemeParse p
  norm <- normalizeProg ast
  schemeRender schemeRenderOptions
    { schemeRenderOptionStyle = SchemeRenderPretty }
    norm