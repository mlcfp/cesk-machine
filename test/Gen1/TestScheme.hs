--------------------------------------------------------------------------------
-- Copyright 2024 Michael P Williams. All rights reserved.
--------------------------------------------------------------------------------

module Gen1.TestScheme
  ( tests
  ) where

import qualified Data.Text as T
import Gen1.CESK
import Gen1.Normalize
import Gen1.Scheme
import Gen1.Sugar
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (assertEqual)
import Text.RawString.QQ

tests :: Test
tests = testGroup "Gen1.Scheme"
  [ testParseDefine
  , testParseFactorial
  , testPrintFactorial
  , testRunFactorial
  , testSequence
  , testDesugar
  ]

testParseDefine :: Test
testParseDefine = testCase "parse define" $ do
  assertEqual "define 1"
    (Right (SchemeProg
      [SchemeDecExp (SchemeExpApp
        [ SchemeExpVar (SchemeVar "+")
        , SchemeExpInt 1
        , SchemeExpVar (SchemeVar "x")
        ])
      ]))
    $ schemeParse [r|(+ 1 x)|]
  assertEqual "define 2"
    (Right (SchemeProg
      [ SchemeDecFunc (SchemeVar "h") [SchemeVar "x"]
        (SchemeExpApp
          [ SchemeExpVar (SchemeVar "+")
          , SchemeExpInt 1
          , SchemeExpVar (SchemeVar "x")
          ])
      , SchemeDecExp (SchemeExpApp
        [ SchemeExpVar (SchemeVar "-")
        , SchemeExpInt 1
        , SchemeExpApp
          [ SchemeExpVar (SchemeVar "h")
          , SchemeExpInt 2
          ]
        ])
      ]))
    $ schemeParse [r|(define (h x) (+ 1 x)) (- 1 (h 2))|]

testParseFactorial :: Test
testParseFactorial = testCase "parse factorial" $ do
  assertEqual "parse"
    (Right $ SchemeProg
      [ SchemeDecDefine (SchemeVar "f")
        (SchemeExpLam [SchemeVar "n"]
          (SchemeExpLet [SchemeBind (SchemeVar "g1")
            (SchemeExpApp
              [ SchemeExpVar (SchemeVar "=")
              , SchemeExpVar (SchemeVar "n")
              , SchemeExpInt 0
              ])]
            (SchemeExpIf (SchemeExpVar $ SchemeVar "g1")
              (SchemeExpInt 1)
              (SchemeExpLet [SchemeBind (SchemeVar "g2")
                (SchemeExpApp
                  [ SchemeExpVar (SchemeVar "-")
                  , SchemeExpVar (SchemeVar "n")
                  , SchemeExpInt 1
                  ])]
                (SchemeExpLet [SchemeBind (SchemeVar "g3")
                  (SchemeExpApp
                    [ SchemeExpVar (SchemeVar "f")
                    , SchemeExpVar (SchemeVar "g2")
                    ])]
                  (SchemeExpApp
                    [ SchemeExpVar (SchemeVar "*")
                    , SchemeExpVar (SchemeVar "n")
                    , SchemeExpVar (SchemeVar "g3")
                    ]))))))
      , SchemeDecExp (SchemeExpApp
          [ SchemeExpVar (SchemeVar "f")
          , SchemeExpInt 20
          ])
      ])
    $ schemeParse factorialProgram

testPrintFactorial :: Test
testPrintFactorial = testCase "print factorial" $ do
  let Right prog = schemeParse factorialProgram
  p0 <- schemeRender schemeRenderOptions
    { schemeRenderOptionStyle = SchemeRenderPretty } prog
  assertEqual "pretty" factorialPretty p0
  p1 <- schemeRender schemeRenderOptions
    { schemeRenderOptionStyle = SchemeRenderNormal } prog
  assertEqual "normal" factorialNormal p1

factorialProgram = [r|
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
  (f 20)
  |]

factorialPretty = [r|(define f (λ (n)
  (let ((g1 (= n 0)))
    (if g1
      1
      (let ((g2 (- n 1)))
        (let ((g3 (f g2)))
          (* n g3)))))))
(f 20)|]

factorialNormal = "\
  \(define f (λ (n) (let ((g1 (= n 0))) \
  \(if g1 1 (let ((g2 (- n 1))) (let ((g3 (f g2))) \
  \(* n g3))))))) (f 20)"

testRunFactorial :: Test
testRunFactorial = testCase "run factorial" $ do
  let r = Right $ CESKValInt 2432902008176640000
  schemeRun factorialScheme >>= assertEqual "run" r
  schemeExec factorialScheme >>= assertEqual "exec" r

factorialScheme = [r|
  (define (factorial n)
    (if (= n 0) 1
        (* n (factorial (- n 1)))))
  (factorial 20)
|]

schemeNorm code = do
  case schemeParse code of
    Left err -> do
      error $ T.unpack err
    Right prog -> do
      normalizeProg prog

schemeRun code = do
  prog' <- schemeNorm code
  code' <- schemeRender schemeRenderOptions prog'
  ceskRun ceskDefaultOptions code'

schemeExec code = do
  prog' <- schemeNorm code
  case schemeANF prog' of
    Left e2 ->
      error $ T.unpack e2
    Right p2 ->
      ceskExec' p2

testSequence :: Test
testSequence = testCase "sequence" $ do
  assertEqual "begin 1"
    (Right (SchemeProg [SchemeDecBegin [SchemeDecExp (SchemeExpInt 1)]]))
    $ schemeParse [r|(begin 1)|]

  assertEqual "begin 2"
    (Right (SchemeProg
      [ SchemeDecBegin
        [ SchemeDecExp (SchemeExpSet (SchemeVar "x") (SchemeExpInt 1))
        , SchemeDecExp (SchemeExpSet (SchemeVar "y") (SchemeExpInt 2))
        , SchemeDecExp (SchemeExpApp
          [ SchemeExpVar (SchemeVar "*")
          , SchemeExpVar (SchemeVar "y")
          , SchemeExpVar (SchemeVar "x")
          ])
        ]
      ]))
    $ schemeParse beginProg

  Right prog <- pure $ schemeParse beginProg
  schemeRender schemeRenderOptions
    { schemeRenderOptionStyle = SchemeRenderPretty } prog >>=
      assertEqual "begin 3" beginProgRender

beginProg = [r|
  (begin
    (set! x 1)
    (set! y 2)
    (* y x))
|]

beginProgRender = [r|(begin
  (set! x 1)
  (set! y 2)
  (* y x))|]

desugar p = do
  let Right p' = schemeParse p
  let Right s = desugarBegin p'
  schemeRender schemeRenderOptions s

desugarNorm p = do
  let Right p' = schemeParse p
  let Right s = desugarBegin p'
  p2 <- normalizeProg s
  schemeRender schemeRenderOptions p2

testDesugar :: Test
testDesugar = testCase "desugar" $ do
  desugar [r|
    (let ((_x (begin (+ 2 3) 1)))
      _x)
  |] >>= assertEqual "desugar 1"
    "(let ((_x ((λ (__r) 1) (+ 2 3)))) _x)"

  desugar [r|
    (let ((_x (begin (* 1 1) (+ 2 3) 1)))
      _x)
  |] >>= assertEqual "desugar 2"
    "(let ((_x ((λ (__r) ((λ (__r) 1) (+ 2 3))) (* 1 1)))) _x)"

  desugar [r|
    (let ((_x (begin (+ 2 3) 1)))
      (begin
        (set! _x 6)
        (set! _x 7)
        _x))
  |] >>= assertEqual "desugar 3"
    "(let ((_x ((λ (__r) 1) (+ 2 3)))) \
      \((λ (__r) ((λ (__r) _x) (set! _x 7))) (set! _x 6)))"

  a <- desugarNorm [r|
    (let ((x 0))
      (let ((y (set! x 6)))
        y))
  |]
  assertEqual "desugar 4"
    "(let ((x 0)) (let ((_0 (set! x 6))) (let ((y (void))) y)))" a
  schemeRun a >>= assertEqual "desugar 5" (Right CESKValVoid)

  desugar [r|
    (let ((_x (begin (+ 2 3) 1)))
      (begin
        (set! _x 6)
        (set! _x 7)
        _x))
  |] >>= schemeRun >>= assertEqual "desugar 6"
    (Right $ CESKValInt 7)

  desugar [r|
    (let ((_x (begin (+ 2 3) 1)))
      (begin
        (set! _x 6)
        (set! _x 7)))
  |] >>= schemeRun >>= assertEqual "desugar 7"
    (Right $ CESKValVoid)
