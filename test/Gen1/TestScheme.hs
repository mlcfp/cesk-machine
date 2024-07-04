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
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (assertEqual)
import Text.RawString.QQ

tests :: Test
tests = testGroup "Gen1.Scheme"
  [ testParseFactorial
  , testPrintFactorial
  , testRunFactorial
  ]

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
  p0 <- schemeRender renderOptions { renderOptionStyle = RenderPretty } prog
  assertEqual "pretty" factorialPretty p0
  p1 <- schemeRender renderOptions { renderOptionStyle = RenderNormal } prog
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
  -- schemeExec factorialScheme >>= assertEqual "exec" r

factorialScheme = [r|
  (define (factorial n)
    (if (= n 0) 1
        (* n (factorial (- n 1)))))
  (factorial 20)
|]

schemeRun code = do
  case schemeParse code of
    Left err -> do
      error $ T.unpack err
    Right prog -> do
      prog' <- normalizeProg prog
      code' <- schemeRender renderOptions prog'
      ceskRun code'

-- TODO the scheme AST to ANF AST conversion is needed for this
-- schemeExec code = do
--   case schemeParse code of
--     Left err -> do
--       error $ T.unpack err
--     Right prog -> do
--       prog' <- normalizeProg prog
--       ceskExec prog'