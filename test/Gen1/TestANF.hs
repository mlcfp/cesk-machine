--------------------------------------------------------------------------------
-- Copyright 2024 Michael P Williams. All rights reserved.
--------------------------------------------------------------------------------

module Gen1.TestANF
  ( tests
  ) where

import Gen1.ANF
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (assertEqual)
import Text.RawString.QQ

tests :: Test
tests = testGroup "ANF"
  [ testParseInt
  , testParseBool
  , testParseStr
  , testParseFloat
  , testParseVar
  , testParsePrim
  , testParseLam
  , testParseLet
  , testProgFactorial
  , testRender
  ]

testParseInt :: Test
testParseInt = testCase "int" $ do
  assertEqual "int small"
    (Right $ expProg $ ANFExpAtomic $ ANFAtomicInt 6)
    (anfParse "6")
  assertEqual "int large"
    (Right $ expProg $ ANFExpAtomic $ ANFAtomicInt 1234567890)
    (anfParse "1234567890")

testParseBool :: Test
testParseBool = testCase "bool" $ do
  assertEqual "bool true"
    (Right $ expProg $ ANFExpAtomic $ ANFAtomicBool True)
    (anfParse "#t")
  assertEqual "bool false"
    (Right $ expProg $ ANFExpAtomic $ ANFAtomicBool False)
    (anfParse "#f")

testParseStr :: Test
testParseStr = testCase "str" $ do
  assertEqual "str 1"
    (Right $ expProg $ ANFExpAtomic $ ANFAtomicStr "a")
    (anfParse "\"a\"")

testParseFloat :: Test
testParseFloat = testCase "float" $ do
  assertEqual "float 1"
    (Right $ expProg $ ANFExpAtomic $ ANFAtomicFloat 0.12)
    (anfParse "0.12")

testParseVar :: Test
testParseVar = testCase "var" $ do
  assertEqual "var letter"
    (Right $ expProg $ ANFExpAtomic $ ANFAtomicVar $ ANFVar "x")
    (anfParse "x")
  assertEqual "var alphanum"
    (Right $ expProg $ ANFExpAtomic $ ANFAtomicVar $ ANFVar "x01")
    (anfParse "x01")

testParsePrim :: Test
testParsePrim = testCase "prim" $ do
  assertEqual "prim add"
    (Right $ expProg $ ANFExpAtomic $ ANFAtomicPrim
      ANFPrimAdd [ANFAtomicInt 1, ANFAtomicInt 2])
    (anfParse "(+ 1 2)")
  assertEqual "prim sub"
    (Right $ expProg $ ANFExpAtomic $ ANFAtomicPrim
      ANFPrimSub [ANFAtomicInt 44, ANFAtomicInt 3])
    (anfParse "(- 44 3)")
  assertEqual "prim mul"
    (Right $ expProg $ ANFExpAtomic $ ANFAtomicPrim
      ANFPrimMul [ANFAtomicInt 66, ANFAtomicInt 88])
    (anfParse "(* 66 88)")
  assertEqual "prim div"
    (Right $ expProg $ ANFExpAtomic $ ANFAtomicPrim
      ANFPrimDiv [ANFAtomicInt 0, ANFAtomicInt 2])
    (anfParse "(/ 0 2)")
  assertEqual "prim eq"
    (Right $ expProg $ ANFExpAtomic $ ANFAtomicPrim
      ANFPrimEq [ANFAtomicInt 0, ANFAtomicInt 2])
    (anfParse "(= 0 2)")

testParseLam :: Test
testParseLam = testCase "lam" $ do
  assertEqual "lam basic"
    (Right $ expProg $ ANFExpAtomic $ ANFAtomicLam
      (ANFLam [ANFVar "n"] (ANFExpAtomic $ ANFAtomicInt 0)))
    (anfParse "(λ (n) 0)")
  assertEqual "lam multivar"
    (Right $ expProg $ ANFExpAtomic $ ANFAtomicLam
      (ANFLam [ANFVar "n", ANFVar "x", ANFVar "y", ANFVar "ggg"]
      (ANFExpAtomic $ ANFAtomicBool False)))
    (anfParse "(λ (n x y ggg) #f)")

testParseLet :: Test
testParseLet = testCase "let" $ do
  assertEqual "prim add"
    (Right $ expProg $ (ANFExpLet (ANFVar "x") (ANFExpAtomic (ANFAtomicInt 1))
      (ANFExpAtomic (ANFAtomicVar (ANFVar "x")))))
    (anfParse "(let ((x 1)) x)")

testProgFactorial :: Test
testProgFactorial = testCase "factorial" $ do
  assertEqual "program"
    (Right $ ANFProg
      [ ANFDecDefine (ANFVar "f") $ ANFExpAtomic
        (ANFAtomicLam $ ANFLam [ANFVar "n"]
          (ANFExpLet (ANFVar "g1")
            (ANFExpAtomic $ ANFAtomicPrim ANFPrimEq
              [ ANFAtomicVar (ANFVar "n")
              , ANFAtomicInt 0
              ])
            (ANFExpComplex $ ANFComplexIf (ANFAtomicVar $ ANFVar "g1")
              (ANFExpAtomic $ ANFAtomicInt 1)
              (ANFExpLet (ANFVar "g2")
                (ANFExpAtomic $ ANFAtomicPrim ANFPrimSub
                  [ ANFAtomicVar (ANFVar "n")
                  , ANFAtomicInt 1
                  ])
                (ANFExpLet (ANFVar "g3")
                  (ANFExpComplex $ ANFComplexApp
                    [ ANFAtomicVar (ANFVar "f")
                    , ANFAtomicVar (ANFVar "g2")
                    ])
                  (ANFExpAtomic $ ANFAtomicPrim ANFPrimMul
                    [ ANFAtomicVar (ANFVar "n")
                    , ANFAtomicVar (ANFVar "g3")
                    ]))))))
      , ANFDecExp (ANFExpComplex $ ANFComplexApp
          [ ANFAtomicVar (ANFVar "f")
          , ANFAtomicInt 20
          ])
      ])
    $ factorialProg

testRender :: Test
testRender = testCase "render" $ do
  Right prog <- pure factorialProg
  p1 <- anfRender anfRenderOptions prog
  p0 <- pure "(define f (λ (n) \
    \(let ((g1 (= n 0))) \
    \(if g1 1 (let ((g2 (- n 1))) \
    \(let ((g3 (f g2))) (* n g3))))))) (f 20)"
  assertEqual "render" p0 p1
  p1p <- anfRender anfRenderOptions { anfRenderOptionStyle = ANFRenderPretty } prog
  assertEqual "render"
    "(define f (λ (n)\n\
    \  (let ((g1 (= n 0)))\n\
    \    (if g1\n\
    \      1\n\
    \      (let ((g2 (- n 1)))\n\
    \        (let ((g3 (f g2)))\n\
    \          (* n g3)))))))\n\
    \(f 20)"
    p1p

expProg :: ANFExp -> ANFProg
expProg exp = ANFProg [ANFDecExp exp]

factorialProg = anfParse [r|
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
