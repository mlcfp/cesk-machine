--------------------------------------------------------------------------------
-- Copyright 2024 Michael P Williams. All rights reserved.
--------------------------------------------------------------------------------

module TestANF
  ( tests
  ) where

import ANF
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit
  ( assertBool
  , assertEqual
  , assertFailure
  )
import Text.RawString.QQ

tests :: Test
tests = testGroup "ANF"
  [ testParseInt
  , testParseBool
  , testParseVar
  , testParsePrim
  , testParseLam
  , testProgFactorial
  ]

testParseInt :: Test
testParseInt = testCase "int" $ do
  assertEqual "int small"
    (Right $ ExpAtomic $ AExpInt 6)
    (anfExp "6")
  assertEqual "int large"
    (Right $ ExpAtomic $ AExpInt 1234567890)
    (anfExp "1234567890")

testParseBool :: Test
testParseBool = testCase "bool" $ do
  assertEqual "bool true"
    (Right $ ExpAtomic AExpTrue)
    (anfExp "#t")
  assertEqual "bool false"
    (Right $ ExpAtomic AExpFalse)
    (anfExp "#f")

testParseVar :: Test
testParseVar = testCase "var" $ do
  assertEqual "var letter"
    (Right $ ExpAtomic $ AExpVar $ Var "x")
    (anfExp "x")
  assertEqual "var alphanum"
    (Right $ ExpAtomic $ AExpVar $ Var "x01")
    (anfExp "x01")
  -- assertEqual "var complex"
  --   (Right $ ExpAtomic $ AExpVar $ Var "yy2_89nn")
  --   (anfExp "yy2_89nn")

testParsePrim :: Test
testParsePrim = testCase "prim" $ do
  assertEqual "prim add"
    (Right $ ExpAtomic $ AExpPrim PrimAdd [AExpInt 1, AExpInt 2])
    (anfExp "(+ 1 2)")
  assertEqual "prim sub"
    (Right $ ExpAtomic $ AExpPrim PrimSub [AExpInt 44, AExpInt 3])
    (anfExp "(- 44 3)")
  assertEqual "prim mul"
    (Right $ ExpAtomic $ AExpPrim PrimMul [AExpInt 66, AExpInt 88])
    (anfExp "(* 66 88)")
  assertEqual "prim div"
    (Right $ ExpAtomic $ AExpPrim PrimDiv [AExpInt 0, AExpInt 2])
    (anfExp "(/ 0 2)")
  assertEqual "prim eq"
    (Right $ ExpAtomic $ AExpPrim PrimEq [AExpInt 0, AExpInt 2])
    (anfExp "(= 0 2)")

testParseLam :: Test
testParseLam = testCase "lam" $ do
  assertEqual "lam basic"
    (Right $ ExpAtomic $ AExpLam (Lam [Var "n"] (ExpAtomic $ AExpInt 0)))
    (anfExp "(λ (n) 0)")
  assertEqual "lam multivar"
    (Right $ ExpAtomic $ AExpLam (Lam [Var "n", Var "x", Var "y", Var "ggg"]
      (ExpAtomic AExpFalse)))
    (anfExp "(λ (n x y ggg) #f)")

testProgFactorial :: Test
testProgFactorial = testCase "factorial" $ do
  assertEqual "program"
    (Right $ Prog
      [ DecDefine (Var "f") $ ExpAtomic
        (AExpLam $ Lam [Var "n"]
          (ExpLet (Var "g1")
            (ExpAtomic $ AExpPrim PrimEq
              [ AExpVar (Var "n")
              , AExpInt 0
              ])
            (ExpComplex $ CExpIf (AExpVar $ Var "g1")
              (ExpAtomic $ AExpInt 1)
              (ExpLet (Var "g2")
                (ExpAtomic $ AExpPrim PrimSub
                  [ AExpVar (Var "n")
                  , AExpInt 1
                  ])
                (ExpLet (Var "g3")
                  (ExpComplex $ CExpApp
                    [ AExpVar (Var "f")
                    , AExpVar (Var "g2")
                    ])
                  (ExpAtomic $ AExpPrim PrimMul
                    [ AExpVar (Var "n")
                    , AExpVar (Var "g3")
                    ]))))))
      , DecExp (ExpComplex $ CExpApp
          [ AExpVar (Var "f")
          , AExpInt 20
          ])
      ])
    $ anfProg [r|
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
