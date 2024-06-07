--------------------------------------------------------------------------------
-- Copyright 2024 Michael P Williams. All rights reserved.
--------------------------------------------------------------------------------

module Main
  ( main
  ) where

import ANF
import qualified CESK as CESK
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import Text.RawString.QQ

main :: IO ()
main = do
  t0 <- getCurrentTime
  putStrLn $ show t0
  -- let !e = CESK.run pFactoral
  -- t1 <- getCurrentTime
  -- putStrLn $ show e
  -- putStrLn $ show t0
  -- putStrLn $ show t1
  -- putStrLn $ show $ diffUTCTime t1 t0

  -- let a = parseANF "(let ((x 1)) x)"
  -- let a = parseANF "(λ (x y) #f)"
  -- let a = parseANF "(let (( y  1 )) x )"
  -- let a = parseANF "(let (( y  1 )) (λ (x y) #f) )"
  -- let a = parseANF "(let (( y  1 )) (let (( x  2 )) (* 2 3) ))"
  -- let a = parseANF "(let (( y  1 )) (call/cc 2))"
  -- let a = parseANF "(letrec (( y  1 )) x )"
  -- let a = parseANF s1
  -- putStrLn $ show a

  -- runTest "(letrec (( y  1 )) x )"
  runTest s1

p0 = Prog (ExpAtomic $ AExpInt 6)

p1 = Prog
  (ExpLet (Var "x") (ExpAtomic $ AExpTrue)
    (ExpAtomic $ AExpVar $ Var "x"))

p2 = Prog
  (ExpLet (Var "x") (ExpAtomic $ AExpInt 1)
    (ExpLet (Var "y") (ExpAtomic $ AExpInt 2)
      (ExpAtomic $ AExpPrim PrimAdd
        [ AExpVar $ Var "x"
        , AExpVar $ Var "y"
        ])))

p3 = Prog
  (ExpLet (Var "x") (ExpAtomic $ AExpInt 1)
    (ExpLet (Var "y") (ExpAtomic $ AExpInt 2)
      (ExpAtomic $ AExpPrim PrimAdd
        [ AExpInt 3
        , AExpInt 4
        ])))

p4 = Prog
  (ExpAtomic $ AExpPrim PrimEq
    [ AExpInt 1
    , AExpInt 1
    ])


--  (define (f n)
--        (if (= n 0)
--            1
--            (* n (f (- n 1)))))

--   (f 20)

--   =>

--   (define f
--     (λ (n)
--       (let ((g1478 (= n 0)))
--         (if g1478
--           1
--           (let ((g1479 (- n 1)))
--             (let ((g1480 (f g1479)))
--               (* n g1480)))))))
--   (f 20)

pFactoral = Prog
  (ExpComplex $ CExpLetRec [(Var "f",
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
    )]
    (ExpComplex $ CExpApp
      [ AExpVar (Var "f")
      , AExpInt 20
      ]))


s0 = [r|
  (let ((x 1))
    x)
|]

s1 = [r|(letrec ((f
    (λ (n)
      (let ((g1 (= n 0)))
        (if g1
          1
          (let ((g2 (- n 1)))
            (let ((g3 (f g2)))
              (* n g3))))))
  #|
              |#
    ))
    (f 20))|]
