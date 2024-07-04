--------------------------------------------------------------------------------
-- Copyright 2024 Michael P Williams. All rights reserved.
--------------------------------------------------------------------------------

module Gen1.TestGC
  ( tests
  ) where

import Gen1.ANF
import Gen1.CESK
import qualified Data.Map as Map
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (assertEqual)

tests :: Test
tests = testGroup "GC"
  [ testOneEnv
  , testOneCont
  -- , testContCycle
  -- , testClosCycle
  ]

a0 :: CESKAddr
a0 = CESKAddr 0

testOneEnv :: Test
testOneEnv = testCase "one env" $ do
  Right (addr, state, stateEvac, state') <- ceskDo $ do
    (store, addr) <- storeAlloc storeEmpty CESKValVoid CESKStoreWhite
    let exp = ANFExpAtomic $ ANFAtomicBool False
    let env = Map.fromList [(ANFVar "x", addr)]
    let state = CESKState exp env store CESKHalt
    (stateEvac, state') <- ceskGarbageCollect state
    pure (addr, state, stateEvac, state')
  assertEqual "addr" a0 addr
  assertEqual "store orig"
    (Map.fromList [(a0, CESKStoreVal CESKStoreWhite CESKValVoid)])
    (stateSpace state)
  assertEqual "store new"
    (Map.fromList [(a0, CESKStoreVal CESKStoreWhite CESKValVoid)])
    (stateSpace state')
  assertEqual "store old"
    (Map.fromList [(a0, CESKStoreForward a0)])
    (stateSpace stateEvac)

testOneCont :: Test
testOneCont = testCase "one cont" $ do
  let val = CESKValInt 6
  Right (addr, state, stateEvac, state') <- ceskDo $ do
    (store, addr) <- storeAlloc storeEmpty val CESKStoreWhite
    let exp = ANFExpAtomic $ ANFAtomicBool False
    let env = Map.fromList [(ANFVar "x", addr)]
    cont <- pure $ CESKCont (ANFVar "y")
      (ANFExpAtomic $ ANFAtomicInt 2) env CESKHalt
    let state = CESKState exp env store cont
    (stateEvac, state') <- ceskGarbageCollect state
    pure (addr, state, stateEvac, state')
  assertEqual "addr" a0 addr
  assertEqual "store orig"
    (Map.fromList [(a0, CESKStoreVal CESKStoreWhite val)])
    (stateSpace state)
  assertEqual "store new"
    (Map.fromList [(a0, CESKStoreVal CESKStoreWhite val)])
    (stateSpace state')
  assertEqual "store old"
    (Map.fromList [(a0, CESKStoreForward a0)])
    (stateSpace stateEvac)



-- testContCycle :: Test
-- testContCycle = testCase "cont cycle" $ do
--   assertEqual "store orig"
--     (Map.fromList
--       [ (Addr 1, StoreVal StoreWhite
--           (ValCont (Cont (Var "y0") (ExpAtomic (AExpInt 2))
--           (Map.fromList [(Var "x0", Addr 3)]) Halt)))
--       , (Addr 2, StoreVal StoreWhite
--           (ValCont (Cont (Var "y1") (ExpAtomic (AExpInt 2))
--           (Map.fromList [(Var "x1", Addr 1)]) Halt)))
--       , (Addr 3, StoreVal StoreWhite
--           (ValCont (Cont (Var "y2") (ExpAtomic (AExpInt 2))
--           (Map.fromList [(Var "x2", Addr 2)]) Halt)))
--       ])
--     (stateSpace state)
--   assertEqual "store new"
--     (Map.fromList
--       [ (Addr 1, StoreVal StoreWhite
--           (ValCont (Cont (Var "y2") (ExpAtomic (AExpInt 2))
--           (Map.fromList [(Var "x2", Addr 2)]) Halt)))
--       , (Addr 2, StoreVal StoreWhite
--           (ValCont (Cont (Var "y1") (ExpAtomic (AExpInt 2))
--           (Map.fromList [(Var "x1", Addr 3)]) Halt)))
--       , (Addr 3, StoreVal StoreWhite
--           (ValCont (Cont (Var "y0") (ExpAtomic (AExpInt 2))
--           (Map.fromList [(Var "x0", Addr 1)]) Halt)))
--       ])
--     (stateSpace state')
--   assertEqual "store old"
--     (Map.fromList
--       [ (Addr 1, StoreForward (Addr 3))
--       , (Addr 2, StoreForward (Addr 2))
--       , (Addr 3, StoreForward (Addr 1))
--       ])
--     (stateSpace stateEvac)
--   where
--     state = State (ExpAtomic AExpFalse) env store2 Halt
--     (stateEvac, state') = garbageCollect state
--     env = Map.fromList [(Var "x", addr2)]
--     env0 = Map.fromList [(Var "x0", addr2)]
--     env1 = Map.fromList [(Var "x1", addr0)]
--     env2 = Map.fromList [(Var "x2", addr1)]
--     cont0 = Cont (Var "y0") (ExpAtomic $ AExpInt 2) env0 Halt
--     cont1 = Cont (Var "y1") (ExpAtomic $ AExpInt 2) env1 Halt
--     cont2 = Cont (Var "y2") (ExpAtomic $ AExpInt 2) env2 Halt
--     (store0, addr0) = storeAlloc storeEmpty (ValCont cont0) StoreWhite
--     (store1, addr1) = storeAlloc store0 (ValCont cont1) StoreWhite
--     (store2, addr2) = storeAlloc store1 (ValCont cont2) StoreWhite

-- testClosCycle :: Test
-- testClosCycle = testCase "clos cycle" $ do
--   assertEqual "store orig"
--     (Map.fromList
--       [ (Addr 1, StoreVal StoreWhite
--           (ValClos (Lam [Var "a0"] (ExpAtomic (AExpInt 8)))
--           (Map.fromList [(Var "x0", Addr 3)])))
--       , (Addr 2, StoreVal StoreWhite
--           (ValClos (Lam [Var "a1"] (ExpAtomic (AExpInt 8)))
--           (Map.fromList [(Var "x1", Addr 1)])))
--       , (Addr 3, StoreVal StoreWhite
--           (ValClos (Lam [Var "a2"] (ExpAtomic (AExpInt 8)))
--           (Map.fromList [(Var "x2", Addr 2)])))
--       ])
--     (stateSpace state)
--   assertEqual "store new"
--     (Map.fromList
--       [ (Addr 1, StoreVal StoreWhite
--           (ValClos (Lam [Var "a2"] (ExpAtomic (AExpInt 8)))
--           (Map.fromList [(Var "x2", Addr 2)])))
--       , (Addr 2, StoreVal StoreWhite
--           (ValClos (Lam [Var "a1"] (ExpAtomic (AExpInt 8)))
--           (Map.fromList [(Var "x1", Addr 3)])))
--       , (Addr 3, StoreVal StoreWhite
--           (ValClos (Lam [Var "a0"] (ExpAtomic (AExpInt 8)))
--           (Map.fromList [(Var "x0", Addr 1)])))
--       ])
--     (stateSpace state')
--   assertEqual "store old"
--     (Map.fromList
--       [ (Addr 1, StoreForward (Addr 3))
--       , (Addr 2, StoreForward (Addr 2))
--       , (Addr 3, StoreForward (Addr 1))
--       ])
--     (stateSpace stateEvac)
--   where
--     state = State (ExpAtomic AExpFalse) env store2 Halt
--     (stateEvac, state') = garbageCollect state
--     env = Map.fromList [(Var "x", addr2)]
--     env0 = Map.fromList [(Var "x0", addr2)]
--     env1 = Map.fromList [(Var "x1", addr0)]
--     env2 = Map.fromList [(Var "x2", addr1)]
--     clos0 = ValClos (Lam [Var "a0"] $ ExpAtomic $ AExpInt 8) env0
--     clos1 = ValClos (Lam [Var "a1"] $ ExpAtomic $ AExpInt 8) env1
--     clos2 = ValClos (Lam [Var "a2"] $ ExpAtomic $ AExpInt 8) env2
--     (store0, addr0) = storeAlloc storeEmpty clos0 StoreWhite
--     (store1, addr1) = storeAlloc store0 clos1 StoreWhite
--     (store2, addr2) = storeAlloc store1 clos2 StoreWhite
