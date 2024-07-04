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
  , testContCycle
  , testClosCycle
  ]

a0 = CESKAddr 0
a1 = CESKAddr 1
a2 = CESKAddr 2
aInt = ANFExpAtomic . ANFAtomicInt
aBool = ANFExpAtomic . ANFAtomicBool

testOneEnv :: Test
testOneEnv = testCase "one env" $ do
  Right (addr, state, stateEvac, state') <- ceskDo $ do
    (store, addr) <- storeAlloc storeEmpty CESKValVoid CESKStoreWhite
    let exp = aBool False
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
    let exp = aBool False
    let env = Map.fromList [(ANFVar "x", addr)]
    let cont = CESKCont (ANFVar "y") (aInt 2) env CESKHalt
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

testContCycle :: Test
testContCycle = testCase "cont cycle" $ do
  Right (state, stateEvac, state') <- ceskDo $ do
    (store0, addr0) <- storeAlloc storeEmpty CESKValVoid CESKStoreWhite
    (store1, addr1) <- storeAlloc store0 CESKValVoid CESKStoreWhite
    (store2, addr2) <- storeAlloc store1 CESKValVoid CESKStoreWhite
    let env = Map.fromList [(ANFVar "x", addr2)]
    let env0 = Map.fromList [(ANFVar "x0", addr2)]
    let env1 = Map.fromList [(ANFVar "x1", addr0)]
    let env2 = Map.fromList [(ANFVar "x2", addr1)]
    let cont0 = CESKCont (ANFVar "y0") (aInt 2) env0 CESKHalt
    let cont1 = CESKCont (ANFVar "y1") (aInt 2) env1 CESKHalt
    let cont2 = CESKCont (ANFVar "y2") (aInt 2) env2 CESKHalt
    store3 <- storePutVal store2 addr0 (CESKValCont cont0)
    store4 <- storePutVal store3 addr1 (CESKValCont cont1)
    store5 <- storePutVal store4 addr2 (CESKValCont cont2)
    state <- pure $ CESKState (aBool False)
      env store5 CESKHalt
    (stateEvac, state') <- ceskGarbageCollect state
    pure (state, stateEvac, state')
  assertEqual "store orig"
    (Map.fromList
      [ (a0, CESKStoreVal CESKStoreWhite
          (CESKValCont (CESKCont (ANFVar "y0") (aInt 2)
          (Map.fromList [(ANFVar "x0", a2)]) CESKHalt)))
      , (a1, CESKStoreVal CESKStoreWhite
          (CESKValCont (CESKCont (ANFVar "y1") (aInt 2)
          (Map.fromList [(ANFVar "x1", a0)]) CESKHalt)))
      , (a2, CESKStoreVal CESKStoreWhite
          (CESKValCont (CESKCont (ANFVar "y2") (aInt 2)
          (Map.fromList [(ANFVar "x2", a1)]) CESKHalt)))
      ])
    (stateSpace state)
  assertEqual "store new"
    (Map.fromList
      [ (a0, CESKStoreVal CESKStoreWhite
          (CESKValCont (CESKCont (ANFVar "y2") (aInt 2)
          (Map.fromList [(ANFVar "x2", a1)]) CESKHalt)))
      , (a1, CESKStoreVal CESKStoreWhite
          (CESKValCont (CESKCont (ANFVar "y1") (aInt 2)
          (Map.fromList [(ANFVar "x1", a2)]) CESKHalt)))
      , (a2, CESKStoreVal CESKStoreWhite
          (CESKValCont (CESKCont (ANFVar "y0") (aInt 2)
          (Map.fromList [(ANFVar "x0", a0)]) CESKHalt)))
      ])
    (stateSpace state')
  assertEqual "store old"
    (Map.fromList
      [ (a0, CESKStoreForward (a2))
      , (a1, CESKStoreForward a1)
      , (a2, CESKStoreForward a0)
      ])
    (stateSpace stateEvac)

testClosCycle :: Test
testClosCycle = testCase "clos cycle" $ do
  Right (state, stateEvac, state') <- ceskDo $ do
    (store0, addr0) <- storeAlloc storeEmpty CESKValVoid CESKStoreWhite
    (store1, addr1) <- storeAlloc store0 CESKValVoid CESKStoreWhite
    (store2, addr2) <- storeAlloc store1 CESKValVoid CESKStoreWhite
    let env = Map.fromList [(ANFVar "x", addr2)]
    let env0 = Map.fromList [(ANFVar "x0", addr2)]
    let env1 = Map.fromList [(ANFVar "x1", addr0)]
    let env2 = Map.fromList [(ANFVar "x2", addr1)]
    let clos0 = CESKValClos (ANFLam [ANFVar "a0"] $ aInt 8) env0
    let clos1 = CESKValClos (ANFLam [ANFVar "a1"] $ aInt 8) env1
    let clos2 = CESKValClos (ANFLam [ANFVar "a2"] $ aInt 8) env2
    store3 <- storePutVal store2 addr0 clos0
    store4 <- storePutVal store3 addr1 clos1
    store5 <- storePutVal store4 addr2 clos2
    let state = CESKState (aBool False) env store5 CESKHalt
    (stateEvac, state') <- ceskGarbageCollect state
    pure (state, stateEvac, state')
  assertEqual "store orig"
    (Map.fromList
      [ (a0, CESKStoreVal CESKStoreWhite
          (CESKValClos (ANFLam [ANFVar "a0"] (aInt 8))
          (Map.fromList [(ANFVar "x0", a2)])))
      , (a1, CESKStoreVal CESKStoreWhite
          (CESKValClos (ANFLam [ANFVar "a1"] (aInt 8))
          (Map.fromList [(ANFVar "x1", a0)])))
      , (a2, CESKStoreVal CESKStoreWhite
          (CESKValClos (ANFLam [ANFVar "a2"] (aInt 8))
          (Map.fromList [(ANFVar "x2", a1)])))
      ])
    (stateSpace state)
  assertEqual "store new"
    (Map.fromList
      [ (a0, CESKStoreVal CESKStoreWhite
          (CESKValClos (ANFLam [ANFVar "a2"] (aInt 8))
          (Map.fromList [(ANFVar "x2", a1)])))
      , (a1, CESKStoreVal CESKStoreWhite
          (CESKValClos (ANFLam [ANFVar "a1"] (aInt 8))
          (Map.fromList [(ANFVar "x1", a2)])))
      , (a2, CESKStoreVal CESKStoreWhite
          (CESKValClos (ANFLam [ANFVar "a0"] (aInt 8))
          (Map.fromList [(ANFVar "x0", a0)])))
      ])
    (stateSpace state')
  assertEqual "store old"
    (Map.fromList
      [ (a0, CESKStoreForward a2)
      , (a1, CESKStoreForward a1)
      , (a2, CESKStoreForward a0)
      ])
    (stateSpace stateEvac)
