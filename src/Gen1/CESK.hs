--------------------------------------------------------------------------------
-- Copyright 2024 Michael P Williams. All rights reserved.
--------------------------------------------------------------------------------

module Gen1.CESK
  ( ceskDo
  , ceskExec
  , ceskGarbageCollect
  , ceskRun
  , storeAlloc
  , storeEmpty
  , storePutVal
  , module X
  ) where

import Prelude hiding (exp)
import Data.Either.Combinators (mapRight)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)
import Data.Text (Text)
import qualified Data.Text as T
import Control.Monad (forM, forM_, when)
import Control.Monad.Except (catchError, runExceptT, throwError)
import Control.Monad.State (evalStateT, get, gets, put, modify)
import Gen1.ANF
import Gen1.Intrinsic
import Gen1.Types as X
import Gen1.Util

-- | Runs an ANF program source code.
ceskRun :: Text -> IO (Either CESKError CESKVal)
ceskRun source =
  case anfParse source of
    Right prog -> do
      ceskExec prog
    Left e -> do
      pure . Left $ CESKErrorParse e

-- | Runs an ANF program AST.
ceskExec :: ANFProg -> IO (Either CESKError CESKVal)
ceskExec = ceskDo . ceskEval

-- | Runs a CESK monad.
ceskDo :: CESK a -> IO (Either CESKError a)
ceskDo x = evalStateT (runExceptT x) initialMachine

-- | Evaluates an ANF program to a final value
-- under a CESK machine monad.
ceskEval :: ANFProg -> CESK CESKVal
ceskEval (ANFProg decs) = do
  wappers <- ceskIntrinsicWrappers
  ceskInject $ ANFProg $ wappers <> decs
  ceskLoop

-- | Loops over the steps until program evalutation is complete.
ceskLoop :: CESK CESKVal
ceskLoop = do
  CESKState{..} <- gets ceskMachineState
  case ceskStateExp of
    ANFExpAtomic aexp | ceskStateCont == CESKHalt -> do
      evalAtomic aexp
    _otherwise -> do
      ceskStep >> ceskAdmin >> ceskLoop

-- | Perform interstep machine administration.
ceskAdmin :: CESK ()
ceskAdmin = do
  -- Increment the program counters.
  modifyStatistics $ \s -> s
    { ceskStepCountGC    = 1 + ceskStepCountGC s
    , ceskStepCountTotal = 1 + ceskStepCountTotal s
    }
  CESKStatistics{..} <- gets ceskMachineStatistics
  -- Check if it is time to empty the trash.
  when (ceskStepCountGC >= ceskStepCountLimit) $ do
    state <- gets ceskMachineState
    (_, state') <- ceskGarbageCollect state
    modifyState $ const $ state'
    modifyStatistics $ \s -> s { ceskStepCountGC = 0 }

-- | Sets an ANF program as the initial state for the CESK machine.
ceskInject :: ANFProg -> CESK ()
ceskInject (ANFProg decs) = do
  ceskInitDecs decs
  -- Find the main entry expression.
  case [exp | ANFDecExp exp <- decs] of
    (exp:[]) -> do
      modifyState $ \s -> s
        { ceskStateExp = exp
        , ceskStateCont = CESKHalt
        }
    (_:_) -> do
      throwError CESKErrorTopLevelMultiple
    [] -> do
      throwError CESKErrorTopLevelNone

-- | Initializes the program declarations.
ceskInitDecs :: [ANFDec] -> CESK ()
ceskInitDecs = mapM_ $ \case
  ANFDecDefine var (ANFExpAtomic (ANFAtomicLam lam)) -> do
    -- Support recursion in definitions by defining the variable
    -- first, capturing the environment with the variable, and
    -- then put the closure in storage with the updated environment.
    -- This is essentially a top-level letrec.
    ceskPutVar var CESKValVoid
    addr <- ceskEnvGet var
    clo <- CESKValClos lam . ceskStateEnv <$> gets ceskMachineState
    ceskStorePutVal addr clo
  ANFDecDefine var (ANFExpAtomic (ANFAtomicBool x)) -> do
    ceskPutVar var $ CESKValBool x
  ANFDecDefine var (ANFExpAtomic (ANFAtomicInt x)) -> do
    ceskPutVar var $ CESKValInt x
  ANFDecDefine var (ANFExpAtomic (ANFAtomicFloat x)) -> do
    ceskPutVar var $ CESKValFloat x
  ANFDecDefine var _ -> do
    throwError $ CESKErrorDefinitionBad var
  ANFDecBegin decs -> do
    ceskInitDecs decs
  ANFDecExp{} -> do
    pure ()

-- | Evaluates an atomic expression.
evalAtomic :: ANFAtomic -> CESK CESKVal
evalAtomic atomic = do
  case atomic of
    ANFAtomicVoid ->
      pure CESKValVoid
    ANFAtomicInt x -> do
      pure $ CESKValInt x
    ANFAtomicFloat x -> do
      pure $ CESKValFloat x
    ANFAtomicBool x -> do
      pure $ CESKValBool x
    ANFAtomicStr x -> do
      pure $ CESKValStr x
    ANFAtomicChar x -> do
      pure $ CESKValChar x
    ANFAtomicLam lam -> do
      CESKValClos lam . ceskStateEnv <$> gets ceskMachineState
    ANFAtomicVar var -> do
      ceskGetVar var
    ANFAtomicPrim (ANFPrimFunc name) args -> do
      evalIntrinsic name args
    ANFAtomicPrim p [a, b] | primBinary p -> do
      evalBinary p a b
    ANFAtomicPrim p [a, b] | primLogical p -> do
      evalLogical p a b
    ANFAtomicPrim p _args -> do
      throwError $ CESKErrorPrimitiveArgs p

-- | Evaluates an intrinsic function.
evalIntrinsic :: Text -> [ANFAtomic] -> CESK CESKVal
evalIntrinsic name aexps = do
  case Map.lookup name intrinsicMap of
    Nothing -> do
      throwError $ CESKErrorIntrinsicBad name
    Just (CESKIntrinsic n _ _ f) -> do
      (mapM evalAtomic aexps >>= f) `catchError` \_ -> do
        throwError $ CESKErrorIntrinsicCall n

-- | Determines if a primitive is binary.
primBinary :: ANFPrim -> Bool
primBinary = \case
  ANFPrimAdd -> True
  ANFPrimSub -> True
  ANFPrimMul -> True
  ANFPrimDiv -> True
  _otherwise -> False

-- | Determines if a primitive is logical.
primLogical :: ANFPrim -> Bool
primLogical = \case
  ANFPrimEQ -> True
  ANFPrimNE -> True
  ANFPrimGT -> True
  ANFPrimGE -> True
  ANFPrimLT -> True
  ANFPrimLE -> True
  _otherwise -> False

-- | Evaluates a binary primitive.
evalBinary :: ANFPrim -> ANFAtomic -> ANFAtomic -> CESK CESKVal
evalBinary prim aexp1 aexp2 = do
  x <- evalAtomic aexp1
  y <- evalAtomic aexp2
  case (x, y) of
    (CESKValInt a, CESKValInt b) -> do
      evalBinaryInt prim a b
    (CESKValInt a, CESKValFloat b) -> do
      evalBinaryFloat prim (fromIntegral a) b
    (CESKValFloat a, CESKValInt b) -> do
      evalBinaryFloat prim a $ fromIntegral b
    (CESKValFloat a, CESKValFloat b) -> do
      evalBinaryFloat prim a b
    (a, b) -> do
      throwError . CESKErrorPrimitiveBad $
        "cannot apply primative to " <>
          ceskValDesc a <> " and " <> ceskValDesc b

-- | Evaluates a binary integer primitive.
evalBinaryInt :: ANFPrim -> Integer -> Integer -> CESK CESKVal
evalBinaryInt prim x y = do
  case prim of
    ANFPrimAdd -> pure . CESKValInt $ x + y
    ANFPrimSub -> pure . CESKValInt $ x - y
    ANFPrimMul -> pure . CESKValInt $ x * y
    ANFPrimDiv -> pure . CESKValInt $ x `div` y
    _otherwise -> throwError . CESKErrorPrimitiveBad $
      "bad binary primitive for integers: " <> textShow prim

-- | Evaluates a binary float primitive.
evalBinaryFloat :: ANFPrim -> Double -> Double -> CESK CESKVal
evalBinaryFloat prim x y = do
  case prim of
    ANFPrimAdd -> pure . CESKValFloat $ x + y
    ANFPrimSub -> pure . CESKValFloat $ x - y
    ANFPrimMul -> pure . CESKValFloat $ x * y
    ANFPrimDiv -> pure . CESKValFloat $ x / y
    _otherwise -> throwError . CESKErrorPrimitiveBad $
      "bad binary primitive for floats: " <> textShow prim

-- | Evaluates a logical primitive.
evalLogical :: ANFPrim -> ANFAtomic -> ANFAtomic -> CESK CESKVal
evalLogical prim aexp1 aexp2 = do
  x <- evalAtomic aexp1
  y <- evalAtomic aexp2
  case (x, y) of
    (CESKValInt a, CESKValInt b) -> do
      evalLogicalInt prim a b
    (CESKValInt a, CESKValFloat b) -> do
      evalLogicalFloat prim (fromIntegral a) b
    (CESKValFloat a, CESKValInt b) -> do
      evalLogicalFloat prim a $ fromIntegral b
    (CESKValFloat a, CESKValFloat b) -> do
      evalLogicalFloat prim a b
    (CESKValBool a, CESKValBool b) -> do
      evalLogicalBool prim a b
    (CESKValChar a, CESKValChar b) -> do
      evalLogicalChar prim a b
    (a, b) -> do
      throwError . CESKErrorPrimitiveBad $
        "cannot apply primative to " <>
          ceskValDesc a <> " and " <> ceskValDesc b

-- | Evaluates a logical integer primitive.
evalLogicalInt :: ANFPrim -> Integer -> Integer -> CESK CESKVal
evalLogicalInt prim x y = do
  case prim of
    ANFPrimEQ -> pure . CESKValBool $ x == y
    ANFPrimNE -> pure . CESKValBool $ x /= y
    ANFPrimGT -> pure . CESKValBool $ x > y
    ANFPrimGE -> pure . CESKValBool $ x >= y
    ANFPrimLT -> pure . CESKValBool $ x < y
    ANFPrimLE -> pure . CESKValBool $ x <= y
    _otherwise -> throwError . CESKErrorPrimitiveBad $
      "bad logical primitive for integers: " <> textShow prim

-- | Evaluates a logical integer primitive.
evalLogicalFloat :: ANFPrim -> Double -> Double -> CESK CESKVal
evalLogicalFloat prim x y = do
  case prim of
    ANFPrimEQ -> pure . CESKValBool $ x == y
    ANFPrimNE -> pure . CESKValBool $ x /= y
    ANFPrimGT -> pure . CESKValBool $ x > y
    ANFPrimGE -> pure . CESKValBool $ x >= y
    ANFPrimLT -> pure . CESKValBool $ x < y
    ANFPrimLE -> pure . CESKValBool $ x <= y
    _otherwise -> throwError . CESKErrorPrimitiveBad $
      "bad logical primitive for floats: " <> textShow prim

-- | Evaluates a logical boolean primitive.
evalLogicalBool :: ANFPrim -> Bool -> Bool -> CESK CESKVal
evalLogicalBool prim x y = do
  case prim of
    ANFPrimEQ -> pure . CESKValBool $ x == y
    ANFPrimNE -> pure . CESKValBool $ x /= y
    _otherwise -> throwError . CESKErrorPrimitiveBad $
      "bad logical primitive for bools: " <> textShow prim

-- | Evaluates a logical char primitive.
evalLogicalChar :: ANFPrim -> Char -> Char -> CESK CESKVal
evalLogicalChar prim x y = do
  case prim of
    ANFPrimEQ -> pure . CESKValBool $ x == y
    ANFPrimNE -> pure . CESKValBool $ x /= y
    _otherwise -> throwError . CESKErrorPrimitiveBad $
      "bad logical primitive for bools: " <> textShow prim

-- | Steps the machine from the current state to the next.
ceskStep :: CESK ()
ceskStep = do
  CESKState exp env store cont <- gets ceskMachineState
  case exp of
    ANFExpLet var exp0 exp1 -> do
      modifyState $ const $
        CESKState exp0 env store $ CESKCont var exp1 env cont
    ANFExpAtomic aexp -> do
      evalAtomic aexp >>= ceskApplyCont
    ANFExpComplex (ANFComplexApp aexps) -> do
      case aexps of
        (arg0:args) -> do
          func <- evalAtomic arg0
          vals <- forM args evalAtomic
          ceskApplyProc func vals
        _otherwise -> do
          throwError CESKErrorApplication
    ANFExpComplex (ANFComplexIf aexp exp0 exp1) -> do
      evalAtomic aexp >>= \case
        CESKValBool True ->
          modifyState $ const $ CESKState exp0 env store cont
        CESKValBool False ->
          modifyState $ const $ CESKState exp1 env store cont
        _otherwise ->
          throwError CESKErrorIfExpression
    ANFExpComplex (ANFComplexCallCC aexp) -> do
      f <- evalAtomic aexp
      ceskApplyProc f [CESKValCont cont]
    ANFExpComplex (ANFComplexSet var aexp) -> do
      addr <- ceskEnvGet var
      evalAtomic aexp >>= ceskStorePutVal addr
      ceskApplyCont CESKValVoid
    ANFExpComplex (ANFComplexLetRec bindings body) -> do
      (vars, vals) <- pure $ unzip $
        map (\(ANFBind var _) -> (var, CESKValVoid)) bindings
      ceskPutVars vars vals
      vals' <- forM bindings $ \(ANFBind _ aexp) -> evalAtomic aexp
      addrs <- forM vars ceskEnvGet
      forM_ (zip addrs vals') $ \(a, v) -> ceskStorePutVal a v
      modifyState $ \s -> s { ceskStateExp = body }

-- | Applies a procedure.
ceskApplyProc :: CESKVal -> [CESKVal] -> CESK ()
ceskApplyProc val vals = do
  case val of
    CESKValClos (ANFLam vars exp) env -> do
      modifyState $ \s -> s
        { ceskStateExp = exp
        , ceskStateEnv = env
        }
      ceskPutVars vars vals
    _otherwise -> do
      throwError CESKErrorProcedure

-- | Applies a continuation.
ceskApplyCont :: CESKVal -> CESK ()
ceskApplyCont val = do
  CESKState{..} <- gets ceskMachineState
  case ceskStateCont of
    CESKCont var exp env cont -> do
      modifyState $ \s -> s
        { ceskStateExp = exp
        , ceskStateEnv = env
        , ceskStateCont = cont
        }
      ceskPutVar var val
    CESKHalt {} -> do
      throwError CESKErrorHaltApplication

-- | Defines an empty environment.
ceskEnvEmpty :: CESKEnv
ceskEnvEmpty = envEmpty

-- | Gets the address for a variable from the current environment.
ceskEnvGet :: ANFVar -> CESK CESKAddr
ceskEnvGet var = do
  CESKState{..} <- gets ceskMachineState
  envGet ceskStateEnv var

-- | Puts a variable in the current environment.
ceskEnvPut :: ANFVar -> CESKAddr -> CESK ()
ceskEnvPut var addr = do
  CESKState{..} <- gets ceskMachineState
  env <- envPut ceskStateEnv var addr
  modifyState $ \s -> s { ceskStateEnv = env }

-- | Puts a variable in the current environment.
envPut :: CESKEnv -> ANFVar -> CESKAddr -> CESK CESKEnv
envPut env var addr = do
  pure $ Map.insert var addr env

-- | Gets the address for a variable from the current environment.
envGet :: CESKEnv -> ANFVar -> CESK CESKAddr
envGet env var = do
  maybe (throwError $ CESKErrorVar var) pure $ Map.lookup var env

-- | Gets the value for a variable.
ceskGetVar :: ANFVar -> CESK CESKVal
ceskGetVar var = ceskEnvGet var >>= ceskStoreGetVal

-- | Adds a variable and its value to the machine state.
ceskPutVar :: ANFVar -> CESKVal -> CESK ()
ceskPutVar var val = ceskStoreAlloc val CESKStoreWhite >>= ceskEnvPut var

-- | Defines a set of variables and corresponding values by allocating
-- the value in the store and defining the variable in the environment.
ceskPutVars :: [ANFVar] -> [CESKVal] -> CESK ()
ceskPutVars (_:_) [] = throwError CESKErrorMissingVals
ceskPutVars [] (_:_) = throwError CESKErrorMissingVars
ceskPutVars [] [] = pure ()
ceskPutVars (var:vars) (val:vals) = do
  ceskPutVar var val
  ceskPutVars vars vals

-- | Defines an empty store.
ceskStoreEmpty :: CESKStore
ceskStoreEmpty = storeEmpty

-- | Allocates a value in the store and returns its address.
ceskStoreAlloc :: CESKVal -> CESKStoreColor -> CESK CESKAddr
ceskStoreAlloc val color = do
  CESKState{..} <- gets ceskMachineState
  (store, addr) <- storeAlloc ceskStateStore val color
  modifyState $ \s -> s { ceskStateStore = store }
  pure addr

-- | Frees a value from the store.
ceskStoreFree :: CESKAddr -> CESK ()
ceskStoreFree addr = do
  CESKState{..} <- gets ceskMachineState
  store <- storeFree ceskStateStore addr
  modifyState $ \s -> s { ceskStateStore = store }

-- | Updates the value at an address in the store.
ceskStorePutVal :: CESKAddr -> CESKVal -> CESK ()
ceskStorePutVal addr val = do
  CESKState{..} <- gets ceskMachineState
  store <- storePutVal ceskStateStore addr val
  modifyState $ \s -> s { ceskStateStore = store }

-- | Updates a store item at an address in the store.
ceskStorePutItem :: CESKAddr -> CESKStoreItem -> CESK ()
ceskStorePutItem addr item = do
  CESKState{..} <- gets ceskMachineState
  store <- storePutItem ceskStateStore addr item
  modifyState $ \s -> s { ceskStateStore = store }

-- | Gets a value from the store.
ceskStoreGetVal :: CESKAddr -> CESK CESKVal
ceskStoreGetVal addr = do
  CESKState{..} <- gets ceskMachineState
  storeGetVal ceskStateStore addr

-- | Gets an item from the store.
ceskStoreGetItem :: CESKAddr -> CESK CESKStoreItem
ceskStoreGetItem addr = do
  CESKState{..} <- gets ceskMachineState
  storeGetItem ceskStateStore addr

-- | Allocates a value in a store and returns the
-- unpdated store and its address.
storeAlloc :: CESKStore -> CESKVal -> CESKStoreColor -> CESK (CESKStore, CESKAddr)
storeAlloc CESKStore{..} val color =
  pure (s, a)
  where
    a = CESKAddr ceskStoreAddr
    v = CESKStoreVal color val
    s = CESKStore
      { ceskStoreSpace = Map.insert a v ceskStoreSpace
      , ceskStoreAddr  = ceskStoreAddr + 1
      , ceskStoreSize  = ceskStoreSize + 1
      }

-- | Frees a value from a store.
storeFree :: CESKStore -> CESKAddr -> CESK CESKStore
storeFree CESKStore{..} addr
  | Map.notMember addr ceskStoreSpace = do
      throwError $ CESKErrorFreeBad addr
  | otherwise = do
      pure CESKStore
        { ceskStoreSpace = Map.delete addr ceskStoreSpace
        , ceskStoreAddr  = ceskStoreAddr
        , ceskStoreSize  = ceskStoreSize - 1
        }

-- | Updates the value at an address in a store.
storePutVal :: CESKStore -> CESKAddr -> CESKVal -> CESK CESKStore
storePutVal store addr val = do
  storePutItem store addr $ CESKStoreVal CESKStoreWhite val

-- | Updates a store item at an address in a store.
storePutItem :: CESKStore -> CESKAddr -> CESKStoreItem -> CESK CESKStore
storePutItem store addr item = do
  let space = ceskStoreSpace store
  pure store { ceskStoreSpace = Map.insert addr item space }

-- | Gets a value from a store.
storeGetVal :: CESKStore -> CESKAddr -> CESK CESKVal
storeGetVal store addr = do
  storeGetItem store addr >>= \case
    CESKStoreVal _color val -> do
      pure val
    CESKStoreForward a -> do
      throwError $ CESKErrorUnexpectedForward a

-- | Gets an item from a store.
storeGetItem :: CESKStore -> CESKAddr -> CESK CESKStoreItem
storeGetItem CESKStore{..} addr = do
  case Map.lookup addr ceskStoreSpace of
    Nothing -> do
      throwError $ CESKErrorAddressBad addr
    Just item -> do
      pure item

-- | Performs garbage collection on a state.
-- Returns a tuple of the modified old state (for testing purposes) and
-- the new, resulting state respectively.
ceskGarbageCollect :: CESKState -> CESK (CESKState, CESKState)
ceskGarbageCollect state = do
  (stateFrom, stateTo) <- ceskEvacuateState state
  (stateFrom0, stateTo0) <- ceskScavengeState stateFrom stateTo
  stateTo' <- ceskColor stateTo0 CESKStoreWhite
  pure (stateFrom0, stateTo')

-- | Evacuates a state.
-- All the root items (those external to the store) are located
-- and copied to the new state "to-space".
ceskEvacuateState :: CESKState -> CESK (CESKState, CESKState)
ceskEvacuateState (CESKState exp env store cont) = do
  (from0, to0, env') <- ceskEvacuateEnv store ceskStoreEmpty env
  (from', to', cont') <- ceskEvacuateCont from0 to0 cont
  pure (CESKState exp env from' cont', CESKState exp env' to' cont')

-- | Evacuates a continuation.
ceskEvacuateCont :: CESKStore -> CESKStore -> CESKCont -> CESK (CESKStore, CESKStore, CESKCont)
ceskEvacuateCont storeFrom storeTo = \case
  CESKHalt -> do
    pure (storeFrom, storeTo, CESKHalt)
  CESKCont var exp env cont -> do
    (from0, to0, env') <- ceskEvacuateEnv storeFrom storeTo env
    (from', to', cont') <- ceskEvacuateCont from0 to0 cont
    pure (from', to', CESKCont var exp env' cont')

-- | Evacuates an environment.
ceskEvacuateEnv :: CESKStore -> CESKStore -> CESKEnv -> CESK (CESKStore, CESKStore, CESKEnv)
ceskEvacuateEnv storeFrom storeTo env =
  go storeFrom storeTo env $ Map.toAscList env
  where
    go from to env' = \case
      (var, addr):vars -> do
        storeGetItem from addr >>= \case
          CESKStoreForward addr' -> do
            go from to (Map.insert var addr' env') vars
          CESKStoreVal _color val -> do
            (to', addr') <- storeAlloc storeTo val CESKStoreGray
            from' <- storePutItem storeFrom addr $ CESKStoreForward addr'
            go from' to' (Map.insert var addr' env') vars
      [] -> do
        pure (from, to, env')

-- | Scavenges a state.
-- The state store is searched for additional items that need to
-- be moved to "to-space".
ceskScavengeState :: CESKState -> CESKState -> CESK (CESKState, CESKState)
ceskScavengeState stateFrom stateTo = do
  let (CESKState exp0 env0 store0 cont0) = stateFrom
  let (CESKState exp1 env1 store1 cont1) = stateTo
  (store0', store1') <- ceskScavenge store0 store1
  pure $
    ( CESKState exp0 env0 store0' cont0
    , CESKState exp1 env1 store1' cont1
    )

-- | Scavenges a store.
ceskScavenge :: CESKStore -> CESKStore -> CESK (CESKStore, CESKStore)
ceskScavenge storeFrom storeTo =
  case grayItem of
    Just (addr, CESKStoreVal _ (CESKValClos lam env)) -> do
      (from0, to0, env') <- ceskEvacuateEnv storeFrom storeTo env
      to' <- storePutItem to0 addr $ blackVal $ CESKValClos lam env'
      ceskScavenge from0 to'
    Just (addr, CESKStoreVal _ (CESKValCont cont)) -> do
      (from0, to0, cont') <- ceskEvacuateCont storeFrom storeTo cont
      to' <- storePutItem to0 addr $ blackVal $ CESKValCont cont'
      ceskScavenge from0 to'
    Just (addr, CESKStoreVal _ val) -> do
      to' <- storePutItem storeTo addr $ blackVal val
      ceskScavenge storeFrom to'
    Just (_, CESKStoreForward a) -> do
      throwError $ CESKErrorUnexpectedForward a
    Nothing -> do
      pure (storeFrom, storeTo)
  where
    isGray (CESKStoreVal CESKStoreGray _) = True
    isGray _ = False
    (grayMap, _) = Map.partition isGray $ ceskStoreSpace storeTo
    grayItem = Map.lookupMin grayMap
    blackVal = CESKStoreVal CESKStoreBlack

-- | Changes the collection color of a state.
-- The supplied state is expected to be all black at this point,
-- so recolor also acts as a validator of this requirement.
ceskColor :: CESKState -> CESKStoreColor -> CESK CESKState
ceskColor (CESKState exp env (CESKStore space supply size) cont) color = do
  space' <- forM space $ \case
    CESKStoreVal CESKStoreBlack val -> do
      pure $ CESKStoreVal color val
    CESKStoreVal c _ -> do
      throwError $ CESKErrorUnexpectedColor c
    CESKStoreForward a -> do
      throwError $ CESKErrorUnexpectedForward a
  pure $ CESKState exp env (CESKStore space' supply size) cont
