--------------------------------------------------------------------------------
-- Copyright 2024 Michael P Williams. All rights reserved.
--------------------------------------------------------------------------------

module Gen1.CESK
  ( CESKAddr(..)
  , CESKCont(..)
  , CESKEnv
  , CESKMachine(..)
  , CESKState(..)
  , CESKStatistics(..)
  , CESKStore(..)
  , CESKStoreColor(..)
  , CESKStoreItem(..)
  , CESKStoreSpace
  , CESKVal(..)
  , ceskDo
  , ceskExec
  , ceskGarbageCollect
  , ceskRun

  -- , ceskStoreAlloc
  -- , ceskStoreEmpty
  -- , ceskStoreGetItem
  -- , ceskStorePutItem

  -- , initialStatistics
  -- , initialState
  -- , initialMachine

  , stateSpace
  , storeEmpty
  , storeAlloc
  ) where

import Prelude hiding (exp)
import Data.Either.Combinators (mapRight)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)
import Data.Text (Text)
import qualified Data.Text as T
import Control.Monad (forM, forM_, when)
import Control.Monad.Except (ExceptT(..), runExceptT, throwError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (StateT(..), evalStateT, get, gets, put, modify)
import Gen1.ANF
import Gen1.Util

-- | Defines the execution monad.
type CESK = ExceptT Text (StateT CESKMachine IO)

-- | Defines the evaluation state.
data CESKMachine = CESKMachine
  { ceskMachineStatistics :: CESKStatistics
  , ceskMachineState      :: CESKState
  } deriving (Eq, Ord, Show)

-- | Defines various statistics used to monitor and control
-- the machine.
data CESKStatistics = CESKStatistics
  { ceskStepCountLimit :: Integer
  , ceskStepCountGC    :: Integer
  , ceskStepCountTotal :: Integer
  } deriving (Eq, Ord, Show)

-- | Defines a CESK machine state.
data CESKState = CESKState
  { ceskStateExp   :: ANFExp
  , ceskStateEnv   :: CESKEnv
  , ceskStateStore :: CESKStore
  , ceskStateCont  :: CESKCont
  } deriving (Eq, Ord, Show)

-- | Defines an environment, which is a map of variables
-- to addresses in the store.
type CESKEnv = Map ANFVar CESKAddr

-- | Defines an address in the store.
newtype CESKAddr = CESKAddr Integer deriving (Eq, Ord, Show)

-- | Defines a store.
data CESKStore = CESKStore
  { ceskStoreSpace :: CESKStoreSpace
  , ceskStoreAddr  :: Integer
  , ceskStoreSize  :: Integer
  } deriving (Eq, Ord, Show)

-- | Defines the address space in the store.
type CESKStoreSpace = Map CESKAddr CESKStoreItem

-- | Defines an item in the store.
data CESKStoreItem
  = CESKStoreVal CESKStoreColor CESKVal
  | CESKStoreForward CESKAddr
    deriving (Eq, Ord, Show)

-- | Defines the garbage collection state for a value in the store.
data CESKStoreColor
  = CESKStoreWhite
  | CESKStoreBlack
  | CESKStoreGray
    deriving (Eq, Ord, Show)

-- | Defines a continuation.
data CESKCont
  = CESKCont ANFVar ANFExp CESKEnv CESKCont
  | CESKHalt
    deriving (Eq, Ord, Show)

-- | Defines a value.
data CESKVal
  = CESKValVoid
  | CESKValInt Integer
  | CESKValBool Bool
  | CESKValClos ANFLam CESKEnv
  | CESKValCont CESKCont
    deriving (Eq, Ord, Show)

-- TODO change to default instance
initialStatistics :: CESKStatistics
initialStatistics = CESKStatistics
  { ceskStepCountLimit = 200
  , ceskStepCountGC    = 0
  , ceskStepCountTotal = 0
  }

initialState :: CESKState
initialState = CESKState
  { ceskStateExp   = ANFExpAtomic ANFAtomicVoid
  , ceskStateEnv   = ceskEnvEmpty
  , ceskStateStore = ceskStoreEmpty
  , ceskStateCont  = CESKHalt
  }

initialMachine :: CESKMachine
initialMachine = CESKMachine
  { ceskMachineStatistics = initialStatistics
  , ceskMachineState      = initialState
  }

-- | Extracts the space from a state.
stateSpace :: CESKState -> CESKStoreSpace
stateSpace CESKState{..} = ceskStoreSpace ceskStateStore

-- | Modifies the machine statistics.
modifyStatistics :: (CESKStatistics -> CESKStatistics) -> CESK ()
modifyStatistics f = modify $ \s -> s
  { ceskMachineStatistics = f $ ceskMachineStatistics s }

-- | Modifies the machine state.
modifyState :: (CESKState -> CESKState) -> CESK ()
modifyState f = modify $ \s -> s
  { ceskMachineState = f $ ceskMachineState s }

-- | Runs an ANF program source code.
ceskRun :: Text -> IO (Either Text CESKVal)
ceskRun source =
  case anfParse source of
    Right prog -> do
      ceskExec prog
    Left e -> do
      pure $ Left e

-- | Runs an ANF program AST.
ceskExec :: ANFProg -> IO (Either Text CESKVal)
ceskExec = ceskDo . ceskEval

-- | Runs a CESK monad.
ceskDo :: CESK a -> IO (Either Text a)
ceskDo x = evalStateT (runExceptT x) initialMachine

-- | Evaluates an ANF program to a final value
-- under a CESK machine monad.
ceskEval :: ANFProg -> CESK CESKVal
ceskEval prog = do
  ceskInject prog
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
      throwError "too many top level expressions"
    [] -> do
      throwError "top level expression missing"

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
  ANFDecDefine var _ -> do
    throwError "only lambda or constants allowed in definitions"
  ANFDecBegin{} -> do
    throwError "being not yet supported"
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
    ANFAtomicBool x -> do
      pure $ CESKValBool x
    ANFAtomicLam lam -> do
      CESKValClos lam . ceskStateEnv <$> gets ceskMachineState
    ANFAtomicVar var -> do
      ceskGetVar var
    ANFAtomicPrim ANFPrimAdd [aexp1, aexp2] ->
      evalBinaryInt (+) aexp1 aexp2
    ANFAtomicPrim ANFPrimSub [aexp1, aexp2] ->
      evalBinaryInt (-) aexp1 aexp2
    ANFAtomicPrim ANFPrimMul [aexp1, aexp2] ->
      evalBinaryInt (*) aexp1 aexp2
    ANFAtomicPrim ANFPrimDiv [aexp1, aexp2] ->
      evalBinaryInt div aexp1 aexp2
    ANFAtomicPrim ANFPrimEq [aexp1, aexp2] ->
      evalCompareInt (==) aexp1 aexp2
    ANFAtomicPrim prim _ ->
      throwError $ "bad args for prim " <> textShow prim

type BinaryInt = Integer -> Integer -> Integer

evalBinaryInt :: BinaryInt -> ANFAtomic -> ANFAtomic -> CESK CESKVal
evalBinaryInt f aexp1 aexp2 = do
  CESKValInt x <- evalAtomic aexp1 -- TODO: check the results types
  CESKValInt y <- evalAtomic aexp2
  pure . CESKValInt $ f x y

type CompareInt = Integer -> Integer -> Bool

evalCompareInt :: CompareInt -> ANFAtomic -> ANFAtomic -> CESK CESKVal
evalCompareInt f aexp1 aexp2 = do
  CESKValInt x <- evalAtomic aexp1 -- TODO: check the results types
  CESKValInt y <- evalAtomic aexp2
  pure . CESKValBool $ f x y

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
    ANFExpComplex (ANFComplexApp aexps) ->
      case aexps of
        (arg0:arg1:args) -> do
          proc <- evalAtomic arg0
          vals <- forM (arg1:args) evalAtomic
          ceskApplyProc proc vals
        _otherwise ->
          throwError "bad application"
    ANFExpComplex (ANFComplexIf aexp exp0 exp1) -> do
      evalAtomic aexp >>= \case
        CESKValBool True ->
          modifyState $ const $ CESKState exp0 env store cont
        CESKValBool False ->
          modifyState $ const $ CESKState exp1 env store cont
        _otherwise ->
          throwError "bad if expression"
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
      throwError $ "not a proc: " <> textShow val

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
      throwError "cannot apply halt"

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

-- | Defines an empty environment.
envEmpty :: CESKEnv
envEmpty = Map.empty

-- | Puts a variable in the current environment.
envPut :: CESKEnv -> ANFVar -> CESKAddr -> CESK CESKEnv
envPut env var addr = do
  pure $ Map.insert var addr env

-- | Gets the address for a variable from the current environment.
envGet :: CESKEnv -> ANFVar -> CESK CESKAddr
envGet env var = do
  maybe (throwError $ "bad var " <> textShow var) pure $
    Map.lookup var env

-- | Gets the value for a variable.
ceskGetVar :: ANFVar -> CESK CESKVal
ceskGetVar var = ceskEnvGet var >>= ceskStoreGetVal

-- | Adds a variable and its value to the machine state.
ceskPutVar :: ANFVar -> CESKVal -> CESK ()
ceskPutVar var val = ceskStoreAlloc val CESKStoreWhite >>= ceskEnvPut var

-- | Defines a set of variables and corresponding values by allocating
-- the value in the store and defining the variable in the environment.
ceskPutVars :: [ANFVar] -> [CESKVal] -> CESK ()
ceskPutVars (_:_) [] = throwError "missing vals"
ceskPutVars [] (_:_) = throwError "missing vars"
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

-- | Defines an empty store.
storeEmpty :: CESKStore
storeEmpty = CESKStore
  { ceskStoreSpace = Map.empty
  , ceskStoreAddr  = 0
  , ceskStoreSize  = 0
  }

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
      throwError $ "free bad addr " <> textShow addr
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
    CESKStoreForward _addr -> do
      throwError "bad store value"

-- | Gets an item from a store.
storeGetItem :: CESKStore -> CESKAddr -> CESK CESKStoreItem
storeGetItem CESKStore{..} addr = do
  case Map.lookup addr ceskStoreSpace of
    Nothing -> do
      throwError $ "bad address " <> textShow addr
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
    Just (_, CESKStoreForward _) -> do
      throwError "forward found while scavenging"
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
      throwError $ "bad color " <> textShow c
    CESKStoreForward _ -> do
      throwError "forward found while recoloring"
  pure $ CESKState exp env (CESKStore space' supply size) cont
