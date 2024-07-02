--------------------------------------------------------------------------------
-- Copyright 2024 Michael P Williams. All rights reserved.
--------------------------------------------------------------------------------

module Gen1.CESK
  ( CESKAddr(..)
  , CESKCont(..)
  , CESKEnv
  , CESKState(..)
  , CESKStore(..)
  , CESKStoreColor(..)
  , CESKStoreItem(..)
  , CESKStoreSpace
  , CESKVal(..)
  , ceskRun
  , ceskStoreAlloc
  , ceskStoreEmpty
  , ceskStoreGetItem
  , ceskStorePutItem
  ) where

import Prelude hiding (exp)
import Data.Either.Combinators (mapRight)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)
import Data.Text (Text)
import qualified Data.Text as T
import Control.Monad (forM, forM_)
import Control.Monad.Except (ExceptT(..), runExceptT, throwError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (StateT(..), evalStateT, get, gets, put, modify)
import Gen1.ANF
import Gen1.Util

-- | Defines the execution monad.
type CESK = ExceptT Text (StateT CESKState IO)

-- | Defines a machine state.
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

-- runProg :: ANFProg -> CESKVal
-- runProg prog =
--   case x of
--     CESKState (ANFExpAtomic aexp) env store _ ->
--       evalAtomic env store aexp
--     _otherwise ->
--       error "non-atomic result"
--   where
--     finalState = evalProg (inject prog)

--     -- TODO move garbage collect into eval on some interval
--     (_, x) = ceskGarbageCollect finalState

-- | Runs an ANF program source code.
ceskRun :: Text -> IO (Either Text CESKVal)
ceskRun source =
  evalStateT (runExceptT $ ceskEval source) $ CESKState
    { ceskStateExp   = ANFExpAtomic (ANFAtomicInt 0) -- TODO make this a void
    , ceskStateEnv   = ceskEnvEmpty
    , ceskStateStore = ceskStoreEmpty
    , ceskStateCont  = CESKHalt
    }

-- | Evaluates an ANF source program to a final value
-- under a CESK machine monad.
ceskEval :: Text -> CESK CESKVal
ceskEval source = do
  case anfParse source of
    Right prog -> do
      ceskInject prog
      ceskLoop
    Left e -> do
      throwError e

-- | Loops over the steps until program evalutation is complete.
ceskLoop :: CESK CESKVal
ceskLoop = do
  CESKState{..} <- get
  case ceskStateExp of
    ANFExpAtomic aexp | ceskStateCont == CESKHalt -> do
      evalAtomic aexp
    _otherwise -> do
      ceskStep >> ceskLoop

-- | Sets an ANF program as the initial state for the CESK machine.
ceskInject :: ANFProg -> CESK ()
ceskInject (ANFProg decs) = do
  ceskInitDecs decs
  -- Find the main entry expression.
  case [exp | ANFDecExp exp <- decs] of
    (exp:[]) -> do
      modify $ \s -> s
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
    ceskPutVar var $ CESKValClos lam ceskEnvEmpty
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
    ANFAtomicInt x -> do
      pure $ CESKValInt x
    ANFAtomicBool x -> do
      pure $ CESKValBool x
    ANFAtomicLam lam -> do
      CESKValClos lam <$> gets ceskStateEnv
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
  CESKState exp env store cont <- get
  case exp of
    ANFExpLet var exp0 exp1 -> do
      put $ CESKState exp0 env store $ CESKCont var exp1 env cont
    ANFExpAtomic aexp -> do
      evalAtomic aexp >>= ceskApplyCont
    ANFExpComplex (ANFComplexApp aexps) ->
      case aexps of
        (arg0:arg1:args) -> do
          -- let
          --   proc = evalAtomic env store arg0
          --   vals = evalAtomic env store <$> (arg1:args)
          -- in
          --   ceskApplyProc proc vals store cont
          proc <- evalAtomic arg0
          vals <- forM (arg1:args) evalAtomic
          ceskApplyProc proc vals
        _otherwise ->
          throwError "bad application"
    ANFExpComplex (ANFComplexIf aexp exp0 exp1) -> do
      -- case evalAtomic env store aexp of
      --   CESKValBool True -> CESKState exp0 env store cont
      --   CESKValBool False -> CESKState exp1 env store cont
      --   _otherwise -> error "bad if expression"
      evalAtomic aexp >>= \case
        CESKValBool True -> put $ CESKState exp0 env store cont
        CESKValBool False -> put $ CESKState exp1 env store cont
        _otherwise -> throwError "bad if expression"

    ANFExpComplex (ANFComplexCallCC aexp) -> do
      -- let
      --   proc = evalAtomic env store aexp
      --   valcc = CESKValCont cont
      -- in
      --   ceskApplyProc proc [valcc] store cont
      f <- evalAtomic aexp
      ceskApplyProc f [CESKValCont cont]
    ANFExpComplex (ANFComplexSet var aexp) -> do
      addr <- ceskEnvGet var
      evalAtomic aexp >>= ceskStorePutVal addr
      ceskApplyCont CESKValVoid

      -- let
      --   addr = fromJust $ Map.lookup var env
      --   val = evalAtomic env store aexp
      --   store' = ceskStorePutVal store addr val
      -- in
      --   ceskApplyCont cont CESKValVoid store'

    ANFExpComplex (ANFComplexLetRec bindings body) -> do
      (vars, vals) <- pure $ unzip $
        map (\(ANFBind var _) -> (var, CESKValVoid)) bindings
      ceskPutVars vars vals
      vals' <- forM bindings $ \(ANFBind _ aexp) -> evalAtomic aexp
      addrs <- forM vars ceskEnvGet
      forM_ (zip addrs vals') $ \(a, v) -> ceskStorePutVal a v
      modify $ \s -> s { ceskStateExp = body }
      -- CESKState{..} <- get
      -- let ceskStateExp = body
      -- put CESKState{..}
      -- let
      --   (vars, vals) = unzip $ map (\(ANFBind var _) -> (var, CESKValVoid)) bindings
      --   (env', store') = ceskPutVars vars vals env store
      --   vals' = map (\(ANFBind _ aexp) -> evalAtomic env' store' aexp) bindings
      --   addrs = map (\var -> fromJust $ Map.lookup var env') vars
      --   store'' = foldl (\s (a, v) -> ceskStorePutVal s a v) store' $ zip addrs vals'
      -- in
      --   CESKState body env' store'' cont

-- | Applies a procedure.
ceskApplyProc :: CESKVal -> [CESKVal] -> CESK ()
ceskApplyProc val vals = do
  case val of
    CESKValClos (ANFLam vars exp) env -> do
      modify $ \s -> s
        { ceskStateExp = exp
        , ceskStateEnv = env
        }
      ceskPutVars vars vals
    _otherwise -> do
      throwError $ "not a proc: " <> textShow val

-- | Applies a continuation.
ceskApplyCont :: CESKVal -> CESK ()
ceskApplyCont val = do
  CESKState{..} <- get
  case ceskStateCont of
    CESKCont var exp env cont -> do
      modify $ \s -> s
        { ceskStateExp = exp
        , ceskStateEnv = env
        , ceskStateCont = cont
        }
      ceskPutVar var val
    CESKHalt {} -> do
      throwError "cannot apply halt"

-- | Defines an empty environment.
ceskEnvEmpty :: CESKEnv
ceskEnvEmpty = Map.empty

-- | Gets the address for a variable from the current environment.
ceskEnvGet :: ANFVar -> CESK CESKAddr
ceskEnvGet var = do
  gets ceskStateEnv >>= maybe
    (throwError $ "bad var " <> textShow var) pure . Map.lookup var

-- | Puts a variable in the current environment.
ceskEnvPut :: ANFVar -> CESKAddr -> CESK ()
ceskEnvPut var addr = do
  modify $ \s -> s { ceskStateEnv = Map.insert var addr $ ceskStateEnv s }

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
  CESKState{..} <- get
  -- CESKStore{..} <- pure ceskStateStore
  -- ceskStoreAddr <- pure $ ceskStoreAddr + 1
  -- addr <- pure $ CESKAddr ceskStoreAddr
  -- ceskStoreSpace <- pure $
  --   Map.insert addr (CESKStoreVal color val) ceskStoreSpace
  -- ceskStoreSize <- pure $ ceskStoreSize + 1
  -- ceskStateStore <- pure CESKStore{..}
  -- put CESKState{..}
  -- pure addr
  (store, addr) <- storeAlloc ceskStateStore
  modify $ \s -> s { ceskStateStore = store }
  pure addr
  -- put $ CESKState
  -- { ceskStateExp   =
  -- , ceskStateEnv   =
  -- , ceskStateStore = s
  -- , ceskStateCont  = ceskStateCont
  -- }


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




-- | Frees a value from the store.
ceskStoreFree :: CESKAddr -> CESK ()
ceskStoreFree addr = do
  CESKState{..} <- get
  -- CESKStore{..} <- pure ceskStateStore
  -- if | Map.notMember addr ceskStoreSpace -> do
  --       throwError $ "free bad addr " <> textShow addr
  --    | otherwise -> do
  --       ceskStoreSpace <- pure $ Map.delete addr ceskStoreSpace
  --       ceskStoreSize <- pure $ ceskStoreSize - 1
  --       ceskStateStore <- pure CESKStore{..}
  --       put CESKState{..}
  store <- storeFree ceskStateStore addr
  modify $ \s -> s { ceskStateStore = store }


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
ceskStorePutVal :: CESKAddr -> CESKVal -> CESK ()
ceskStorePutVal addr val = do
  -- ceskStorePutItem addr $ CESKStoreVal CESKStoreWhite val
  CESKState{..} <- get
  store <- storePutVal ceskStateStore addr val
  modify $ \s -> s { ceskStateStore = store }


storePutVal :: CESKStore -> CESKAddr -> CESKVal -> CESK CESKStore
storePutVal store addr val = do
  storePutItem store addr $ CESKStoreVal CESKStoreWhite val



-- | Updates a store item at an address in a store.
ceskStorePutItem :: CESKAddr -> CESKStoreItem -> CESK ()
ceskStorePutItem addr item = do
  CESKState{..} <- get
  -- CESKStore{..} <- pure ceskStateStore
  -- ceskStoreSpace <- pure $ Map.insert addr item ceskStoreSpace
  -- ceskStateStore <- pure CESKStore{..}
  -- put CESKState{..}
  store <- storePutItem ceskStateStore addr item
  modify $ \s -> s { ceskStateStore = store }


storePutItem :: CESKStore -> CESKAddr -> CESKStoreItem -> CESK CESKStore
storePutItem store addr item = do
  let space = ceskStoreSpace store
  pure store { ceskStoreSpace = Map.insert addr item space }



-- | Gets a value from a store.
ceskStoreGetVal :: CESKAddr -> CESK CESKVal
ceskStoreGetVal addr = do
  CESKState{..} <- get
  storeGetVal ceskStateStore addr
  -- ceskStoreGetItem addr >>= \case
  --   Just (CESKStoreVal _ val) ->
  --     pure val
  --   Just (CESKStoreForward _) ->
  --     throwError "bad store value"
  --   Nothing ->
  --     throwError $ "bad address " <> textShow addr

storeGetVal :: CESKStore -> CESKAddr -> CESK CESKVal
storeGetVal store addr = do
  storeGetItem store addr >>= \case
    Just (CESKStoreVal _color val) -> do
      pure val
    Just (CESKStoreForward _) -> do
      throwError "bad store value"
    Nothing -> do
      throwError $ "bad address " <> textShow addr



-- | Gets an item from a store.
ceskStoreGetItem :: CESKAddr -> CESK (Maybe CESKStoreItem)
ceskStoreGetItem addr = do
  CESKStore{..} <- gets ceskStateStore
  storeGetItem ceskStoreSpace addr

-- | Gets an item from a store.
storeGetItem :: CESKStore -> CESKAddr -> CESK (Maybe CESKStoreItem)
storeGetItem CESKStore{..} addr = do
  pure $ Map.lookup addr ceskStoreSpace



-- | Defines an empty store.
storeEmpty :: CESKStore
storeEmpty = CESKStore
  { ceskStoreSpace = Map.empty
  , ceskStoreAddr  = 0
  , ceskStoreSize  = 0
  }





-- | Performs garbage collection on a state.
-- Returns a tuple of the modified old state (for testing purposes) and
-- the new, resulting state respectively.
ceskGarbageCollect :: CESKState -> (CESKState, CESKState)
ceskGarbageCollect state =
  (stateFrom', stateTo'')
  where
    (stateFrom, stateTo) = evacuateState state
    (stateFrom', stateTo') = scavengeState stateFrom stateTo
    stateTo'' = recolor CESKStoreWhite stateTo'


data GCCycle = GCCycle
  { gcCycleEnv  :: CESKEnv
  , gcCycleFrom :: CESKStore
  , gcCycleTo   :: CESKStore
  }

data CESKState = CESKState
  { ceskStateExp   :: ANFExp
  , ceskStateEnv   :: CESKEnv
  , ceskStateStore :: CESKStore
  , ceskStateCont  :: CESKCont
  } deriving (Eq, Ord, Show)



ceskGarbageCollect :: CESK ()
ceskGarbageCollect = do
  undefined

ceskColorState :: CESKStoreColor -> CESKState -> CESK CESKState
ceskColorState color to = do
  undefined

ceskEvacuateState :: GCCycle -> CESK GCCycle
ceskEvacuateState = do
  undefined

ceskScavengeState :: GCCycle -> CESK GCCycle
ceskScavengeState to = do
  undefined

-- evacuateCont :: CESKCont -> CESKStore -> CESKStore -> (CESKCont, CESKStore, CESKStore)
ceskEvacuateCont :: GCCycle -> CESK GCCycle
ceskEvacuateCont to = do
  undefined

-- evacuateEnv :: CESKEnv -> CESKStore -> CESKStore -> (CESKEnv, CESKStore, CESKStore)
ceskEvacuateEnv :: GCCycle -> CESK GCCycle
ceskEvacuateEnv cycle@GCCycle{..} = do
  go (Map.toAscList gcCycleEnv) cycle
  where
    go [(CESKVar,CESKAddr)] -> GCCycle -> CESK GCCycle
    go [] cycle = do
      pure cycle
    go ((var, addr):vars) (GCCycle env storeFrom storeTo) = do
      case storeGetItem storeFrom addr of
        CESKStoreForward addrForward -> do
          go vars GCCycle
            { gcCycleEnv  = Map.insert var addrForward env
            , gcCycleFrom = storeFrom
            , gcCycleTo   = storeTo
            }
        CESKStoreVal _color val -> do
          let
            (to', addrVal) = storeAlloc to val CESKStoreGray
            from' = storePutItem from addr $ CESKStoreForward addrVal
          go vars GCCycle
            { gcCycleEnv  = Map.insert var addrForward env
            , gcCycleFrom = storeFrom
            , gcCycleTo   = storeTo
            }



    go [] env' from to =
      (env', from, to)
    go ((var, addr):vars) env' from to =
      case ceskStoreGetItem from addr of
        CESKStoreVal _color val ->
          let
            (to', addr') = ceskStoreAlloc to val CESKStoreGray
            from' = ceskStorePutItem from addr $ CESKStoreForward addr'
          in
            go vars (Map.insert var addr' env') from' to'
        CESKStoreForward addr' ->
            go vars (Map.insert var addr' env') from to

ceskScavengeState :: GCCycle -> CESK GCCycle
ceskScavengeState to = do
  undefined

ceskScavenge :: GCCycle -> CESK GCCycle
ceskScavenge to = do
  undefined


{-

-- | Changes the collection color of a state.
-- The supplied state is expected to be all black at this point,
-- so recolor also acts as a validator of this requirement.
recolor :: CESKStoreColor -> CESKState -> CESKState
recolor color (CESKState exp env (CESKStore space supply size) cont) =
  (CESKState exp env (CESKStore space' supply size) cont)
  where
    space' = Map.map colorItem space
    colorItem = \case
      CESKStoreVal CESKStoreBlack val -> CESKStoreVal color val
      CESKStoreVal badColor _ -> error $ "bad color " <> show badColor
      CESKStoreForward _ -> error "forward found while recoloring"

-- | Evacuates a state.
-- All the root items (those external to the store) are located
-- and copied to the new state "to-space".
evacuateState :: CESKState -> (CESKState, CESKState)
evacuateState (CESKState exp env store cont) =
  (CESKState exp env from'' cont', CESKState exp env' to'' cont')
  where
    (env', from', to') = evacuateEnv env store ceskStoreEmpty
    (cont', from'', to'') = evacuateCont cont from' to'

-- | Evacuates a continuation.
evacuateCont :: CESKCont -> CESKStore -> CESKStore -> (CESKCont, CESKStore, CESKStore)
evacuateCont = go
  where
    -- go :: CESKCont -> CESKStore -> CESKStore -> (CESKCont, CESKStore, CESKStore)
    go CESKHalt storeFrom storeTo =
      (CESKHalt, storeFrom, storeTo)
    go (CESKCont var exp env cont) storeFrom storeTo =
      let
        (env', from0, to0) = evacuateEnv env storeFrom storeTo
        (cont', from', to') = go cont from0 to0
      in
        (CESKCont var exp env' cont', from', to')

-- | Evacuates an environment.
evacuateEnv :: CESKEnv -> CESKStore -> CESKStore -> (CESKEnv, CESKStore, CESKStore)
evacuateEnv env =
  go (Map.toAscList env) env
  where
    go [] env' from to =
      (env', from, to)
    go ((var, addr):vars) env' from to =
      case ceskStoreGetItem from addr of
        CESKStoreVal _ val ->
          let
            (to', addr') = ceskStoreAlloc to val CESKStoreGray
            from' = ceskStorePutItem from addr $ CESKStoreForward addr'
          in
            go vars (Map.insert var addr' env') from' to'
        CESKStoreForward addr' ->
            go vars (Map.insert var addr' env') from to

-- | Scavenges a state.
-- The state store is searched for additional items that need to
-- be moved to "to-space".
scavengeState :: CESKState -> CESKState -> (CESKState, CESKState)
scavengeState stateFrom stateTo =
  ( CESKState exp0 env0 store0' cont0
  , CESKState exp1 env1 store1' cont1
  )
  where
    (CESKState exp0 env0 store0 cont0) = stateFrom
    (CESKState exp1 env1 store1 cont1) = stateTo
    (store0', store1') = scavenge store0 store1

-- | Scavenges a store.
scavenge :: CESKStore -> CESKStore -> (CESKStore, CESKStore)
scavenge storeFrom storeTo =
  case grayItem of
    Just (addr, CESKStoreVal _ (CESKValClos lam env)) ->
      let
        (env', from', to') = evacuateEnv env storeFrom storeTo
        to'' = ceskStorePutItem to' addr $ CESKStoreVal CESKStoreBlack $ CESKValClos lam env'
      in
        scavenge from' to''
    Just (addr, CESKStoreVal _ (CESKValCont cont)) ->
      let
        (cont', from', to') = evacuateCont cont storeFrom storeTo
        to'' = ceskStorePutItem to' addr $ CESKStoreVal CESKStoreBlack $ CESKValCont cont'
      in
        scavenge from' to''
    Just (addr, CESKStoreVal _ val) ->
      let
        to' = ceskStorePutItem storeTo addr $ CESKStoreVal CESKStoreBlack val
      in
        scavenge storeFrom to'
    Just (_, CESKStoreForward _) ->
      error "forward found while scavenging"
    Nothing ->
      (storeFrom, storeTo)
  where
    isGray (CESKStoreVal CESKStoreGray _) = True
    isGray _ = False
    (grayMap, _) = Map.partition isGray $ ceskStoreSpace storeTo
    grayItem = Map.lookupMin grayMap
-}