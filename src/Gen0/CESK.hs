--------------------------------------------------------------------------------
-- Copyright 2024 Michael P Williams. All rights reserved.
--------------------------------------------------------------------------------

module Gen0.CESK
  ( Addr(..)
  , AddrSupply
  , Cont(..)
  , Env
  , State(..)
  , Store(..)
  , StoreColor(..)
  , StoreItem(..)
  , StoreSpace
  , Val(..)
  , envEmpty
  , garbageCollect
  , run
  , runProg
  , stateSpace
  , storeEmpty
  , storeAlloc
  , storeFree
  , storeUpdate
  , storeUpdateItem
  , storeVal
  , storeItem
  ) where

import Prelude hiding (exp)
import Data.Either.Combinators (mapRight)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Data.Text (Text)
-- import Debug.Trace (trace)
import Gen0.ANF

-- | Defines an address in the store.
newtype Addr = Addr Integer deriving (Eq, Ord, Show)

-- | Defines an environment, which is a map of variables
-- to addresses in the store.
type Env = Map Var Addr

-- | Defines a store.
data Store = Store
  { storeSpace  :: StoreSpace
  , storeSupply :: AddrSupply
  , storeSize   :: Int
  }

-- | Defines a supply of addresses.
type AddrSupply = [Addr]

-- | Defines the address space in the store.
type StoreSpace = Map Addr StoreItem

-- | Defines an item in the store.
data StoreItem
  = StoreVal StoreColor Val
  | StoreForward Addr
    deriving (Eq, Ord, Show)

-- | Defines the garbage collection state for a value in the store.
data StoreColor
  = StoreWhite
  | StoreBlack
  | StoreGray
    deriving (Eq, Ord, Show)

-- | Defines a continuation.
data Cont = Cont Var Exp Env Cont | Halt deriving (Eq, Ord, Show)

-- | Defines a value.
data Val
  = ValVoid
  | ValInt Integer
  | ValBool Bool
  | ValClos Lam Env
  | ValCont Cont
    deriving (Eq, Ord, Show)

-- | Defines a machine state.
data State = State Exp Env Store Cont

-- | Returns the store space for a state.
stateSpace :: State -> StoreSpace
stateSpace (State _ _ (Store space _ _) _) = space

run :: Text -> Either Text Val
run = mapRight runProg . anfParseProg

runProg :: Prog -> Val
runProg prog =
  case x of
    State (ExpAtomic aexp) env store _ ->
      evalAtomic env store aexp
    _otherwise ->
      error "non-atomic result"
  where
    finalState = evalProg (inject prog)

    -- TODO move garbage collect into eval on some interval
    (_, x) = garbageCollect finalState

evalProg :: State -> State
evalProg state@(State (ExpAtomic _) _ _ Halt) = state
evalProg state = evalProg $ step state

-- | Defines an empty environment.
envEmpty :: Env
envEmpty = Map.empty

inject :: Prog -> State
inject (Prog decs) =
  case [exp | DecExp exp <- decs] of
    (exp:[]) -> State exp env' store' Halt
    (_:_) -> error "too many top level expressions"
    [] -> error "top level expression missing"
  where
    (env', store') = envInit decs envEmpty storeEmpty

envInit :: [Dec] -> Env -> Store -> (Env, Store)
envInit [] env store =
  (env, store)
envInit ((DecExp _):decs) env store =
  envInit decs env store
envInit ((DecDefine var (ExpAtomic (AExpLam lam))):decs) env store =
  let
    (e', s') = storeAdd var (ValClos lam envEmpty) env store
  in
    envInit decs e' s'
envInit ((DecDefine var (ExpAtomic AExpTrue)):decs) env store =
  let
    (e', s') = storeAdd var (ValBool True) env store
  in
    envInit decs e' s'
envInit ((DecDefine var (ExpAtomic AExpFalse)):decs) env store =
  let
    (e', s') = storeAdd var (ValBool False) env store
  in
    envInit decs e' s'
envInit ((DecDefine var (ExpAtomic (AExpInt i))):decs) env store =
  let
    (e', s') = storeAdd var (ValInt i) env store
  in
    envInit decs e' s'
envInit ((DecDefine _ _):_) _ _ =
  error "only lambda or constants allowed in definitions"

-- | Evaluates an atomic expression.
evalAtomic :: Env -> Store -> AExp -> Val
evalAtomic env store = \case
  AExpInt x -> ValInt x
  AExpTrue -> ValBool True
  AExpFalse -> ValBool False
  AExpLam lam -> ValClos lam env
  AExpVar var ->
    let
      addr = fromJust $ Map.lookup var env
    in
      storeVal store addr
  AExpPrim PrimAdd [aexp1, aexp2] -> evalBinary (+) aexp1 aexp2
  AExpPrim PrimSub [aexp1, aexp2] -> evalBinary (-) aexp1 aexp2
  AExpPrim PrimMul [aexp1, aexp2] -> evalBinary (*) aexp1 aexp2
  AExpPrim PrimDiv [aexp1, aexp2] -> evalBinary div aexp1 aexp2
  AExpPrim PrimEq [aexp1, aexp2] -> evalCompare (==) aexp1 aexp2
  AExpPrim prim _ -> error $ "bad args for prim " <> show prim

  where
  evalBinary :: (Integer -> Integer -> Integer) -> AExp -> AExp -> Val
  evalBinary f aexp1 aexp2 =
    let
      ValInt x = evalAtomic env store aexp1
      ValInt y = evalAtomic env store aexp2
    in
      ValInt $ f x y

  evalCompare :: (Integer -> Integer -> Bool) -> AExp -> AExp -> Val
  evalCompare f aexp1 aexp2 =
    let
      ValInt x = evalAtomic env store aexp1
      ValInt y = evalAtomic env store aexp2
    in
      ValBool $ f x y

-- | Steps the machine from a given state to the next.
step :: State -> State
step (State exp env store cont) = case exp of
  ExpLet var exp0 exp1 ->
    State exp0 env store $ Cont var exp1 env cont
  ExpAtomic aexp ->
    applyCont cont (evalAtomic env store aexp) store
  ExpComplex (CExpApp aexps) ->
    case aexps of
      (arg0:arg1:args) ->
        let
          proc = evalAtomic env store arg0
          vals = evalAtomic env store <$> (arg1:args)
        in
          applyProc proc vals store cont
      _otherwise -> error "bad application"
  ExpComplex (CExpIf aexp exp0 exp1) ->
    case evalAtomic env store aexp of
      ValBool True -> State exp0 env store cont
      ValBool False -> State exp1 env store cont
      _otherwise -> error "bad if expression"
  ExpComplex (CExpCallCC aexp) ->
    let
      proc = evalAtomic env store aexp
      valcc = ValCont cont
    in
      applyProc proc [valcc] store cont
  ExpComplex (CExpSet var aexp) ->
    let
      addr = fromJust $ Map.lookup var env
      val = evalAtomic env store aexp
      store' = storeUpdate store addr val
    in
      applyCont cont ValVoid store'
  ExpComplex (CExpLetRec bindings body) ->
    let
      (vars, vals) = unzip $ map (\(var, _) -> (var, ValVoid)) bindings
      (env', store') = storeMap vars vals env store
      vals' = map (\(_, aexp) -> evalAtomic env' store' aexp) bindings
      addrs = map (\var -> fromJust $ Map.lookup var env') vars
      store'' = foldl (\s (a, v) -> storeUpdate s a v) store' $ zip addrs vals'
    in
      State body env' store'' cont

-- | Applies a procedure.
applyProc :: Val -> [Val] -> Store -> Cont -> State
applyProc proc vals store cont =
  case proc of
    ValClos (Lam vars exp) env ->
      let
        (env', store') = storeMap vars vals env store
      in
        State exp env' store' cont
    _otherwise ->
      error "not a proc"

-- | Applies a continuation.
applyCont :: Cont -> Val -> Store -> State
applyCont (Cont var exp env cont) val store =
  State exp env' store' cont
  where
    (env', store') = storeAdd var val env store
applyCont Halt _ _ =
  error "cannot apply halt"

-- | Defines a set of variables and corresponding values by allocating
-- the value in the store and defining the variable in the environment.
storeMap :: [Var] -> [Val] -> Env -> Store -> (Env, Store)
storeMap (_:_) [] _ _ = error "missing vals"
storeMap [] (_:_) _ _ = error "missing vars"
storeMap [] [] env store = (env, store)
storeMap (var:vars) (val:vals) env store =
  storeMap vars vals env' store'
  where
  (env', store') = storeAdd var val env store

-- | Adds a variable and its value to the machine state.
storeAdd :: Var -> Val -> Env -> Store -> (Env, Store)
storeAdd var val env store =
  (env', store')
  where
    (store', addr) = storeAlloc store val StoreWhite
    env' = Map.insert var addr env

-- | Defines an empty store.
storeEmpty :: Store
storeEmpty = Store
  { storeSpace  = Map.empty
  , storeSupply = map Addr [1..]
  , storeSize   = 0
  }

-- | Allocates a value in the store and returns its address.
storeAlloc :: Store -> Val -> StoreColor -> (Store, Addr)
storeAlloc (Store _ [] _) _ _ =
  error "you've reached the end of the multiverse"
storeAlloc (Store space (addr:supply) size) val color =
  ( Store
    { storeSpace  = Map.insert addr (StoreVal color val) space
    , storeSupply = supply
    , storeSize   = size + 1
    }
  , addr
  )

-- | Frees a value from the store.
storeFree :: Store -> Addr -> Store
storeFree (Store space supply size) addr =
  Store (Map.delete addr space) supply (size - 1)

-- | Updates the value at an address in a store.
storeUpdate :: Store -> Addr -> Val -> Store
storeUpdate store addr val =
  storeUpdateItem store addr $ StoreVal StoreWhite val

-- | Updates a store item at an address in a store.
storeUpdateItem :: Store -> Addr -> StoreItem -> Store
storeUpdateItem (Store space supply size) addr item =
  Store (Map.insert addr item space) supply size

-- | Gets a value from a store.
storeVal :: Store -> Addr -> Val
storeVal store addr =
  case storeItem store addr of
    StoreVal _ val -> val
    StoreForward _ -> error "bad store value"

-- | Gets an item from a store.
storeItem :: Store -> Addr -> StoreItem
storeItem Store{..} addr = fromJust $ Map.lookup addr storeSpace

-- | Performs garbage collection on a state.
-- Returns a tuple of the modified old state (for testing purposes) and
-- the new, resulting state respectively.
garbageCollect :: State -> (State, State)
garbageCollect state =
  (stateFrom', stateTo'')
  where
    (stateFrom, stateTo) = evacuateState state
    (stateFrom', stateTo') = scavengeState stateFrom stateTo
    stateTo'' = recolor StoreWhite stateTo'

-- | Changes the collection color of a state.
-- The supplied state is expected to be all black at this point,
-- so recolor also acts as a validator of this requirement.
recolor :: StoreColor -> State -> State
recolor color (State exp env (Store space supply size) cont) =
  (State exp env (Store space' supply size) cont)
  where
    space' = Map.map colorItem space
    colorItem = \case
      StoreVal StoreBlack val -> StoreVal color val
      StoreVal badColor _ -> error $ "bad color " <> show badColor
      StoreForward _ -> error "forward found while recoloring"

-- | Evacuates a state.
-- All the root items (those external to the store) are located
-- and copied to the new state "to-space".
evacuateState :: State -> (State, State)
evacuateState (State exp env store cont) =
  (State exp env from'' cont', State exp env' to'' cont')
  where
    (env', from', to') = evacuateEnv env store storeEmpty
    (cont', from'', to'') = evacuateCont cont from' to'

-- | Evacuates a continuation.
evacuateCont :: Cont -> Store -> Store -> (Cont, Store, Store)
evacuateCont = go
  where
    -- go :: Cont -> Store -> Store -> (Cont, Store, Store)
    go Halt storeFrom storeTo =
      (Halt, storeFrom, storeTo)
    go (Cont var exp env cont) storeFrom storeTo =
      let
        (env', from0, to0) = evacuateEnv env storeFrom storeTo
        (cont', from', to') = go cont from0 to0
      in
        (Cont var exp env' cont', from', to')

-- | Evacuates an environment.
evacuateEnv :: Env -> Store -> Store -> (Env, Store, Store)
evacuateEnv env =
  go (Map.toAscList env) env
  where
    go [] env' from to =
      (env', from, to)
    go ((var, addr):vars) env' from to =
      case storeItem from addr of
        StoreVal _ val ->
          let
            (to', addr') = storeAlloc to val StoreGray
            from' = storeUpdateItem from addr $ StoreForward addr'
          in
            go vars (Map.insert var addr' env') from' to'
        StoreForward addr' ->
            go vars (Map.insert var addr' env') from to

-- | Scavenges a state.
-- The state store is searched for additional items that need to
-- be moved to "to-space".
scavengeState :: State -> State -> (State, State)
scavengeState stateFrom stateTo =
  ( State exp0 env0 store0' cont0
  , State exp1 env1 store1' cont1
  )
  where
    (State exp0 env0 store0 cont0) = stateFrom
    (State exp1 env1 store1 cont1) = stateTo
    (store0', store1') = scavenge store0 store1

-- | Scavenges a store.
scavenge :: Store -> Store -> (Store, Store)
scavenge storeFrom storeTo =
  case grayItem of
    Just (addr, StoreVal _ (ValClos lam env)) ->
      let
        (env', from', to') = evacuateEnv env storeFrom storeTo
        to'' = storeUpdateItem to' addr $ StoreVal StoreBlack $ ValClos lam env'
      in
        scavenge from' to''
    Just (addr, StoreVal _ (ValCont cont)) ->
      let
        (cont', from', to') = evacuateCont cont storeFrom storeTo
        to'' = storeUpdateItem to' addr $ StoreVal StoreBlack $ ValCont cont'
      in
        scavenge from' to''
    Just (addr, StoreVal _ val) ->
      let
        to' = storeUpdateItem storeTo addr $ StoreVal StoreBlack val
      in
        scavenge storeFrom to'
    Just (_, StoreForward _) ->
      error "forward found while scavenging"
    Nothing ->
      (storeFrom, storeTo)
  where
    isGray (StoreVal StoreGray _) = True
    isGray _ = False
    (grayMap, _) = Map.partition isGray $ storeSpace storeTo
    grayItem = Map.lookupMin grayMap
