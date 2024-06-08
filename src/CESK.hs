--------------------------------------------------------------------------------
-- Copyright 2024 Michael P Williams. All rights reserved.
--------------------------------------------------------------------------------

module CESK
  ( run
  ) where

import ANF
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromJust, fromMaybe)
import Debug.Trace (trace)

newtype Addr = Addr Integer deriving (Eq, Ord, Show)

type Env = Map Var Addr

type Store = Map Addr Val

data Cont
  = Cont Var Exp Env Cont
  | Halt
    deriving (Show)


data Val
  = ValVoid
  | ValInt Integer
  | ValBool Bool
  | ValClos Lam Env
  | ValCont Cont
    deriving (Show)

data State = State Exp Env Store Cont deriving (Show)

run :: Prog -> Val
run prog =
  case evalProg (inject prog) of
    State (ExpAtomic aexp) env store _ ->
      evalAtomic env store aexp

evalProg :: State -> State
evalProg state@(State (ExpAtomic _) _ _ Halt) =
  state
  -- trace ("state: " <> show state) $ state
evalProg state = evalProg $ step state

inject :: Prog -> State
inject (Prog ((DecExp exp):[])) = State exp Map.empty Map.empty Halt

evalAtomic :: Env -> Store -> AExp -> Val
evalAtomic env store = \case
  AExpInt x -> ValInt x
  AExpTrue -> ValBool True
  AExpFalse -> ValBool False
  AExpLam lam -> ValClos lam env
  AExpVar var ->
    let
      -- v = Map.lookup var env
      -- addr = fromJust $ trace ("var: " <> show var <> " v: " <> show v <> " env: " <> show env) v
      addr = fromJust $ Map.lookup var env
    in
      fromJust $ Map.lookup addr store
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

step :: State -> State
step (State exp env store cont) = case exp of
  ExpLet var exp0 exp1 ->
    State exp0 env store $ Cont var exp1 env cont
  ExpAtomic aexp ->
    applyCont cont (evalAtomic env store aexp) store
  ExpComplex cexp ->
    case cexp of
      CExpApp aexps ->
        case aexps of
          (arg0:arg1:args) ->
            let
              proc = evalAtomic env store arg0
              vals = evalAtomic env store <$> (arg1:args)
            in
              applyProc proc vals store cont
          _otherwise -> error "bad application"
      CExpIf aexp exp0 exp1 ->
        case evalAtomic env store aexp of
          ValBool True -> State exp0 env store cont
          ValBool False -> State exp1 env store cont
          _otherwise -> error "bad if expression"
      CExpCallCC aexp ->
        let
          proc = evalAtomic env store aexp
          valcc = ValCont cont
        in
          applyProc proc [valcc] store cont
      CExpSet var aexp ->
        let
          addr = fromJust $ Map.lookup var env
          val = evalAtomic env store aexp
          store' = Map.insert addr val store
        in
          applyCont cont ValVoid store'
      CExpLetRec bindings body ->
        let
          (vars, vals) = unzip $ map (\(var, aexp) -> (var, ValVoid)) bindings
          (env', store') = storeMap vars vals env store
          vals' = map (\(_, aexp) -> evalAtomic env' store' aexp) bindings
          addrs = map (\var -> fromJust $ Map.lookup var env') vars
          store'' = Map.union (Map.fromList $ zip addrs vals') store'
        in
          State body env' store'' cont

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

applyCont :: Cont -> Val -> Store -> State
applyCont (Cont var exp env cont) val store =
  State exp env' store' cont
  where
    (env', store') = storeAdd var val env store
applyCont Halt _ _ =
  error "cannot apply halt"

storeMap :: [Var] -> [Val] -> Env -> Store -> (Env, Store)
storeMap (var:vars) [] _ _ = error "missing vals"
storeMap [] (val:vals) _ _ = error "missing vars"
storeMap [] [] env store = (env, store)
storeMap (var:vars) (val:vals) env store =
  storeMap vars vals env' store'
  where
  (env', store') = storeAdd var val env store

storeAdd :: Var -> Val -> Env -> Store -> (Env, Store)
storeAdd var val env store =
  (env', store'')
  where
    (store', addr) = newAddr store
    store'' = Map.insert addr val store'
    env' = Map.insert var addr env

newAddr :: Store -> (Store, Addr)
newAddr store =
  (store', Addr a')
  where
    ValInt a = fromMaybe (ValInt 0) $ Map.lookup (Addr 0) store
    a' = a + 1
    store' = Map.insert (Addr 0) (ValInt a') store
