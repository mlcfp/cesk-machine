--------------------------------------------------------------------------------
-- Copyright 2024 Michael P Williams. All rights reserved.
--------------------------------------------------------------------------------

module Gen1.Types
  ( CESK
  , CESKAddr(..)
  , CESKArity(..)
  , CESKCont(..)
  , CESKEnv
  , CESKError(..)
  , CESKIntrinsic(..)
  , CESKIntrinsicFunc
  , CESKMachine(..)
  , CESKOptions(..)
  , CESKState(..)
  , CESKStatistics(..)
  , CESKStore(..)
  , CESKStoreColor(..)
  , CESKStoreItem(..)
  , CESKStoreSpace
  , CESKVal(..)
  , envEmpty
  , ceskDefaultMachine
  , ceskDefaultOptions
  , ceskDefaultState
  , ceskDefaultStatistics
  , ceskErrorHumanize
  , ceskValDesc
  , ceskValHumanize
  , modifyStatistics
  , modifyState
  , println
  , stateSpace
  , storeEmpty
  ) where

import Prelude hiding (exp)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Control.Monad.Except (ExceptT(..))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (StateT(..), modify)
import Gen1.ANF
import Gen1.Util

-- | Defines the execution monad.
type CESK = ExceptT CESKError (StateT CESKMachine IO)

-- | Defines the evaluation state.
data CESKMachine = CESKMachine
  { ceskMachineOptions    :: CESKOptions
  , ceskMachineStatistics :: CESKStatistics
  , ceskMachineState      :: CESKState
  } deriving (Eq, Ord, Show)

-- | Defines options for the execution of the machine.
data CESKOptions = CESKOptions
  { ceskOptionIntrinsicBindings :: Bool
  , ceskOptionTraceState        :: Bool
  } deriving (Eq, Ord, Show)

-- | Defines various statistics used to monitor and control
-- the machine.
data CESKStatistics = CESKStatistics
  { ceskStepCountLimit :: Integer
  -- ^ The step count at which to perform garbage collection.
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
  | CESKValFloat Double
  | CESKValBool Bool
  | CESKValStr Text
  | CESKValChar Char
  | CESKValClos ANFLam CESKEnv
  | CESKValCont CESKCont
  | CESKValNull
  | CESKValPair CESKVal CESKVal
    deriving (Eq, Ord, Show)

-- | Returns a description of a value.
ceskValDesc :: CESKVal -> Text
ceskValDesc = \case
  CESKValVoid  {} -> "void"
  CESKValInt   {} -> "int"
  CESKValFloat {} -> "float"
  CESKValBool  {} -> "bool"
  CESKValStr   {} -> "string"
  CESKValChar  {} -> "char"
  CESKValClos  {} -> "closure"
  CESKValCont  {} -> "continuation"
  CESKValNull  {} -> "null"
  CESKValPair  {} -> "pair"

-- | Defines the arity for an intrinsic function.
-- Fixed specifies a set number of arguments to the function.
-- Any specifies an unlimited number of arguments.
data CESKArity
  = CESKArityFixed Int
  | CESKArityAny
    deriving (Eq, Ord, Show)

-- | Defines the signature for an intrinsic function.
type CESKIntrinsicFunc = [CESKVal] -> CESK CESKVal

-- | Defines an intrinsic function.
data CESKIntrinsic = CESKIntrinsic
  { ceskIntrinsicName   :: Text
  , ceskIntrinsicPublic :: Text
  , ceskIntrinsicArity  :: CESKArity
  , ceskIntrinsicFunc   :: CESKIntrinsicFunc
  }

-- | Defines the initial optins.
ceskDefaultOptions :: CESKOptions
ceskDefaultOptions = CESKOptions
  { ceskOptionIntrinsicBindings = False
  , ceskOptionTraceState        = False
  }

-- | Defines the initial statistics.
ceskDefaultStatistics :: CESKStatistics
ceskDefaultStatistics = CESKStatistics
  { ceskStepCountLimit = 200
  , ceskStepCountGC    = 0
  , ceskStepCountTotal = 0
  }

-- | Defines the initial state.
ceskDefaultState :: CESKState
ceskDefaultState = CESKState
  { ceskStateExp   = ANFExpAtomic ANFAtomicVoid
  , ceskStateEnv   = envEmpty
  , ceskStateStore = storeEmpty
  , ceskStateCont  = CESKHalt
  }

-- | Defines the initial machine.
ceskDefaultMachine :: CESKMachine
ceskDefaultMachine = CESKMachine
  { ceskMachineOptions    = ceskDefaultOptions
  , ceskMachineStatistics = ceskDefaultStatistics
  , ceskMachineState      = ceskDefaultState
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

-- | Defines an empty environment.
envEmpty :: CESKEnv
envEmpty = Map.empty

-- | Defines an empty store.
storeEmpty :: CESKStore
storeEmpty = CESKStore
  { ceskStoreSpace = Map.empty
  , ceskStoreAddr  = 0
  , ceskStoreSize  = 0
  }

-- | Prints text to the console.
println :: Text -> CESK ()
println = liftIO . putStrLn . T.unpack

-- | Defines an error.
data CESKError
  = CESKErrorParse Text
  | CESKErrorTopLevelNone
  | CESKErrorTopLevelMultiple
  | CESKErrorDefinitionBad ANFVar
  | CESKErrorIntrinsicBad Text
  | CESKErrorIntrinsicCall Text
  | CESKErrorIntrinsicArgs
  | CESKErrorPrimitiveArgs ANFPrim
  | CESKErrorPrimitiveBad Text
  | CESKErrorVar ANFVar
  | CESKErrorApplication
  | CESKErrorIfExpression
  | CESKErrorProcedure
  | CESKErrorHaltApplication
  | CESKErrorMissingVars
  | CESKErrorMissingVals
  | CESKErrorAddressBad CESKAddr
  | CESKErrorFreeBad CESKAddr
  | CESKErrorUnexpectedForward CESKAddr
  | CESKErrorUnexpectedColor CESKStoreColor
  | CESKErrorListValue Text
    deriving (Eq, Ord, Show)

-- | Renders an error in human compatible form.
ceskErrorHumanize :: CESKError -> Text
ceskErrorHumanize = \case
  CESKErrorParse message ->
    message
  CESKErrorTopLevelNone ->
    "top level expression missing"
  CESKErrorTopLevelMultiple ->
    "too many top level expressions"
  CESKErrorDefinitionBad (ANFVar name) ->
    "bad definition: " <> name
  CESKErrorIntrinsicBad name ->
    "bad intrinsic: " <> name
  CESKErrorIntrinsicCall name ->
    "bad call to " <> name
  CESKErrorIntrinsicArgs ->
    "bad intrinsic call"
  CESKErrorPrimitiveBad message ->
    message
  CESKErrorPrimitiveArgs prim ->
    "bad args for primitive " <> textShow prim
  CESKErrorVar (ANFVar name) ->
    "bad var " <> name
  CESKErrorApplication ->
    "bad application"
  CESKErrorIfExpression ->
    "bad if expression"
  CESKErrorProcedure ->
    "bad procedure"
  CESKErrorHaltApplication ->
    "cannot apply halt"
  CESKErrorMissingVars ->
    "missing vars"
  CESKErrorMissingVals ->
    "missing vals"
  CESKErrorAddressBad addr ->
    "bad address " <> textShow addr
  CESKErrorFreeBad addr ->
    "free bad addr " <> textShow addr
  CESKErrorUnexpectedForward addr ->
    "unexpected forward " <> textShow addr
  CESKErrorUnexpectedColor color ->
    "unexpected color " <> textShow color
  CESKErrorListValue val ->
    "bad value in list: " <> val

-- | Renders a value in human compatible form.
ceskValHumanize :: CESKVal -> Text
ceskValHumanize val = case val of
  CESKValVoid ->
    "#void"
  CESKValInt integer ->
    textShow integer
  CESKValFloat double ->
    textShow double
  CESKValBool bool ->
    if bool then "#t" else "#f"
  CESKValStr string ->
    string
  CESKValChar char ->
    T.singleton char
  CESKValClos{} ->
    ceskValDesc val
  CESKValCont{} ->
    ceskValDesc val
  CESKValNull ->
    "'()"
  CESKValPair a b ->
    "(" <> ceskValHumanize a <> " . " <> ceskValHumanize b <> ")"
