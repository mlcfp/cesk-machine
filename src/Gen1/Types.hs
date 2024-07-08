--------------------------------------------------------------------------------
-- Copyright 2024 Michael P Williams. All rights reserved.
--------------------------------------------------------------------------------

module Gen1.Types
  ( CESK
  , CESKAddr(..)
  , CESKArity(..)
  , CESKCont(..)
  , CESKEnv
  , CESKIntrinsic(..)
  , CESKMachine(..)
  , CESKState(..)
  , CESKStatistics(..)
  , CESKStore(..)
  , CESKStoreColor(..)
  , CESKStoreItem(..)
  , CESKStoreSpace
  , CESKVal(..)
  , envEmpty
  , ceskValDesc
  , initialStatistics
  , initialState
  , initialMachine
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
  | CESKValFloat Double
  | CESKValBool Bool
  | CESKValStr Text
  | CESKValChar Char
  | CESKValClos ANFLam CESKEnv
  | CESKValCont CESKCont
    deriving (Eq, Ord, Show)

-- | Returns a description of a value.
ceskValDesc :: CESKVal -> Text
ceskValDesc = \case
  CESKValVoid  {} -> "void"
  CESKValInt   {} -> "int"
  CESKValFloat {} -> "float"
  CESKValBool  {} -> "bool"
  CESKValStr   {} -> "str"
  CESKValChar  {} -> "char"
  CESKValClos  {} -> "closure"
  CESKValCont  {} -> "continuation"

-- | Defines the arity for an intrinsic function.
newtype CESKArity = CESKArity Int deriving (Eq, Ord, Show)

-- | Defines an intrinsic function.
data CESKIntrinsic = CESKIntrinsic
  { ceskIntrinsicName   :: Text
  , ceskIntrinsicPublic :: Text
  , ceskIntrinsicArity  :: CESKArity
  , ceskIntrinsicFunc   :: [CESKVal] -> CESK CESKVal
  }

-- | Defines the initial statistics.
initialStatistics :: CESKStatistics
initialStatistics = CESKStatistics
  { ceskStepCountLimit = 200
  , ceskStepCountGC    = 0
  , ceskStepCountTotal = 0
  }

-- | Defines the initial state.
initialState :: CESKState
initialState = CESKState
  { ceskStateExp   = ANFExpAtomic ANFAtomicVoid
  , ceskStateEnv   = envEmpty
  , ceskStateStore = storeEmpty
  , ceskStateCont  = CESKHalt
  }

-- | Defines the initial machine.
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
