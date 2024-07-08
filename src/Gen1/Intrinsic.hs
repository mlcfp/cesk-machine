--------------------------------------------------------------------------------
-- Copyright 2024 Michael P Williams. All rights reserved.
--------------------------------------------------------------------------------

module Gen1.Intrinsic
  ( ceskIntrinsicWrappers
  , intrinsicMap
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Control.Monad (forM)
import Control.Monad.Except (throwError)
import Gen1.ANF
import Gen1.Types
import Gen1.Util

-- | Create declaration wrappers for all the intrinsics.
ceskIntrinsicWrappers :: CESK [ANFDec]
ceskIntrinsicWrappers = do
  forM (Map.elems intrinsicMap) $ \(CESKIntrinsic n p a _) -> do
    ceskWrapperDec n p a

-- | Creates a declaration wrapper for an intrinsic function.
-- The purpose of the wrapper is to provide a more ergonomic
-- name, and to define the name in the environment so it can
-- be overridden.
ceskWrapperDec :: Text -> Text -> CESKArity -> CESK ANFDec
ceskWrapperDec name name' (CESKArity x) = do
  vars <- forM [1..x] $ \i -> do
    pure $ ANFVar $ "x" <> textShow i
  pure $ ANFDecDefine (ANFVar name') $
    ANFExpAtomic (ANFAtomicLam $ ANFLam vars
      (ANFExpAtomic $ ANFAtomicPrim (ANFPrimFunc name) $
        map ANFAtomicVar vars))

-- | A map of intrinsic functions keyed by name.
intrinsicMap :: Map Text CESKIntrinsic
intrinsicMap = Map.fromList $ map (\i -> (ceskIntrinsicName i, i))
  [ g 1 "@sin" "sin" $ mathUnary sin
  , g 1 "@cos" "cos" $ mathUnary cos
  , g 1 "@tan" "tan" $ mathUnary tan
  , g 1 "@asin" "asin" $ mathUnary asin
  , g 1 "@acos" "acos" $ mathUnary acos
  , g 1 "@atan" "atan" $ mathUnary atan
  , g 1 "@sinh" "sinh" $ mathUnary sinh
  , g 1 "@cosh" "cosh" $ mathUnary cosh
  , g 1 "@tanh" "tanh" $ mathUnary tanh
  , g 1 "@asinh" "asinh" $ mathUnary asinh
  , g 1 "@acosh" "acosh" $ mathUnary acosh
  , g 1 "@atanh" "atanh" $ mathUnary atanh
  , g 1 "@exp" "exp" $ mathUnary exp
  , g 1 "@log" "log" $ mathUnary log
  , g 1 "@sqrt" "sqrt" $ mathUnary sqrt
  , g 0 "@pi" "pi" $ mathNone pi

  , g 1 "@strlen" "strlen" strLen
  , g 2 "@strchar" "strchar" strChar

  , g 1 "@chr?" "chr?" testChar
  , g 1 "@str?" "str?" testStr
  , g 1 "@int?" "int?" testInt
  , g 1 "@float?" "float?" testFloat
  , g 1 "@bool?" "bool?" testBool
  , g 1 "@void?" "void?" testVoid
  ]
  where
    g i n p = CESKIntrinsic n p (CESKArity i)


-- | Runs a math function that takes no argument.
mathNone :: Double -> [CESKVal] -> CESK CESKVal
mathNone f = \case
  [] ->
    pure $ CESKValFloat f
  _otherwise ->
    throwError "bad intrinsic call"

-- | Runs a math function that takes one argument.
mathUnary :: (Double -> Double) -> [CESKVal] -> CESK CESKVal
mathUnary f = \case
  (CESKValInt x):[] ->
    pure $ CESKValFloat $ f $ fromIntegral x
  (CESKValFloat x):[] ->
    pure $ CESKValFloat $ f x
  _otherwise ->
    throwError "bad intrinsic call"

-- | String length intrinsic.
strLen :: [CESKVal] -> CESK CESKVal
strLen = \case
  (CESKValStr x):[] ->
    pure $ CESKValInt $ fromIntegral $ T.length x
  _otherwise ->
    throwError "bad intrinsic call"

-- | String element access intrinsic.
strChar :: [CESKVal] -> CESK CESKVal
strChar = \case
  (CESKValStr x):(CESKValInt i):[] ->
    pure $ CESKValChar $ T.index x $ fromIntegral i
  _otherwise ->
    throwError "bad intrinsic call"

-- | Tests whether a value is a char.
testChar :: [CESKVal] -> CESK CESKVal
testChar = \case
  (CESKValChar _):[] ->
    pure $ CESKValBool True
  (_:[]) ->
    pure $ CESKValBool False
  _otherwise ->
    throwError "bad intrinsic call"

-- | Tests whether a value is a string.
testStr :: [CESKVal] -> CESK CESKVal
testStr = \case
  (CESKValStr _):[] ->
    pure $ CESKValBool True
  (_:[]) ->
    pure $ CESKValBool False
  _otherwise ->
    throwError "bad intrinsic call"

-- | Tests whether a value is an integer.
testInt :: [CESKVal] -> CESK CESKVal
testInt = \case
  (CESKValInt _):[] ->
    pure $ CESKValBool True
  (_:[]) ->
    pure $ CESKValBool False
  _otherwise ->
    throwError "bad intrinsic call"

-- | Tests whether a value is a float.
testFloat :: [CESKVal] -> CESK CESKVal
testFloat = \case
  (CESKValFloat _):[] ->
    pure $ CESKValBool True
  (_:[]) ->
    pure $ CESKValBool False
  _otherwise ->
    throwError "bad intrinsic call"

-- | Tests whether a value is a boolean.
testBool :: [CESKVal] -> CESK CESKVal
testBool = \case
  (CESKValBool _):[] ->
    pure $ CESKValBool True
  (_:[]) ->
    pure $ CESKValBool False
  _otherwise ->
    throwError "bad intrinsic call"

-- | Tests whether a value is a void.
testVoid :: [CESKVal] -> CESK CESKVal
testVoid = \case
  (CESKValVoid:[]) ->
    pure $ CESKValBool True
  (_:[]) ->
    pure $ CESKValBool False
  _otherwise ->
    throwError "bad intrinsic call"
