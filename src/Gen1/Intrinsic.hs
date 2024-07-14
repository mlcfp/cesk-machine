--------------------------------------------------------------------------------
-- Copyright 2024 Michael P Williams. All rights reserved.
--------------------------------------------------------------------------------

module Gen1.Intrinsic
  ( ceskIntrinsicWrappers
  , intrinsicMap
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes)
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
  fmap catMaybes $ forM (Map.elems intrinsicMap) $
    \(CESKIntrinsic n p a _) -> case a of
      CESKArityFixed i ->
        Just <$> ceskWrapperDec n p i
      _otherwise ->
        pure Nothing

-- | Creates a declaration wrapper for an intrinsic function.
-- The purpose of the wrapper is to provide a more ergonomic
-- name, and to define the name in the environment so it can
-- be overridden.
ceskWrapperDec :: Text -> Text -> Int -> CESK ANFDec
ceskWrapperDec name name' x = do
  vars <- forM [1..x] $ \i -> do
    pure $ ANFVar $ "x" <> textShow i
  pure $ ANFDecDefine (ANFVar name') $
    ANFExpAtomic (ANFAtomicLam $ ANFLam vars
      (ANFExpAtomic $ ANFAtomicPrim (ANFPrimFunc name) $
        map ANFAtomicVar vars))

-- | A map of intrinsic functions keyed by name.
intrinsicMap :: Map Text CESKIntrinsic
intrinsicMap = Map.fromList $ map (\(n, p, a, f) ->
  (n, CESKIntrinsic n p
    (if a == -1 then CESKArityAny else CESKArityFixed a) f))
  [ ("@sin", "sin", 1, mathUnary sin)
  , ("@cos", "cos", 1, mathUnary cos)
  , ("@tan", "tan", 1, mathUnary tan)
  , ("@asin", "asin", 1, mathUnary asin)
  , ("@acos", "acos", 1, mathUnary acos)
  , ("@atan", "atan", 1, mathUnary atan)
  , ("@sinh", "sinh", 1, mathUnary sinh)
  , ("@cosh", "cosh", 1, mathUnary cosh)
  , ("@tanh", "tanh", 1, mathUnary tanh)
  , ("@asinh", "asinh", 1, mathUnary asinh)
  , ("@acosh", "acosh", 1, mathUnary acosh)
  , ("@atanh", "atanh", 1, mathUnary atanh)
  , ("@exp", "exp", 1, mathUnary exp)
  , ("@log", "log", 1, mathUnary log)
  , ("@sqrt", "sqrt", 1, mathUnary sqrt)
  , ("@pi", "pi", 0, mathNone pi)

  , ("@string-length", "string-length", 1, strLen)
  , ("@string-char", "string-char", 2, strChar)
  , ("@string-upper", "string-upper", 1, strUpper)
  , ("@string-lower", "string-lower", 1, strLower)
  , ("@string-list", "string-list", 1, strList)
  , ("@string-make", "string-make", 1, strMake)
  , ("@string-append", "string-append", 2, strAppend)
  , ("@string-part", "string-part", 3, strPart)

  , ("@char?", "char?", 1, testChar)
  , ("@string?", "string?", 1, testStr)
  , ("@int?", "int?", 1, testInt)
  , ("@float?", "float?", 1, testFloat)
  , ("@number?", "number?", 1, testNumber)
  , ("@bool?", "bool?", 1, testBool)
  , ("@void?", "void?", 1, testVoid)
  , ("@pair?", "pair?", 1, testPair)

  , ("@null", "null", 0, pairNull)
  , ("@cons", "cons", 2, pairCons)
  , ("@head", "head", 1, pairHead)
  , ("@tail", "tail", 1, pairTail)
  -- Do not make a wrapper for list, since the arity is variable.
  , ("@list", "list", -1, pairList)
  ]

-- | Runs a math function that takes no argument.
mathNone :: Double -> [CESKVal] -> CESK CESKVal
mathNone f = \case
  [] -> pure $ CESKValFloat f
  _ -> throwError CESKErrorIntrinsicArgs

-- | Runs a math function that takes one argument.
mathUnary :: (Double -> Double) -> [CESKVal] -> CESK CESKVal
mathUnary f = \case
  (CESKValInt x):[] ->
    pure $ CESKValFloat $ f $ fromIntegral x
  (CESKValFloat x):[] ->
    pure $ CESKValFloat $ f x
  _otherwise ->
    throwError CESKErrorIntrinsicArgs

-- | String length intrinsic.
strLen :: [CESKVal] -> CESK CESKVal
strLen = \case
  (CESKValStr x):[] ->
    pure $ CESKValInt $ fromIntegral $ T.length x
  _otherwise ->
    throwError CESKErrorIntrinsicArgs

-- | String element access intrinsic.
strChar :: [CESKVal] -> CESK CESKVal
strChar = \case
  (CESKValStr x):(CESKValInt i):[] ->
    pure $ CESKValChar $ T.index x $ fromIntegral i
  _otherwise ->
    throwError CESKErrorIntrinsicArgs

-- | String uppercase intrinsic.
strUpper :: [CESKVal] -> CESK CESKVal
strUpper = \case
  (CESKValStr x):[] ->
    pure $ CESKValStr $ T.toUpper x
  _otherwise ->
    throwError CESKErrorIntrinsicArgs

-- | String lowercase intrinsic.
strLower :: [CESKVal] -> CESK CESKVal
strLower = \case
  (CESKValStr x):[] ->
    pure $ CESKValStr $ T.toLower x
  _otherwise ->
    throwError CESKErrorIntrinsicArgs

-- | String to list intrinsic.
strList :: [CESKVal] -> CESK CESKVal
strList = \case
  (CESKValStr x):[] ->
    pure $ go $ T.unpack x
  _otherwise ->
    throwError CESKErrorIntrinsicArgs
  where
    go :: String -> CESKVal
    go [] = CESKValNull
    go (c:cs) = CESKValPair (CESKValChar c) (go cs)

-- | String from list intrinsic.
strMake :: [CESKVal] -> CESK CESKVal
strMake = \case
  CESKValNull:[] ->
    pure $ CESKValStr T.empty
  (x@CESKValPair{}):[] ->
    CESKValStr . T.pack <$> go x
  _otherwise ->
    throwError CESKErrorIntrinsicArgs
  where
    go :: CESKVal -> CESK String
    go = \case
      CESKValNull -> pure []
      CESKValPair (CESKValChar c) x -> go x >>= (\s -> pure $ c : s)
      val -> throwError $ CESKErrorListValue $ textShow val

-- | String append intrinsic.
strAppend :: [CESKVal] -> CESK CESKVal
strAppend = \case
  (CESKValStr x):(CESKValStr y):[] ->
    pure $ CESKValStr $ x <> y
  _otherwise ->
    throwError CESKErrorIntrinsicArgs

-- | String part selection intrinsic.
strPart :: [CESKVal] -> CESK CESKVal
strPart = \case
  (CESKValStr x):(CESKValInt i):(CESKValInt j):[] ->
    pure $ CESKValStr $ T.take (fromIntegral j) $ T.drop (fromIntegral i) x
  _otherwise ->
    throwError CESKErrorIntrinsicArgs

-- | Tests whether a value is a char.
testChar :: [CESKVal] -> CESK CESKVal
testChar = \case
  (CESKValChar _):[] ->
    pure $ CESKValBool True
  (_:[]) ->
    pure $ CESKValBool False
  _otherwise ->
    throwError CESKErrorIntrinsicArgs

-- | Tests whether a value is a string.
testStr :: [CESKVal] -> CESK CESKVal
testStr = \case
  (CESKValStr _):[] ->
    pure $ CESKValBool True
  (_:[]) ->
    pure $ CESKValBool False
  _otherwise ->
    throwError CESKErrorIntrinsicArgs

-- | Tests whether a value is an integer.
testInt :: [CESKVal] -> CESK CESKVal
testInt = \case
  (CESKValInt _):[] ->
    pure $ CESKValBool True
  (_:[]) ->
    pure $ CESKValBool False
  _otherwise ->
    throwError CESKErrorIntrinsicArgs

-- | Tests whether a value is a float.
testFloat :: [CESKVal] -> CESK CESKVal
testFloat = \case
  (CESKValFloat _):[] ->
    pure $ CESKValBool True
  (_:[]) ->
    pure $ CESKValBool False
  _otherwise ->
    throwError CESKErrorIntrinsicArgs

-- | Tests whether a value is a number.
testNumber :: [CESKVal] -> CESK CESKVal
testNumber = \case
  (CESKValInt _):[] ->
    pure $ CESKValBool True
  (CESKValFloat _):[] ->
    pure $ CESKValBool True
  (_:[]) ->
    pure $ CESKValBool False
  _otherwise ->
    throwError CESKErrorIntrinsicArgs

-- | Tests whether a value is a boolean.
testBool :: [CESKVal] -> CESK CESKVal
testBool = \case
  (CESKValBool _):[] ->
    pure $ CESKValBool True
  (_:[]) ->
    pure $ CESKValBool False
  _otherwise ->
    throwError CESKErrorIntrinsicArgs

-- | Tests whether a value is a void.
testVoid :: [CESKVal] -> CESK CESKVal
testVoid = \case
  (CESKValVoid:[]) ->
    pure $ CESKValBool True
  (_:[]) ->
    pure $ CESKValBool False
  _otherwise ->
    throwError CESKErrorIntrinsicArgs

-- | Tests whether a value is a pair.
testPair :: [CESKVal] -> CESK CESKVal
testPair = \case
  (CESKValPair{}:[]) ->
    pure $ CESKValBool True
  (_:[]) ->
    pure $ CESKValBool False
  _otherwise ->
    throwError CESKErrorIntrinsicArgs

-- | Creates a null.
pairNull :: [CESKVal] -> CESK CESKVal
pairNull = \case
  [] -> pure $ CESKValNull
  _ -> throwError CESKErrorIntrinsicArgs

-- | Creates a pair.
pairCons :: [CESKVal] -> CESK CESKVal
pairCons = \case
  (a:b:[]) -> pure $ CESKValPair a b
  _ -> throwError CESKErrorIntrinsicArgs

-- | Gets the first value in a pair.
pairHead :: [CESKVal] -> CESK CESKVal
pairHead = \case
  (CESKValPair a b):[] -> pure a
  _ -> throwError CESKErrorIntrinsicArgs

-- | Gets the second value in a pair.
pairTail :: [CESKVal] -> CESK CESKVal
pairTail = \case
  (CESKValPair a b):[] -> pure a
  _ -> throwError CESKErrorIntrinsicArgs

-- | Creates a list.
pairList :: [CESKVal] -> CESK CESKVal
pairList vals =
  pure $ go vals
  where
    go [] = CESKValNull
    go (x:[]) = CESKValPair x CESKValNull
    go (x:xs) = CESKValPair x $ go xs
