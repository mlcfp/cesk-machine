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
import Text.Read (readMaybe)

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
intrinsicMap =
  Map.fromList $ map (\(n, a, f) ->
    (name n, CESKIntrinsic (name n) n (arity a) f))
    intrinsicList
  where
    name x = "@" <> x
    arity x
      | x == -1 = CESKArityAny
      | otherwise = CESKArityFixed x

-- | The list of available built-in functions.
intrinsicList :: [(Text, Int, CESKIntrinsicFunc)]
intrinsicList = concat
  [ typeIntrinsics
  , pairIntrinsics
  , mathIntrinsics
  , stringIntrinsics
  , convIntrinsics
  ]

-- | The list of available built-in type functions.
typeIntrinsics :: [(Text, Int, CESKIntrinsicFunc)]
typeIntrinsics =
  [ ("char?",   1, testChar)
  , ("string?", 1, testStr)
  , ("int?",    1, testInt)
  , ("float?",  1, testFloat)
  , ("number?", 1, testNumber)
  , ("bool?",   1, testBool)
  , ("void?",   1, testVoid)
  , ("pair?",   1, testPair)
  ]

-- | The list of available built-in pair functions.
pairIntrinsics :: [(Text, Int, CESKIntrinsicFunc)]
pairIntrinsics =
  [ ("null",  0, pairNull)
  , ("cons",  2, pairCons)
  , ("head",  1, pairHead)
  , ("tail",  1, pairTail)
  , ("list", -1, pairList)
  ]

-- | The list of available built-in math functions.
mathIntrinsics :: [(Text, Int, CESKIntrinsicFunc)]
mathIntrinsics =
  [ ("sin",   1, mathUnary sin)
  , ("cos",   1, mathUnary cos)
  , ("tan",   1, mathUnary tan)
  , ("asin",  1, mathUnary asin)
  , ("acos",  1, mathUnary acos)
  , ("atan",  1, mathUnary atan)
  , ("sinh",  1, mathUnary sinh)
  , ("cosh",  1, mathUnary cosh)
  , ("tanh",  1, mathUnary tanh)
  , ("asinh", 1, mathUnary asinh)
  , ("acosh", 1, mathUnary acosh)
  , ("atanh", 1, mathUnary atanh)
  , ("exp",   1, mathUnary exp)
  , ("log",   1, mathUnary log)
  , ("sqrt",  1, mathUnary sqrt)
  , ("pi",    0, mathNone pi)
  ]

-- | The list of available built-in string functions.
stringIntrinsics :: [(Text, Int, CESKIntrinsicFunc)]
stringIntrinsics =
  [ ("string-length", 1, strLen)
  , ("string-char",   2, strChar)
  , ("string-upper",  1, strUpper)
  , ("string-lower",  1, strLower)
  , ("string-list",   1, strList)
  , ("string-make",   1, strMake)
  , ("string-append", 2, strAppend)
  , ("string-part",   3, strPart)
  ]

-- | The list of available built-in conversion functions.
convIntrinsics :: [(Text, Int, CESKIntrinsicFunc)]
convIntrinsics =
  [ ("read-int",   1, readInt)
  , ("read-float", 1, readFloat)
  , ("show-int",   1, showInt)
  , ("show-float", 1, showFloat)
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

-- | Converts a string to an int.
readInt :: [CESKVal] -> CESK CESKVal
readInt = \case
  (CESKValStr x):[] ->
    case readMaybe (T.unpack x) of
      Just y -> pure $ CESKValInt y
      Nothing -> throwError $ CESKErrorConversion x "int"
  _ -> throwError CESKErrorIntrinsicArgs

-- | Converts a string to an float.
readFloat :: [CESKVal] -> CESK CESKVal
readFloat = \case
  (CESKValStr x):[] ->
    case readMaybe (T.unpack x) of
      Just y -> pure $ CESKValFloat y
      Nothing -> throwError $ CESKErrorConversion x "float"
  _ -> throwError CESKErrorIntrinsicArgs

-- | Converts an int value to a string.
showInt :: [CESKVal] -> CESK CESKVal
showInt = \case
  (CESKValInt x):[] -> pure $ CESKValStr $ textShow x
  _ -> throwError CESKErrorIntrinsicArgs

-- | Converts a float value to a string.
showFloat :: [CESKVal] -> CESK CESKVal
showFloat = \case
  (CESKValFloat x):[] -> pure $ CESKValStr $ textShow x
  _ -> throwError CESKErrorIntrinsicArgs
