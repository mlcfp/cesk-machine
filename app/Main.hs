--------------------------------------------------------------------------------
-- Copyright 2024 Michael P Williams. All rights reserved.
--------------------------------------------------------------------------------

module Main
  ( main
  ) where

import Control.Monad.Except (ExceptT(..), runExceptT)
import Data.Either (either)
import Data.Either.Extra (mapLeft, mapRight)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
-- import Data.Time.Clock (getCurrentTime)
import Text.RawString.QQ
import Gen1.CESK
import Gen1.Normalize
import Gen1.Scheme
import Gen1.Sugar
import Options.Applicative

-- | Defines the application main.
main :: IO ()
main = do
  Args{..} <- readArgs
  -- TODO add option to time execution
  -- t0 <- getCurrentTime
  -- putStrLn $ show t0
  schemeRunFile argSourceFile

normalizePretty :: Text -> IO Text
normalizePretty p = do
  -- let x = schemeParse p
  -- putStrLn $ show x
  let Right ast = schemeParse p
  normalizeProg ast >>= schemeRender schemeRenderOptions
    { schemeRenderOptionStyle = SchemeRenderNormal }

-- | Defines the command line arguments.
data Args = Args
  { argBindings   :: Bool
  , argSourceFile :: Text
  }

-- | Reads the command line arguments.
readArgs :: IO Args
readArgs = execParser $ info (parseArgs <**> helper) fullDesc

-- | Parses command line arguments.
parseArgs :: Parser Args
parseArgs = Args
  <$> parseBindings
  <*> parseSourceFile

-- | Parses the bindings flag.
parseBindings :: Parser Bool
parseBindings = switch $ long "bindings" <> short 'b'
  <> help "Create bindings for intrinsic functions"

-- | Parses the source file argument.
parseSourceFile :: Parser Text
parseSourceFile = argument str (metavar "FILE")

-- | Runs a scheme file.
schemeRunFile :: Text -> IO ()
schemeRunFile path = do
  TIO.readFile (T.unpack path) >>= schemeRun >>= either
    (putStrLn . ("ERROR: "<>) . T.unpack)
    (putStrLn . T.unpack)

-- | Runs scheme source code and get humanized results.
schemeRun :: Text -> IO (Either Text Text)
schemeRun code = runExceptT $ do
  prog <- ExceptT $ pure $ schemeParse code
  sugar <- ExceptT $ pure $
    mapLeft desugarErrorHumanize $ schemeDesugar prog
  norm <- ExceptT $ fmap Right $ normalizeProg sugar
  anf <- ExceptT $ pure $ schemeANF norm
  ExceptT $ mapLeft ceskErrorHumanize . mapRight ceskValHumanize <$>
    ceskExec ceskDefaultOptions anf
