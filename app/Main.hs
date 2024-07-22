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
import Options.Applicative

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

data Args = Args
  { argBindings   :: Bool
  , argSourceFile :: Text
  }

readArgs :: IO Args
readArgs = execParser $ info (parseArgs <**> helper) fullDesc

parseArgs :: Parser Args
parseArgs = Args
  <$> parseBindings
  <*> parseSourceFile

parseBindings :: Parser Bool
parseBindings = switch $ long "bindings" <> short 'b'
  <> help "Create bindings for intrinsic functions"

parseSourceFile :: Parser Text
parseSourceFile = argument str (metavar "FILE")



-- schemeParse :: Text -> Either Text SchemeProg

-- normalizeProg :: SchemeProg -> IO SchemeProg

-- schemeANF :: SchemeProg -> Either Text ANFProg

-- ceskExec :: CESKOptions -> ANFProg -> IO (Either CESKError CESKVal)


-- schemeRun :: Text -> IO (Either Text Text)
-- schemeRun code = do
--   case schemeParse code of
--     Left e0 -> do
--       error $ T.unpack e0
--     Right prog -> do
--       prog' <- normalizeProg prog
--       case schemeANF prog' of
--         Left e2 ->
--           error $ T.unpack e2
--         Right p2 ->
--           ceskExec ceskDefaultOptions p2 >>= \case
--             Left e3 ->
--               pure $ Left $ ceskErrorHumanize e3
--             Right val ->
--               pure $ Right $ ceskValHumanize val

schemeRunFile :: Text -> IO ()
schemeRunFile path = do
  TIO.readFile (T.unpack path) >>= schemeRun >>= either
    (putStrLn . ("ERROR: "<>) . T.unpack)
    (putStrLn . T.unpack)

schemeRun :: Text -> IO (Either Text Text)
schemeRun code = runExceptT $ do
  prog <- ExceptT $ pure $ schemeParse code
  norm <- ExceptT $ fmap Right $ normalizeProg prog
  anf <- ExceptT $ pure $ schemeANF norm
  ExceptT $ mapLeft ceskErrorHumanize . mapRight ceskValHumanize <$>
    ceskExec ceskDefaultOptions anf
